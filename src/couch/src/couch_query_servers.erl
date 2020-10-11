% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_query_servers).

-export([try_compile/4]).
-export([start_doc_map/3, map_doc_raw/2, stop_doc_map/1, raw_to_ejson/1]).
-export([reduce/3, rereduce/3,validate_doc_update/5]).
-export([filter_docs/5]).
-export([filter_view/3]).
-export([finalize/2]).
-export([rewrite/3]).

-export([with_ddoc_proc/2, proc_prompt/2, ddoc_prompt/3, ddoc_proc_prompt/3, json_doc/1]).

% For 210-os-proc-pool.t
-export([get_os_process/1, get_ddoc_process/2, ret_os_process/1]).

-include_lib("couch/include/couch_db.hrl").

-define(SUMERROR, <<"The _sum function requires that map values be numbers, "
    "arrays of numbers, or objects. Objects cannot be mixed with other "
    "data structures. Objects can be arbitrarily nested, provided that the values "
    "for all fields are themselves numbers, arrays of numbers, or objects.">>).

-define(STATERROR, <<"The _stats function requires that map values be numbers "
    "or arrays of numbers, not '~p'">>).


try_compile(Proc, FunctionType, FunctionName, FunctionSource) ->
    try
        proc_prompt(Proc, [<<"add_fun">>, FunctionSource]),
        ok
    catch
        {compilation_error, E} ->
            Fmt = "Compilation of the ~s function in the '~s' view failed: ~s",
            Msg = io_lib:format(Fmt, [FunctionType, FunctionName, E]),
            throw({compilation_error, Msg});
        {os_process_error, {exit_status, ExitStatus}} ->
            Fmt = "Compilation of the ~s function in the '~s' view failed with exit status: ~p",
            Msg = io_lib:format(Fmt, [FunctionType, FunctionName, ExitStatus]),
            throw({compilation_error, Msg})
    end.

start_doc_map(Lang, Functions, Lib) ->
    Proc = get_os_process(Lang),
    case Lib of
    {[]} -> ok;
    Lib ->
        true = proc_prompt(Proc, [<<"add_lib">>, Lib])
    end,
    lists:foreach(fun(FunctionSource) ->
        true = proc_prompt(Proc, [<<"add_fun">>, FunctionSource])
    end, Functions),
    {ok, Proc}.

map_doc_raw(Proc, Doc) ->
    Json = couch_doc:to_json_obj(Doc, []),
    {ok, proc_prompt_raw(Proc, [<<"map_doc">>, Json])}.


stop_doc_map(nil) ->
    ok;
stop_doc_map(Proc) ->
    ok = ret_os_process(Proc).

group_reductions_results([]) ->
    [];
group_reductions_results(List) ->
    {Heads, Tails} = lists:foldl(
        fun([H|T], {HAcc,TAcc}) ->
            {[H|HAcc], [T|TAcc]}
        end, {[], []}, List),
    case Tails of
    [[]|_] -> % no tails left
        [Heads];
    _ ->
     [Heads | group_reductions_results(Tails)]
    end.

finalize(<<"_approx_count_distinct",_/binary>>, Reduction) ->
    true = hyper:is_hyper(Reduction),
    {ok, round(hyper:card(Reduction))};
finalize(<<"_stats",_/binary>>, Unpacked) ->
    {ok, pack_stats(Unpacked)};
finalize(_RedSrc, Reduction) ->
    {ok, Reduction}.

rereduce(_Lang, [], _ReducedValues) ->
    {ok, []};
rereduce(Lang, RedSrcs, ReducedValues) ->
    Grouped = group_reductions_results(ReducedValues),
    Results = lists:zipwith(
        fun
        (<<"_", _/binary>> = FunSrc, Values) ->
            {ok, [Result]} = builtin_reduce(rereduce, [FunSrc], [[[], V] || V <- Values], []),
            Result;
        (FunSrc, Values) ->
            os_rereduce(Lang, [FunSrc], Values)
        end, RedSrcs, Grouped),
    {ok, Results}.

reduce(_Lang, [], _KVs) ->
    {ok, []};
reduce(Lang, RedSrcs, KVs) ->
    {OsRedSrcs, BuiltinReds} = lists:partition(fun
        (<<"_", _/binary>>) -> false;
        (_OsFun) -> true
    end, RedSrcs),
    {ok, OsResults} = os_reduce(Lang, OsRedSrcs, KVs),
    {ok, BuiltinResults} = builtin_reduce(reduce, BuiltinReds, KVs, []),
    recombine_reduce_results(RedSrcs, OsResults, BuiltinResults, []).


recombine_reduce_results([], [], [], Acc) ->
    {ok, lists:reverse(Acc)};
recombine_reduce_results([<<"_", _/binary>>|RedSrcs], OsResults, [BRes|BuiltinResults], Acc) ->
    recombine_reduce_results(RedSrcs, OsResults, BuiltinResults, [BRes|Acc]);
recombine_reduce_results([_OsFun|RedSrcs], [OsR|OsResults], BuiltinResults, Acc) ->
    recombine_reduce_results(RedSrcs, OsResults, BuiltinResults, [OsR|Acc]).

os_reduce(_Lang, [], _KVs) ->
    {ok, []};
os_reduce(Lang, OsRedSrcs, KVs) ->
    Proc = get_os_process(Lang),
    OsResults = try proc_prompt(Proc, [<<"reduce">>, OsRedSrcs, KVs]) of
        [true, Reductions] -> Reductions
    catch
        throw:{reduce_overflow_error, Msg} ->
            [{[{reduce_overflow_error, Msg}]} || _ <- OsRedSrcs]
    after
        ok = ret_os_process(Proc)
    end,
    {ok, OsResults}.

os_rereduce(Lang, OsRedSrcs, KVs) ->
    case get_overflow_error(KVs) of
        undefined ->
            Proc = get_os_process(Lang),
            try proc_prompt(Proc, [<<"rereduce">>, OsRedSrcs, KVs]) of
                [true, [Reduction]] -> Reduction
            catch
                throw:{reduce_overflow_error, Msg} ->
                    {[{reduce_overflow_error, Msg}]}
            after
                ok = ret_os_process(Proc)
            end;
        Error ->
            Error
    end.


get_overflow_error([]) ->
    undefined;
get_overflow_error([{[{reduce_overflow_error, _}]} = Error | _]) ->
    Error;
get_overflow_error([_ | Rest]) ->
    get_overflow_error(Rest).


builtin_reduce(_Re, [], _KVs, Acc) ->
    {ok, lists:reverse(Acc)};
builtin_reduce(Re, [<<"_sum",_/binary>>|BuiltinReds], KVs, Acc) ->
    Sum = builtin_sum_rows(KVs, 0),
    Red = check_sum_overflow(?term_size(KVs), ?term_size(Sum), Sum),
    builtin_reduce(Re, BuiltinReds, KVs, [Red|Acc]);
builtin_reduce(reduce, [<<"_count",_/binary>>|BuiltinReds], KVs, Acc) ->
    Count = length(KVs),
    builtin_reduce(reduce, BuiltinReds, KVs, [Count|Acc]);
builtin_reduce(rereduce, [<<"_count",_/binary>>|BuiltinReds], KVs, Acc) ->
    Count = builtin_sum_rows(KVs, 0),
    builtin_reduce(rereduce, BuiltinReds, KVs, [Count|Acc]);
builtin_reduce(Re, [<<"_stats",_/binary>>|BuiltinReds], KVs, Acc) ->
    Stats = builtin_stats(Re, KVs),
    builtin_reduce(Re, BuiltinReds, KVs, [Stats|Acc]);
builtin_reduce(Re, [<<"_approx_count_distinct",_/binary>>|BuiltinReds], KVs, Acc) ->
    Distinct = approx_count_distinct(Re, KVs),
    builtin_reduce(Re, BuiltinReds, KVs, [Distinct|Acc]).


builtin_sum_rows([], Acc) ->
    Acc;
builtin_sum_rows([[_Key, Value] | RestKVs], Acc) ->
    try sum_values(Value, Acc) of
        NewAcc ->
            builtin_sum_rows(RestKVs, NewAcc)
    catch
        throw:{builtin_reduce_error, Obj} ->
            Obj;
        throw:{invalid_value, Reason, Cause} ->
            {[{<<"error">>, <<"builtin_reduce_error">>},
                {<<"reason">>, Reason}, {<<"caused_by">>, Cause}]}
    end.


sum_values(Value, Acc) when is_number(Value), is_number(Acc) ->
    Acc + Value;
sum_values(Value, Acc) when is_list(Value), is_list(Acc) ->
    sum_arrays(Acc, Value);
sum_values(Value, Acc) when is_number(Value), is_list(Acc) ->
    sum_arrays(Acc, [Value]);
sum_values(Value, Acc) when is_list(Value), is_number(Acc) ->
    sum_arrays([Acc], Value);
sum_values({Props}, Acc) ->
    case lists:keyfind(<<"error">>, 1, Props) of
        {<<"error">>, <<"builtin_reduce_error">>} ->
            throw({builtin_reduce_error, {Props}});
        false ->
            ok
    end,
    case Acc of
        0 ->
            {Props};
        {AccProps} ->
            {sum_objects(lists:sort(Props), lists:sort(AccProps))}
    end;
sum_values(Else, _Acc) ->
    throw_sum_error(Else).

sum_objects([{K1, V1} | Rest1], [{K1, V2} | Rest2]) ->
    [{K1, sum_values(V1, V2)} | sum_objects(Rest1, Rest2)];
sum_objects([{K1, V1} | Rest1], [{K2, V2} | Rest2]) when K1 < K2 ->
    [{K1, V1} | sum_objects(Rest1, [{K2, V2} | Rest2])];
sum_objects([{K1, V1} | Rest1], [{K2, V2} | Rest2]) when K1 > K2 ->
    [{K2, V2} | sum_objects([{K1, V1} | Rest1], Rest2)];
sum_objects([], Rest) ->
    Rest;
sum_objects(Rest, []) ->
    Rest.

sum_arrays([], []) ->
    [];
sum_arrays([_|_]=Xs, []) ->
    Xs;
sum_arrays([], [_|_]=Ys) ->
    Ys;
sum_arrays([X|Xs], [Y|Ys]) when is_number(X), is_number(Y) ->
    [X+Y | sum_arrays(Xs,Ys)];
sum_arrays(Else, _) ->
    throw_sum_error(Else).

check_sum_overflow(InSize, OutSize, Sum) ->
    Overflowed = OutSize > 4906 andalso OutSize * 2 > InSize,
    case config:get("query_server_config", "reduce_limit", "true") of
        "true" when Overflowed ->
            Msg = log_sum_overflow(InSize, OutSize),
            {[
                {<<"error">>, <<"builtin_reduce_error">>},
                {<<"reason">>, Msg}
            ]};
        "log" when Overflowed ->
            log_sum_overflow(InSize, OutSize),
            Sum;
        _ ->
            Sum
    end.

log_sum_overflow(InSize, OutSize) ->
    Fmt = "Reduce output must shrink more rapidly: "
            "input size: ~b "
            "output size: ~b",
    Msg = iolist_to_binary(io_lib:format(Fmt, [InSize, OutSize])),
    couch_log:error(Msg, []),
    Msg.

builtin_stats(_, []) ->
    {0, 0, 0, 0, 0};
builtin_stats(_, [[_,First]|Rest]) ->
    lists:foldl(fun([_Key, Value], Acc) ->
        stat_values(Value, Acc)
    end, build_initial_accumulator(First), Rest).

stat_values(Value, Acc) when is_list(Value), is_list(Acc) ->
    lists:zipwith(fun stat_values/2, Value, Acc);
stat_values({PreRed}, Acc) when is_list(PreRed) ->
    stat_values(unpack_stats({PreRed}), Acc);
stat_values(Value, Acc) when is_number(Value) ->
    stat_values({Value, 1, Value, Value, Value*Value}, Acc);
stat_values(Value, Acc) when is_number(Acc) ->
    stat_values(Value, {Acc, 1, Acc, Acc, Acc*Acc});
stat_values(Value, Acc) when is_tuple(Value), is_tuple(Acc) ->
    {Sum0, Cnt0, Min0, Max0, Sqr0} = Value,
    {Sum1, Cnt1, Min1, Max1, Sqr1} = Acc,
    {
      Sum0 + Sum1,
      Cnt0 + Cnt1,
      erlang:min(Min0, Min1),
      erlang:max(Max0, Max1),
      Sqr0 + Sqr1
    };
stat_values(Else, _Acc) ->
    throw_stat_error(Else).

build_initial_accumulator(L) when is_list(L) ->
    [build_initial_accumulator(X) || X <- L];
build_initial_accumulator(X) when is_number(X) ->
    {X, 1, X, X, X*X};
build_initial_accumulator({_, _, _, _, _} = AlreadyUnpacked) ->
    AlreadyUnpacked;
build_initial_accumulator({Props}) ->
    unpack_stats({Props});
build_initial_accumulator(Else) ->
    Msg = io_lib:format("non-numeric _stats input: ~w", [Else]),
    throw({invalid_value, iolist_to_binary(Msg)}).

unpack_stats({PreRed}) when is_list(PreRed) ->
    {
      get_number(<<"sum">>, PreRed),
      get_number(<<"count">>, PreRed),
      get_number(<<"min">>, PreRed),
      get_number(<<"max">>, PreRed),
      get_number(<<"sumsqr">>, PreRed)
    }.


pack_stats({Sum, Cnt, Min, Max, Sqr}) ->
    {[{<<"sum">>,Sum}, {<<"count">>,Cnt}, {<<"min">>,Min}, {<<"max">>,Max}, {<<"sumsqr">>,Sqr}]};
pack_stats({Packed}) ->
    % Legacy code path before we had the finalize operation
    {Packed};
pack_stats(Stats) when is_list(Stats) ->
    lists:map(fun pack_stats/1, Stats).

get_number(Key, Props) ->
    case couch_util:get_value(Key, Props) of
    X when is_number(X) ->
        X;
    undefined when is_binary(Key) ->
        get_number(binary_to_atom(Key, latin1), Props);
    undefined ->
        Msg = io_lib:format("user _stats input missing required field ~s (~p)",
            [Key, Props]),
        throw({invalid_value, iolist_to_binary(Msg)});
    Else ->
        Msg = io_lib:format("non-numeric _stats input received for ~s: ~w",
            [Key, Else]),
        throw({invalid_value, iolist_to_binary(Msg)})
    end.

% TODO allow customization of precision in the ddoc.
approx_count_distinct(reduce, KVs) ->
    lists:foldl(fun([[Key, _Id], _Value], Filter) ->
        hyper:insert(term_to_binary(Key), Filter)
    end, hyper:new(11), KVs);
approx_count_distinct(rereduce, Reds) ->
    hyper:union([Filter || [_, Filter] <- Reds]).

% use the function stored in ddoc.validate_doc_update to test an update.
-spec validate_doc_update(DDoc, EditDoc, DiskDoc, Ctx, SecObj) -> ok when
    DDoc    :: ddoc(),
    EditDoc :: doc(),
    DiskDoc :: doc() | nil,
    Ctx     :: user_ctx(),
    SecObj  :: sec_obj().

validate_doc_update(DDoc, EditDoc, DiskDoc, Ctx, SecObj) ->
    JsonEditDoc = couch_doc:to_json_obj(EditDoc, [revs]),
    JsonDiskDoc = json_doc(DiskDoc),
    Resp = ddoc_prompt(
        DDoc,
        [<<"validate_doc_update">>],
        [JsonEditDoc, JsonDiskDoc, Ctx, SecObj]
    ),
    if Resp == 1 -> ok; true ->
        couch_stats:increment_counter([couchdb, query_server, vdu_rejects], 1)
    end,
    case Resp of
        RespCode when RespCode =:= 1; RespCode =:= ok; RespCode =:= true ->
            ok;
        {[{<<"forbidden">>, Message}]} ->
            throw({forbidden, Message});
        {[{<<"unauthorized">>, Message}]} ->
            throw({unauthorized, Message});
        {[{_, Message}]} ->
            throw({unknown_error, Message});
        Message when is_binary(Message) ->
            throw({unknown_error, Message})
    end.


rewrite(Req, Db, DDoc) ->
    Fields = [F || F <- chttpd_external:json_req_obj_fields(),
              F =/= <<"info">>, F =/= <<"form">>,
              F =/= <<"uuid">>, F =/= <<"id">>],
    JsonReq = chttpd_external:json_req_obj(Req, Db, null, Fields),
    case couch_query_servers:ddoc_prompt(DDoc, [<<"rewrites">>], [JsonReq]) of
        {[{<<"forbidden">>, Message}]} ->
            throw({forbidden, Message});
        {[{<<"unauthorized">>, Message}]} ->
            throw({unauthorized, Message});
        [<<"no_dispatch_rule">>] ->
            undefined;
        [<<"ok">>, {V}=Rewrite] when is_list(V) ->
            ok = validate_rewrite_response(Rewrite),
            Rewrite;
        [<<"ok">>, _]  ->
            throw_rewrite_error(<<"bad rewrite">>);
        V ->
            couch_log:error("bad rewrite return ~p", [V]),
            throw({unknown_error, V})
    end.

validate_rewrite_response({Fields}) when is_list(Fields) ->
    validate_rewrite_response_fields(Fields).

validate_rewrite_response_fields([{Key, Value} | Rest]) ->
    validate_rewrite_response_field(Key, Value),
    validate_rewrite_response_fields(Rest);
validate_rewrite_response_fields([]) ->
    ok.

validate_rewrite_response_field(<<"method">>, Method) when is_binary(Method) ->
    ok;
validate_rewrite_response_field(<<"method">>, _) ->
    throw_rewrite_error(<<"bad method">>);
validate_rewrite_response_field(<<"path">>, Path) when is_binary(Path) ->
    ok;
validate_rewrite_response_field(<<"path">>, _) ->
    throw_rewrite_error(<<"bad path">>);
validate_rewrite_response_field(<<"body">>, Body) when is_binary(Body) ->
    ok;
validate_rewrite_response_field(<<"body">>, _) ->
    throw_rewrite_error(<<"bad body">>);
validate_rewrite_response_field(<<"headers">>, {Props}=Headers) when is_list(Props) ->
    validate_object_fields(Headers);
validate_rewrite_response_field(<<"headers">>, _) ->
    throw_rewrite_error(<<"bad headers">>);
validate_rewrite_response_field(<<"query">>, {Props}=Query) when is_list(Props) ->
    validate_object_fields(Query);
validate_rewrite_response_field(<<"query">>, _) ->
    throw_rewrite_error(<<"bad query">>);
validate_rewrite_response_field(<<"code">>, Code) when is_integer(Code) andalso Code >= 200 andalso Code < 600 ->
    ok;
validate_rewrite_response_field(<<"code">>, _) ->
    throw_rewrite_error(<<"bad code">>);
validate_rewrite_response_field(K, V) ->
    couch_log:debug("unknown rewrite field ~p=~p", [K, V]),
    ok.

validate_object_fields({Props}) when is_list(Props) ->
    lists:foreach(fun
        ({Key, Value}) when is_binary(Key) andalso is_binary(Value) ->
            ok;
        ({Key, Value}) ->
            Reason = io_lib:format(
                "object key/value must be strings ~p=~p", [Key, Value]),
            throw_rewrite_error(Reason);
        (Value) ->
            throw_rewrite_error(io_lib:format("bad value ~p", [Value]))
    end, Props).


throw_rewrite_error(Reason) when is_list(Reason)->
    throw_rewrite_error(iolist_to_binary(Reason));
throw_rewrite_error(Reason) when is_binary(Reason) ->
    throw({rewrite_error, Reason}).


json_doc_options() ->
    json_doc_options([]).

json_doc_options(Options) ->
    Limit = config:get_integer("query_server_config", "revs_limit", 20),
    [{revs, Limit} | Options].

json_doc(Doc) ->
    json_doc(Doc, json_doc_options()).

json_doc(nil, _) ->
    null;
json_doc(Doc, Options) ->
    couch_doc:to_json_obj(Doc, Options).

filter_view(DDoc, VName, Docs) ->
    Options = json_doc_options(),
    JsonDocs = [json_doc(Doc, Options) || Doc <- Docs],
    [true, Passes] = ddoc_prompt(DDoc, [<<"views">>, VName, <<"map">>], [JsonDocs]),
    {ok, Passes}.

filter_docs(Req, Db, DDoc, FName, Docs) ->
    JsonReq = case Req of
        {json_req, JsonObj} ->
            JsonObj;
        #httpd{} = HttpReq ->
            couch_httpd_external:json_req_obj(HttpReq, Db)
    end,
    Options = json_doc_options(),
    JsonDocs = [json_doc(Doc, Options) || Doc <- Docs],
    [true, Passes] = ddoc_prompt(DDoc, [<<"filters">>, FName],
        [JsonDocs, JsonReq]),
    {ok, Passes}.

ddoc_proc_prompt({Proc, DDocId}, FunPath, Args) ->
    proc_prompt(Proc, [<<"ddoc">>, DDocId, FunPath, Args]).

ddoc_prompt(DDoc, FunPath, Args) ->
    with_ddoc_proc(DDoc, fun({Proc, DDocId}) ->
        proc_prompt(Proc, [<<"ddoc">>, DDocId, FunPath, Args])
    end).

with_ddoc_proc(#doc{id=DDocId,revs={Start, [DiskRev|_]}}=DDoc, Fun) ->
    Rev = couch_doc:rev_to_str({Start, DiskRev}),
    DDocKey = {DDocId, Rev},
    Proc = get_ddoc_process(DDoc, DDocKey),
    try Fun({Proc, DDocId})
    after
        ok = ret_os_process(Proc)
    end.

proc_prompt(Proc, Args) ->
     case proc_prompt_raw(Proc, Args) of
     {json, Json} ->
         raw_to_ejson({json, Json});
     EJson ->
         EJson
     end.

proc_prompt_raw(#proc{prompt_fun = {Mod, Func}} = Proc, Args) ->
    apply(Mod, Func, [Proc#proc.pid, Args]).

raw_to_ejson({json, Json}) ->
    try
        ?JSON_DECODE(Json)
    catch throw:{invalid_json, {_, invalid_string}} ->
        Forced = try
            force_utf8(Json)
        catch _:_ ->
            Json
        end,
        ?JSON_DECODE(Forced)
    end;
raw_to_ejson(EJson) ->
    EJson.

force_utf8(Bin) ->
    case binary:match(Bin, <<"\\u">>) of
        {Start, 2} ->
            <<Prefix:Start/binary, Rest1/binary>> = Bin,
            {Insert, Rest3} = case check_uescape(Rest1) of
                {ok, Skip} ->
                    <<Skipped:Skip/binary, Rest2/binary>> = Rest1,
                    {Skipped, Rest2};
                {error, Skip} ->
                    <<_:Skip/binary, Rest2/binary>> = Rest1,
                    {<<16#EF, 16#BF, 16#BD>>, Rest2}
            end,
            RestForced = force_utf8(Rest3),
            <<Prefix/binary, Insert/binary, RestForced/binary>>;
        nomatch ->
            Bin
    end.

check_uescape(Data) ->
    case extract_uescape(Data) of
        {Hi, Rest} when Hi >= 16#D800, Hi < 16#DC00 ->
            case extract_uescape(Rest) of
                {Lo, _} when Lo >= 16#DC00, Lo =< 16#DFFF ->
                    % A low surrogate pair
                    UTF16 = <<
                        Hi:16/big-unsigned-integer,
                        Lo:16/big-unsigned-integer
                    >>,
                    try
                        [_] = xmerl_ucs:from_utf16be(UTF16),
                        {ok, 12}
                    catch _:_ ->
                        {error, 6}
                    end;
                {_, _} ->
                    % Found a uescape that's not a low half
                    {error, 6};
                false ->
                    % No hex escape found
                    {error, 6}
            end;
        {Hi, _} when Hi >= 16#DC00, Hi =< 16#DFFF ->
            % Found a low surrogate half without a high half
            {error, 6};
        {_, _} ->
            % Found a uescape we don't care about
            {ok, 6};
        false ->
            % Incomplete uescape which we don't care about
            {ok, 2}
    end.

extract_uescape(<<"\\u", Code:4/binary, Rest/binary>>) ->
    {binary_to_integer(Code, 16), Rest};
extract_uescape(_) ->
    false.

proc_stop(Proc) ->
    {Mod, Func} = Proc#proc.stop_fun,
    apply(Mod, Func, [Proc#proc.pid]).

proc_set_timeout(Proc, Timeout) ->
    {Mod, Func} = Proc#proc.set_timeout_fun,
    apply(Mod, Func, [Proc#proc.pid, Timeout]).

get_os_process_timeout() ->
    list_to_integer(config:get("couchdb", "os_process_timeout", "5000")).

get_ddoc_process(#doc{} = DDoc, DDocKey) ->
    % remove this case statement
    case gen_server:call(couch_proc_manager, {get_proc, DDoc, DDocKey}, get_os_process_timeout()) of
    {ok, Proc, {QueryConfig}} ->
        % process knows the ddoc
        case (catch proc_prompt(Proc, [<<"reset">>, {QueryConfig}])) of
        true ->
            proc_set_timeout(Proc, couch_util:get_value(<<"timeout">>, QueryConfig)),
            Proc;
        _ ->
            catch proc_stop(Proc),
            get_ddoc_process(DDoc, DDocKey)
        end;
    Error ->
        throw(Error)
    end.

get_os_process(Lang) ->
    case gen_server:call(couch_proc_manager, {get_proc, Lang}, get_os_process_timeout()) of
    {ok, Proc, {QueryConfig}} ->
        case (catch proc_prompt(Proc, [<<"reset">>, {QueryConfig}])) of
        true ->
            proc_set_timeout(Proc, couch_util:get_value(<<"timeout">>, QueryConfig)),
            Proc;
        _ ->
            catch proc_stop(Proc),
            get_os_process(Lang)
        end;
    Error ->
        throw(Error)
    end.

ret_os_process(Proc) ->
    true = gen_server:call(couch_proc_manager, {ret_proc, Proc}, infinity),
    catch unlink(Proc#proc.pid),
    ok.

throw_sum_error(Else) ->
    throw({invalid_value, ?SUMERROR, Else}).

throw_stat_error(Else) ->
    throw({invalid_value, iolist_to_binary(io_lib:format(?STATERROR, [Else]))}).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

builtin_sum_rows_negative_test() ->
    A = [{[{<<"a">>, 1}]}, {[{<<"a">>, 2}]}, {[{<<"a">>, 3}]}],
    E = {[{<<"error">>, <<"builtin_reduce_error">>}]},
    ?assertEqual(E, builtin_sum_rows([["K", E]], [])),
    % The below case is where the value is invalid, but no error because
    % it's only one document.
    ?assertEqual(A, builtin_sum_rows([["K", A]], [])),
    {Result} = builtin_sum_rows([["K", A]], [1, 2, 3]),
    ?assertEqual({<<"error">>, <<"builtin_reduce_error">>},
        lists:keyfind(<<"error">>, 1, Result)).

sum_values_test() ->
    ?assertEqual(3, sum_values(1, 2)),
    ?assertEqual([2,4,6], sum_values(1, [1,4,6])),
    ?assertEqual([3,5,7], sum_values([3,2,4], [0,3,3])),
    X = {[{<<"a">>,1}, {<<"b">>,[1,2]}, {<<"c">>, {[{<<"d">>,3}]}},
            {<<"g">>,1}]},
    Y = {[{<<"a">>,2}, {<<"b">>,3}, {<<"c">>, {[{<<"e">>, 5}]}},
            {<<"f">>,1}, {<<"g">>,1}]},
    Z = {[{<<"a">>,3}, {<<"b">>,[4,2]}, {<<"c">>, {[{<<"d">>,3},{<<"e">>,5}]}},
            {<<"f">>,1}, {<<"g">>,2}]},
    ?assertEqual(Z, sum_values(X, Y)),
    ?assertEqual(Z, sum_values(Y, X)).

sum_values_negative_test() ->
    % invalid value
    A = [{[{<<"a">>, 1}]}, {[{<<"a">>, 2}]}, {[{<<"a">>, 3}]}],
    B = ["error 1", "error 2"],
    C = [<<"error 3">>, <<"error 4">>],
    KV = {[{<<"error">>, <<"builtin_reduce_error">>},
        {<<"reason">>, ?SUMERROR}, {<<"caused_by">>, <<"some cause">>}]},
    ?assertThrow({invalid_value, _, _}, sum_values(A, [1, 2, 3])),
    ?assertThrow({invalid_value, _, _}, sum_values(A, 0)),
    ?assertThrow({invalid_value, _, _}, sum_values(B, [1, 2])),
    ?assertThrow({invalid_value, _, _}, sum_values(C, [0])),
    ?assertThrow({builtin_reduce_error, KV}, sum_values(KV, [0])).

stat_values_test() ->
    ?assertEqual({1, 2, 0, 1, 1}, stat_values(1, 0)),
    ?assertEqual({11, 2, 1, 10, 101}, stat_values(1, 10)),
    ?assertEqual([{9, 2, 2, 7, 53},
                  {14, 2, 3, 11, 130},
                  {18, 2, 5, 13, 194}
                 ], stat_values([2,3,5], [7,11,13])).

reduce_stats_test() ->
    ?assertEqual([
        {[{<<"sum">>,2},{<<"count">>,1},{<<"min">>,2},{<<"max">>,2},{<<"sumsqr">>,4}]}
    ], test_reduce(<<"_stats">>, [[[null, key], 2]])),

    ?assertEqual([[
        {[{<<"sum">>,1},{<<"count">>,1},{<<"min">>,1},{<<"max">>,1},{<<"sumsqr">>,1}]},
        {[{<<"sum">>,2},{<<"count">>,1},{<<"min">>,2},{<<"max">>,2},{<<"sumsqr">>,4}]}
    ]], test_reduce(<<"_stats">>, [[[null, key],[1,2]]])),

    ?assertEqual(
         {[{<<"sum">>,2},{<<"count">>,1},{<<"min">>,2},{<<"max">>,2},{<<"sumsqr">>,4}]}
    , element(2, finalize(<<"_stats">>, {2, 1, 2, 2, 4}))),

    ?assertEqual([
        {[{<<"sum">>,1},{<<"count">>,1},{<<"min">>,1},{<<"max">>,1},{<<"sumsqr">>,1}]},
        {[{<<"sum">>,2},{<<"count">>,1},{<<"min">>,2},{<<"max">>,2},{<<"sumsqr">>,4}]}
    ], element(2, finalize(<<"_stats">>, [
        {1, 1, 1, 1, 1},
        {2, 1, 2, 2, 4}
    ]))),

    ?assertEqual([
        {[{<<"sum">>,1},{<<"count">>,1},{<<"min">>,1},{<<"max">>,1},{<<"sumsqr">>,1}]},
        {[{<<"sum">>,2},{<<"count">>,1},{<<"min">>,2},{<<"max">>,2},{<<"sumsqr">>,4}]}
    ], element(2, finalize(<<"_stats">>, [
        {1, 1, 1, 1, 1},
        {[{<<"sum">>,2},{<<"count">>,1},{<<"min">>,2},{<<"max">>,2},{<<"sumsqr">>,4}]}
    ]))),

    ?assertEqual([
        {[{<<"sum">>,1},{<<"count">>,1},{<<"min">>,1},{<<"max">>,1},{<<"sumsqr">>,1}]},
        {[{<<"sum">>,2},{<<"count">>,1},{<<"min">>,2},{<<"max">>,2},{<<"sumsqr">>,4}]}
    ], element(2, finalize(<<"_stats">>, [
        {[{<<"sum">>,1},{<<"count">>,1},{<<"min">>,1},{<<"max">>,1},{<<"sumsqr">>,1}]},
        {2, 1, 2, 2, 4}
    ]))),
    ok.

test_reduce(Reducer, KVs) ->
    ?assertMatch({ok, _}, reduce(<<"javascript">>, [Reducer], KVs)),
    {ok, Reduced} = reduce(<<"javascript">>, [Reducer], KVs),
    {ok, Finalized} = finalize(Reducer, Reduced),
    Finalized.

force_utf8_test() ->
    % "\uDCA5\uD83D"
    Ok = [
        <<"foo">>,
        <<"\\u00A0">>,
        <<"\\u0032">>,
        <<"\\uD83D\\uDCA5">>,
        <<"foo\\uD83D\\uDCA5bar">>,
        % Truncated but we doesn't break replacements
        <<"\\u0FA">>
    ],
    lists:foreach(fun(Case) ->
        ?assertEqual(Case, force_utf8(Case))
    end, Ok),

    NotOk = [
        <<"\\uDCA5">>,
        <<"\\uD83D">>,
        <<"fo\\uDCA5bar">>,
        <<"foo\\uD83Dbar">>,
        <<"\\uDCA5\\uD83D">>,
        <<"\\uD83Df\\uDCA5">>,
        <<"\\uDCA5\\u00A0">>,
        <<"\\uD83D\\u00A0">>
    ],
    ToJSON = fun(Bin) -> <<34, Bin/binary, 34>> end,
    lists:foreach(fun(Case) ->
        try
            ?assertNotEqual(Case, force_utf8(Case)),
            ?assertThrow(_, ?JSON_DECODE(ToJSON(Case))),
            ?assertMatch(<<_/binary>>, ?JSON_DECODE(ToJSON(force_utf8(Case))))
        catch
          T:R ->
            io:format(standard_error, "~p~n~p~n", [T, R])
        end
    end, NotOk).

-endif.

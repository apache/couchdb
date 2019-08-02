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

-module(couch_replicator_utils).

-export([
   parse_rep_doc/2,
   replication_id/2,
   sum_stats/2,
   is_deleted/1,
   rep_error_to_binary/1,
   get_json_value/2,
   get_json_value/3,
   pp_rep_id/1,
   iso8601/1,
   filter_state/3,
   remove_basic_auth_from_headers/1,
   normalize_rep/1
]).


-include_lib("couch/include/couch_db.hrl").
-include("couch_replicator.hrl").
-include_lib("couch_replicator/include/couch_replicator_api_wrap.hrl").

-import(couch_util, [
    get_value/2,
    get_value/3
]).


rep_error_to_binary(Error) ->
    couch_util:to_binary(error_reason(Error)).


error_reason({shutdown, Error}) ->
    error_reason(Error);
error_reason({error, {Error, Reason}})
  when is_atom(Error), is_binary(Reason) ->
    io_lib:format("~s: ~s", [Error, Reason]);
error_reason({error, Reason}) ->
    Reason;
error_reason(Reason) ->
    Reason.


get_json_value(Key, Props) ->
    get_json_value(Key, Props, undefined).

get_json_value(Key, Props, Default) when is_atom(Key) ->
    Ref = make_ref(),
    case get_value(Key, Props, Ref) of
        Ref ->
            get_value(?l2b(atom_to_list(Key)), Props, Default);
        Else ->
            Else
    end;
get_json_value(Key, Props, Default) when is_binary(Key) ->
    Ref = make_ref(),
    case get_value(Key, Props, Ref) of
        Ref ->
            get_value(list_to_atom(?b2l(Key)), Props, Default);
        Else ->
            Else
    end.


% pretty-print replication id
-spec pp_rep_id(#rep{} | rep_id()) -> string().
pp_rep_id(#rep{id = RepId}) ->
    pp_rep_id(RepId);
pp_rep_id({Base, Extension}) ->
    Base ++ Extension.


% NV: TODO: this function is not used outside api wrap module
% consider moving it there during final cleanup
is_deleted(Change) ->
    get_json_value(<<"deleted">>, Change, false).


% NV: TODO: proxy some functions which used to be here, later remove
% these and replace calls to their respective modules
replication_id(Rep, Version) ->
    couch_replicator_ids:replication_id(Rep, Version).


sum_stats(S1, S2) ->
    couch_replicator_stats:sum_stats(S1, S2).


parse_rep_doc(Props, UserCtx) ->
    couch_replicator_docs:parse_rep_doc(Props, UserCtx).


-spec iso8601(erlang:timestamp()) -> binary().
iso8601({_Mega, _Sec, _Micro} = Timestamp) ->
    {{Y, Mon, D}, {H, Min, S}} = calendar:now_to_universal_time(Timestamp),
    Format = "~B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
    iolist_to_binary(io_lib:format(Format, [Y, Mon, D, H, Min, S])).


%% Filter replication info ejson by state provided. If it matches return
%% the input value, if it doesn't return 'skip'. This is used from replicator
%% fabric coordinator and worker.
-spec filter_state(atom(), [atom()], {[_ | _]}) -> {[_ | _]} | skip.
filter_state(null = _State, _States, _Info) ->
    skip;
filter_state(_ = _State, [] = _States, Info) ->
    Info;
filter_state(State, States, Info) ->
    case lists:member(State, States) of
        true ->
            Info;
        false ->
            skip
    end.


remove_basic_auth_from_headers(Headers) ->
    Headers1 = mochiweb_headers:make(Headers),
    case mochiweb_headers:get_value("Authorization", Headers1) of
        undefined ->
            {{undefined, undefined}, Headers};
        Auth ->
            {Basic, Base64} = lists:splitwith(fun(X) -> X =/= $\s end, Auth),
            maybe_remove_basic_auth(string:to_lower(Basic), Base64, Headers1)
    end.


maybe_remove_basic_auth("basic", " " ++ Base64, Headers) ->
    Headers1 = mochiweb_headers:delete_any("Authorization", Headers),
    {decode_basic_creds(Base64), mochiweb_headers:to_list(Headers1)};
maybe_remove_basic_auth(_, _, Headers) ->
    {{undefined, undefined}, mochiweb_headers:to_list(Headers)}.


decode_basic_creds(Base64) ->
    try re:split(base64:decode(Base64), ":", [{return, list}, {parts, 2}]) of
        [User, Pass] ->
            {User, Pass};
        _ ->
            {undefined, undefined}
    catch
        % Tolerate invalid B64 values here to avoid crashing replicator
        error:function_clause ->
            {undefined, undefined}
    end.


% Normalize a #rep{} record such that it doesn't contain time dependent fields
% pids (like httpc pools), and options / props are sorted. This function would
% used during comparisons.
-spec normalize_rep(#rep{} | nil) -> #rep{} | nil.
normalize_rep(nil) ->
    nil;

normalize_rep(#rep{} = Rep)->
    #rep{
        source = couch_replicator_api_wrap:normalize_db(Rep#rep.source),
        target = couch_replicator_api_wrap:normalize_db(Rep#rep.target),
        options = Rep#rep.options,  % already sorted in make_options/1
        type = Rep#rep.type,
        view = Rep#rep.view,
        doc_id = Rep#rep.doc_id,
        db_name = Rep#rep.db_name
    }.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

remove_basic_auth_from_headers_test_() ->
    [?_assertMatch({{User, Pass}, NoAuthHeaders},
        remove_basic_auth_from_headers(Headers)) ||
        {{User, Pass, NoAuthHeaders}, Headers} <- [
            {
                {undefined, undefined, []},
                []
            },
            {
                {undefined, undefined, [{"h", "v"}]},
                [{"h", "v"}]
            },
            {
                {undefined, undefined, [{"Authorization", "junk"}]},
                [{"Authorization", "junk"}]
            },
            {
                {undefined, undefined, []},
                [{"Authorization", "basic X"}]
            },
            {
                {"user", "pass", []},
                [{"Authorization", "Basic " ++ b64creds("user", "pass")}]
            },
            {
                {"user", "pass", []},
                [{"AuThorization", "Basic " ++ b64creds("user", "pass")}]
            },
            {
                {"user", "pass", []},
                [{"Authorization", "bAsIc " ++ b64creds("user", "pass")}]
            },
            {
                {"user", "pass", [{"h", "v"}]},
                [
                    {"Authorization", "Basic " ++ b64creds("user", "pass")},
                    {"h", "v"}
                ]
            }
        ]
    ].


b64creds(User, Pass) ->
    base64:encode_to_string(User ++ ":" ++ Pass).


normalize_rep_test_() ->
    {
        setup,
        fun() -> meck:expect(config, get,
            fun(_, _, Default) -> Default end)
        end,
        fun(_) -> meck:unload() end,
        ?_test(begin
            EJson1 = {[
                {<<"source">>, <<"http://host.com/source_db">>},
                {<<"target">>, <<"http://target.local/db">>},
                {<<"doc_ids">>, [<<"a">>, <<"c">>, <<"b">>]},
                {<<"other_field">>, <<"some_value">>}
            ]},
            Rep1 = couch_replicator_docs:parse_rep_doc_without_id(EJson1),
            EJson2 = {[
                {<<"other_field">>, <<"unrelated">>},
                {<<"target">>, <<"http://target.local/db">>},
                {<<"source">>, <<"http://host.com/source_db">>},
                {<<"doc_ids">>, [<<"c">>, <<"a">>, <<"b">>]},
                {<<"other_field2">>, <<"unrelated2">>}
            ]},
            Rep2 = couch_replicator_docs:parse_rep_doc_without_id(EJson2),
            ?assertEqual(normalize_rep(Rep1), normalize_rep(Rep2))
        end)
    }.

-endif.

% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(mango_simple_proc).
-behavior(gen_server).

-export([
    start_link/0,
    set_timeout/2,
    prompt/2
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

% DDoc Fields
-define(SELECTOR, <<"selector">>).
-define(KEYS, <<"keys">>).
-define(VALUES, <<"values">>).

% Proc commands
-define(RESET, <<"reset">>).
-define(ADD_FUN, <<"add_fun">>).
-define(MAP_DOC, <<"map_doc">>).
-define(REDUCE, <<"reduce">>).
-define(REREDUCE, <<"rereduce">>).

-record(st, {
    indexes = [],
    timeout = 5000
}).

-record(idx, {
    selector,
    keys,
    values
}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

set_timeout(Pid, TimeOut) when is_integer(TimeOut), TimeOut > 0 ->
    gen_server:call(Pid, {set_timeout, TimeOut}).

prompt(Pid, Data) ->
    gen_server:call(Pid, {prompt, Data}).

init(_) ->
    {ok, #st{}}.

handle_call({set_timeout, TimeOut}, _From, #st{} = St) ->
    {reply, ok, St#st{timeout = TimeOut}};
handle_call({prompt, [?RESET | _QueryConfig]}, _From, #st{} = St) ->
    {reply, true, St#st{indexes = []}};
handle_call({prompt, [?ADD_FUN, IndexInfo | _IgnoreRest]}, _From, #st{} = St) ->
    #st{indexes = Indexes} = St,
    case get_index_def(IndexInfo) of
        #idx{} = Idx -> {reply, true, St#st{indexes = Indexes ++ [Idx]}};
        undefined -> {reply, true, St}
    end;
handle_call({prompt, [?MAP_DOC, Doc]}, _From, St) ->
    {reply, map_doc(St, mango_json:to_binary(Doc)), St};
handle_call({prompt, [?REDUCE, RedSrcs, _]}, _From, St) ->
    {reply, [true, [null || _ <- RedSrcs]], St};
handle_call({prompt, [?REREDUCE, RedSrcs, _]}, _From, St) ->
    {reply, [true, [null || _ <- RedSrcs]], St};
handle_call(Msg, _From, St) ->
    {stop, {invalid_call, Msg}, {invalid_call, Msg}, St}.

handle_cast(garbage_collect, St) ->
    erlang:garbage_collect(),
    {noreply, St};
handle_cast(stop, St) ->
    {stop, normal, St};
handle_cast(Msg, St) ->
    {stop, {invalid_cast, Msg}, St}.

map_doc(#st{indexes = Indexes}, Doc) ->
    lists:map(fun(Idx) -> get_index_entries(Idx, Doc) end, Indexes).

get_index_entries(#idx{} = Idx, Doc) ->
    #idx{selector = Selector, keys = Keys, values = Values} = Idx,
    case should_index(Selector, Doc) of
        false -> [];
        true -> process_doc(Keys, Values, Doc)
    end.

process_doc(Keys, undefined, Doc) ->
    case get_index_values(Keys, Doc) of
        [] -> [];
        [_ | _] = KeyResults -> [[KeyResults, null]]
    end;
process_doc(Keys, [_ | _] = Values, Doc) ->
    case get_index_values(Keys, Doc) of
        [] ->
            [];
        [_ | _] = KeyResults ->
            case get_index_values(Values, Doc) of
                [] -> [];
                [_ | _] = ValueResults -> [[KeyResults, ValueResults]]
            end
    end.

get_index_values(Fields, Doc) ->
    MapF = fun(Field) ->
        case mango_doc:get_field(Doc, Field) of
            not_found -> not_found;
            bad_path -> not_found;
            Value -> Value
        end
    end,
    Results = lists:map(MapF, Fields),
    case lists:member(not_found, Results) of
        true -> [];
        false -> Results
    end.

should_index(Selector, Doc) ->
    case mango_doc:get_field(Doc, <<"_id">>) of
        <<"_design/", _/binary>> -> false;
        _ -> mango_selector:match(Selector, Doc)
    end.

get_selector({IdxProps}) ->
    case couch_util:get_value(?SELECTOR, IdxProps, {[]}) of
        {L} = Selector when is_list(L) -> mango_selector:normalize(Selector);
        _ -> undefined
    end.

get_field({IdxProps}, <<FieldName/binary>>) ->
    case couch_util:get_value(FieldName, IdxProps) of
        Fields when is_list(Fields) ->
            case lists:all(fun is_binary/1, Fields) of
                true -> Fields;
                false -> undefined
            end;
        _ ->
            undefined
    end.

get_index_def(IndexInfo) ->
    Selector = get_selector(IndexInfo),
    Keys = get_field(IndexInfo, ?KEYS),
    Values = get_field(IndexInfo, ?VALUES),
    case {Selector, Keys, Values} of
        {{_}, [_ | _], [_ | _]} ->
            #idx{selector = Selector, keys = Keys, values = Values};
        {{_}, [_ | _], undefined} ->
            #idx{selector = Selector, keys = Keys, values = undefined};
        {_, _, _} ->
            undefined
    end.

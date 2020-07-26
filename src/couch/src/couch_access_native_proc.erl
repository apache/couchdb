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

-module(couch_access_native_proc).
-behavior(gen_server).


-export([
    start_link/0,
    set_timeout/2,
    prompt/2
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-record(st, {
    indexes = [],
    timeout = 5000 % TODO: make configurable
}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).


set_timeout(Pid, TimeOut) when is_integer(TimeOut), TimeOut > 0 ->
    gen_server:call(Pid, {set_timeout, TimeOut}).


prompt(Pid, Data) ->
    gen_server:call(Pid, {prompt, Data}).


init(_) ->
    {ok, #st{}}.


terminate(_Reason, _St) ->
    ok.


handle_call({set_timeout, TimeOut}, _From, St) ->
    {reply, ok, St#st{timeout=TimeOut}};

handle_call({prompt, [<<"reset">>]}, _From, St) ->
    {reply, true, St#st{indexes=[]}};

handle_call({prompt, [<<"reset">>, _QueryConfig]}, _From, St) ->
    {reply, true, St#st{indexes=[]}};

handle_call({prompt, [<<"add_fun">>, IndexInfo]}, _From, St) ->
    {reply, true, St};

handle_call({prompt, [<<"map_doc">>, Doc]}, _From, St) ->
    {reply, map_doc(St, mango_json:to_binary(Doc)), St};

handle_call({prompt, [<<"reduce">>, _, _]}, _From, St) ->
    {reply, null, St};

handle_call({prompt, [<<"rereduce">>, _, _]}, _From, St) ->
    {reply, null, St};

handle_call({prompt, [<<"index_doc">>, Doc]}, _From, St) ->
    {reply, [[]], St};

handle_call(Msg, _From, St) ->
    {stop, {invalid_call, Msg}, {invalid_call, Msg}, St}.

handle_cast(garbage_collect, St) ->
    erlang:garbage_collect(),
    {noreply, St};

handle_cast(Msg, St) ->
    {stop, {invalid_cast, Msg}, St}.


handle_info(Msg, St) ->
    {stop, {invalid_info, Msg}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

% return value is an array of arrays, first dimension is the different indexes
% [0] will be by-access-id // for this test, later we should make this by-access
% -seq, since that one we will always need, and by-access-id can be opt-in.
% the second dimension is the number of emit kv pairs:
% [ // the return value
%   [ // the first view
%     ['k1', 'v1'], // the first k/v pair for the first view
%     ['k2', 'v2']  // second, etc.
%   ],
%   [ // second view
%     ['l1', 'w1'] // first k/v par in second view
%   ]
% ]
% {"id":"account/bongel","key":"account/bongel","value":{"rev":"1-967a00dff5e02add41819138abb3284d"}},

map_doc(_St, {Doc}) ->
    case couch_util:get_value(<<"_access">>, Doc) of
        undefined ->
            [[],[]]; % do not index this doc
        Access when is_list(Access) ->
            Id = couch_util:get_value(<<"_id">>, Doc),
            Rev = couch_util:get_value(<<"_rev">>, Doc),
            Seq = couch_util:get_value(<<"_seq">>, Doc),
            Deleted = couch_util:get_value(<<"_deleted">>, Doc, false),
            BodySp = couch_util:get_value(<<"_body_sp">>, Doc),
            % by-access-id
            ById = case Deleted of
                false ->
                    lists:map(fun(UserOrRole) -> [
                        [[UserOrRole, Id], Rev]
                    ] end, Access);
                _True -> [[]]
            end,

            % by-access-seq
            BySeq = lists:map(fun(UserOrRole) -> [
                [[UserOrRole, Seq], [{rev, Rev}, {deleted, Deleted}, {body_sp, BodySp}]]
            ] end, Access),
            ById ++ BySeq;
        Else ->
            % TODO: no comprende: should not be needed once we implement
            % _access field validation
            [[],[]]
    end.

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

-module(csrt_test_helper).

-export([
        rctx_gen/0,
        rctx_gen/1,
        rctxs/0,
        jrctx/1
]).

-include_lib("couch/include/couch_eunit.hrl").

-include("../../src/couch_stats_resource_tracker.hrl").
-define(RCTX_RANGE, 1000).
-define(RCTX_COUNT, 10000).

%% Dirty hack for hidden records as .hrl is only in src/
-define(RCTX_RPC, #rpc_worker{from = {self(), make_ref()}}).
-define(RCTX_COORDINATOR, #coordinator{method = 'GET', path = "/foo/_all_docs"}).

rctx_gen() ->
    rctx_gen(#{}).

rctx_gen(Opts0) ->
    DbnameGen = one_of([<<"foo">>, <<"bar">>, ?tempdb]),
    TypeGen = one_of([?RCTX_RPC, ?RCTX_COORDINATOR]),
    R = fun() -> rand:uniform(?RCTX_RANGE) end,
    R10 = fun() -> 3 + rand:uniform(round(?RCTX_RANGE / 10)) end,
    Occasional = one_of([0, 0, 0, 0, 0, R]),
    Nonce = one_of(["9c54fa9283", "foobar7799" | lists:duplicate(10, fun nonce/0)]),
    Base = #{
        dbname => DbnameGen,
        db_open => R10,
        docs_read => R,
        docs_written => Occasional,
        get_kp_node => R10,
        get_kv_node => R,
        nonce => Nonce,
        pid_ref => {self(), make_ref()},
        ioq_calls => R,
        rows_read => R,
        type => TypeGen,
        %% Hack because we need to modify both fields
        '_do_changes' => true
    },
    Opts = maps:merge(Base, Opts0),
    csrt_entry:from_map(
        maps:fold(
            fun
                %% Hack for changes because we need to modify both
                %% changes_processed (rows_read) and changes_returned but the
                %% latter must be <= the former
                ('_do_changes', V, Acc) ->
                    case V of
                        true ->
                            Processed = R(),
                            Returned = (one_of([0, 0, 1, Processed, rand:uniform(Processed)]))(),
                            maps:put(
                                rows_read,
                                Processed,
                                maps:put(changes_returned, Returned, Acc)
                            );
                        _ ->
                            Acc
                    end;
                (K, F, Acc) when is_function(F) ->
                    maps:put(K, F(), Acc);
                (K, V, Acc) ->
                    maps:put(K, V, Acc)
            end,
            #{},
            Opts
        )
    ).

rctxs() ->
    [rctx_gen() || _ <- lists:seq(1, ?RCTX_COUNT)].

jrctx(Rctx) ->
    JRctx = csrt_entry:to_json(Rctx),
    case csrt_logger:should_truncate_reports() of
        true ->
            maps:filter(fun(_K, V) -> V > 0 end, JRctx);
        false ->
            JRctx
    end.

nonce() ->
    couch_util:to_hex(crypto:strong_rand_bytes(5)).

one_of(L) ->
    fun() ->
        case lists:nth(rand:uniform(length(L)), L) of
            F when is_function(F) ->
                F();
            N ->
                N
        end
    end.

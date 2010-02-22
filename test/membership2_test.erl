%%% -*-  erlang-indent-level:2  -*-
-module(membership2_test).
-author('cliff@powerset.com').
-author('brad@cloudant.com').

-include("../include/config.hrl").
-include("../include/common.hrl").
-include("../include/test.hrl").

-include_lib("eunit/include/eunit.hrl").

% singular_startup_sequence_test() ->
%   %% configuration:start_link(#config{n=1,r=1,w=1,q=6,directory=?TMP_DIR}),
%   {ok, _} = mock:mock(configuration),
%   mock:expects(configuration, get_config, fun(_Args) -> true end,
%                #config{n=1,r=1,w=1,q=6,directory=?TMP_DIR}, 3),
%   {ok, _} = mock:mock(replication),
%   mock:expects(replication, partners, fun({_, [a], _}) -> true end, []),
%   mock:expects(replication, partners_plus, fun({a, [a]}) -> true end, []),
%   {ok, M} = membership2:start_link(a, [a]),
%   State = gen_server:call(M, state),
%   ?assertEqual(a, State#membership.node),
%   ?assertEqual([a], State#membership.nodes),
%   mock:verify_and_stop(replication),
%   membership2:stop(M),
%   %% configuration:stop(),
%   mock:verify_and_stop(configuration),
%   ?assertMatch({ok, [[a]]}, file:consult(?TMP_FILE("a.world"))),
%   file:delete(?TMP_FILE("a.world")).

% -define(NODEA, {a, ["d", "1", "4"]}).
% -define(NODEB, {b, ["e", "3", "1"]}).
% -define(NODEC, {c, ["f", "1", "2"]}).
% -define(NODES, [?NODEA, ?NODEB, ?NODEC]).

% multi_startup_sequence_test() ->
%   {ok, _} = mock:mock(configuration),
%   mock:expects(configuration, get_config, fun(_Args) -> true end,
%                (#config{n=3,r=1,w=1,q=6,directory=?TMP_DIR}), 3),
%   {ok, _} = mock:mock(replication),
%   VersionOne = vector_clock:create(make_ref()),
%   Pid1 = make_ref(),
%   VersionTwo = vector_clock:create(make_ref()),
%   Pid2 = make_ref(),
%   mock:expects(replication, partners, fun({_, ?NODES, _}) -> true end, [?NODEB, ?NODEC]),
%   {ok, _} = stub:stub(membership2, call_join, fun(?NODEB, ?NODEA) ->
%       {VersionOne, ?NODES, [{1,Pid1}]};
%     (?NODEC, ?NODEA) ->
%       {VersionTwo, ?NODES, [{2,Pid2}]}
%     end, 2),
%   ?debugMsg("proxied"),
%   ?debugFmt("check process code: ~p", [erlang:check_process_code(self(), membership2)]),
%   {ok, M} = membership2:start_link(?NODEA, ?NODES),
%   State = gen_server:call(M, state),
%   ?assertEqual(?NODEA, State#membership.node),
%   ?assertEqual(?NODES, State#membership.nodes),
%   % Servers = State#membership.servers,
%   % ?assertMatch([{1,Pid1},{2,Pid2}], membership2:servers_to_list(Servers)),
%   ?assertEqual(greater, vector_clock:compare(State#membership.version, VersionOne)),
%   ?assertEqual(greater, vector_clock:compare(State#membership.version, VersionTwo)),
%   mock:verify_and_stop(replication),
%   membership2:stop(M),
%   mock:verify_and_stop(configuration),
%   ?assertMatch({ok, [?NODES]}, file:consult(?TMP_FILE("a.world"))),
%   file:delete(?TMP_FILE("a.world")).

% startup_and_first_servers_for_key_test() ->
%   configuration:start_link(#config{n=1,r=1,w=1,q=6,directory=?TMP_DIR}),
%   {ok, _} = mock:mock(replication),
%   mock:expects(replication, partners, fun({_, [a], _}) -> true end, []),
%   {ok, M} = membership2:start_link(a, [a]),
%   _State = gen_server:call(M, state),
%   ?assertEqual([], membership2:servers_for_key("blah")),
%   mock:verify_and_stop(replication),
%   membership2:stop(M),
%   configuration:stop(),
%   ?assertMatch({ok, [[a]]}, file:consult(?TMP_FILE("a.world"))),
%   file:delete(?TMP_FILE("a.world")).

% startup_and_register_test() ->
%   configuration:start_link(#config{n=1,r=1,w=1,q=0,directory=?TMP_DIR}),
%   {ok, _} = mock:mock(replication),
%   mock:expects(replication, partners, fun({_, [?NODEA], _}) -> true end, [], 3),
%   {ok, M} = membership2:start_link(?NODEA, [?NODEA]),
%   SServer1 = make_server(),
%   SServer2 = make_server(),
%   membership2:register(1, SServer1),
%   membership2:register(1, SServer2),
%   ?assertEqual([SServer1, SServer2], membership2:servers_for_key("blah")),
%   mock:verify_and_stop(replication),
%   membership2:stop(M),
%   configuration:stop(),
%   SServer1 ! stop,
%   SServer2 ! stop,
%   file:delete(?TMP_FILE("a.world")).

% handle_local_server_outage_test() ->
%   configuration:start_link(#config{n=1,r=1,w=1,q=0,directory=?TMP_DIR}),
%   {ok, _} = mock:mock(replication),
%   mock:expects(replication, partners, fun({_, [?NODEA], _}) -> true end, [], 4),
%   {ok, M} = membership2:start_link(?NODEA, [?NODEA]),
%   SServer1 = make_server(),
%   SServer2 = make_server(),
%   membership2:register(1, SServer1),
%   membership2:register(1, SServer2),
%   SServer1 ! stop,
%   timer:sleep(1),
%   ?assertEqual([SServer2], membership2:servers_for_key("blah")),
%   mock:verify_and_stop(replication),
%   membership2:stop(M),
%   configuration:stop(),
%   SServer2 ! stop,
%   file:delete(?TMP_FILE("a.world")).

% full_gossip_test() ->
%   configuration:start_link(#config{n=1,r=1,w=1,q=2,directory=priv_dir()}),
%   {ok, _} = mock:mock(replication),
%   mock:expects(replication, partners, fun({_, ?NODES, _}) -> true end, [?NODEB, ?NODEC],4),


% make_server() ->
%   spawn(fun() ->
%       receive
%         stop -> ok
%       end
%     end).

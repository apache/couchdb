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

-module(mem3_bdu_test).


-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


-define(TDEF_FE(Name), fun(Arg) -> {atom_to_list(Name), ?_test(Name(Arg))} end).

-define(USER, "mem3_bdu_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(JSON, {"Content-Type", "application/json"}).
-define(DBS, "_node/_local/_dbs").


setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist=false),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Db = ?tempdb(),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/"]),
    {Url, Db}.


teardown({Url, Db}) ->
    sync_delete_db(Url, Db),
    ok = config:delete("admins", ?USER, _Persist=false).


start_couch() ->
    test_util:start_couch([mem3, chttpd]).


stop_couch(Ctx) ->
    test_util:stop_couch(Ctx).


mem3_bdu_shard_doc_test_() ->
    {
        "mem3 bdu shard doc tests",
        {
            setup,
            fun start_couch/0, fun stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    ?TDEF_FE(t_can_insert_shard_map_doc),
                    ?TDEF_FE(t_missing_by_node_section),
                    ?TDEF_FE(t_by_node_not_a_map),
                    ?TDEF_FE(t_missing_by_range_section),
                    ?TDEF_FE(t_by_range_not_a_map),
                    ?TDEF_FE(t_missing_range_in_by_range),
                    ?TDEF_FE(t_missing_node_in_by_range_node_list),
                    ?TDEF_FE(t_missing_node_in_by_node),
                    ?TDEF_FE(t_missing_range_in_by_node_range_list),
                    ?TDEF_FE(t_by_node_val_not_array),
                    ?TDEF_FE(t_by_range_val_not_array),
                    ?TDEF_FE(t_design_docs_are_not_validated),
                    ?TDEF_FE(t_replicated_changes_not_validated)
                ]
            }
        }
    }.


t_can_insert_shard_map_doc({Top, Db}) ->
    Node = atom_to_binary(node(), utf8),
    Range = <<"00000000-ffffffff">>,
    ShardMap = #{
        <<"_id">> => Db,
        <<"by_node">> => #{Node => [Range]},
        <<"by_range">> => #{Range => [Node]},
        <<"suffix">> => suffix()
    },
    {Code, Res} = req(post, Top ++ ?DBS, ShardMap),
    ?assertEqual(201, Code),
    ?assertMatch(#{<<"ok">> := true}, Res).


t_missing_by_node_section({Top, Db}) ->
    Node = atom_to_binary(node(), utf8),
    Range = <<"00000000-ffffffff">>,
    ShardMap = #{
        <<"_id">> => Db,
        <<"by_range">> => #{Range => [Node]}
    },
    ?assertMatch({403, _}, req(post, Top ++ ?DBS, ShardMap)).


t_by_node_not_a_map({Top, Db}) ->
    Node = atom_to_binary(node(), utf8),
    Range = <<"00000000-ffffffff">>,
    ShardMap = #{
        <<"_id">> => Db,
        <<"by_node">> => 42,
        <<"by_range">> => #{Range => [Node]}
    },
    ?assertMatch({403, _}, req(post, Top ++ ?DBS, ShardMap)).


t_missing_by_range_section({Top, Db}) ->
    Node = atom_to_binary(node(), utf8),
    Range = <<"00000000-ffffffff">>,
    ShardMap = #{
        <<"_id">> => Db,
        <<"by_node">> => #{Node => [Range]}
    },
    ?assertMatch({403, _}, req(post, Top ++ ?DBS, ShardMap)).


t_by_range_not_a_map({Top, Db}) ->
    Node = atom_to_binary(node(), utf8),
    Range = <<"00000000-ffffffff">>,
    ShardMap = #{
        <<"_id">> => Db,
        <<"by_node">> => #{Node => [Range]},
        <<"by_range">> => 42
    },
    ?assertMatch({403, _}, req(post, Top ++ ?DBS, ShardMap)).


t_missing_range_in_by_range({Top, Db}) ->
    Node = atom_to_binary(node(), utf8),
    Range = <<"00000000-ffffffff">>,
    ShardMap = #{
        <<"_id">> => Db,
        <<"by_node">> => #{Node => [Range]},
        <<"by_range">> => #{<<"xyz">> => [Node]}
    },
    ?assertMatch({403, _}, req(post, Top ++ ?DBS, ShardMap)).


t_missing_node_in_by_range_node_list({Top, Db}) ->
    Node = atom_to_binary(node(), utf8),
    Range = <<"00000000-ffffffff">>,
    ShardMap = #{
        <<"_id">> => Db,
        <<"by_node">> => #{Node => [Range]},
        <<"by_range">> => #{Range => [<<"xyz">>]}
    },
    ?assertMatch({403, _}, req(post, Top ++ ?DBS, ShardMap)).


t_missing_node_in_by_node({Top, Db}) ->
    Node = atom_to_binary(node(), utf8),
    Range = <<"00000000-ffffffff">>,
    ShardMap = #{
        <<"_id">> => Db,
        <<"by_node">> => #{<<"xyz">> => [Range]},
        <<"by_range">> => #{Range => [Node]}
    },
    ?assertMatch({403, _}, req(post, Top ++ ?DBS, ShardMap)).


t_missing_range_in_by_node_range_list({Top, Db}) ->
    Node = atom_to_binary(node(), utf8),
    Range = <<"00000000-ffffffff">>,
    ShardMap = #{
        <<"_id">> => Db,
        <<"by_node">> => #{Node => [<<"xyz">>]},
        <<"by_range">> => #{Range => [Node]}
    },
    ?assertMatch({403, _}, req(post, Top ++ ?DBS, ShardMap)).


t_by_node_val_not_array({Top, Db}) ->
    Node = atom_to_binary(node(), utf8),
    Range = <<"00000000-ffffffff">>,
    ShardMap = #{
        <<"_id">> => Db,
        <<"by_node">> => #{Node => 42},
        <<"by_range">> => #{Range => [Node]}
    },
    ?assertMatch({403, _}, req(post, Top ++ ?DBS, ShardMap)).


t_by_range_val_not_array({Top, Db}) ->
    Node = atom_to_binary(node(), utf8),
    Range = <<"00000000-ffffffff">>,
    ShardMap = #{
        <<"_id">> => Db,
        <<"by_node">> => #{Node => [Range]},
        <<"by_range">> => #{Range => 42}
    },
    ?assertMatch({403, _}, req(post, Top ++ ?DBS, ShardMap)).


t_design_docs_are_not_validated({Top, _}) ->
    DDoc = #{<<"_id">> => <<"_design/ddoc_bdu_test">>},
    {Code, Res} = req(post, Top ++ ?DBS, DDoc),
    ?assertEqual(201, Code),
    #{<<"rev">> := Rev} = Res,
    Deleted = #{
        <<"id">> => <<"_design/ddoc_bdu_test">>,
        <<"_rev">> => Rev,
        <<"_deleted">> => true
    },
    ?assertMatch({200, _}, req(post, Top ++ ?DBS, Deleted)).


t_replicated_changes_not_validated({Top, Db}) ->
    Node = atom_to_binary(node(), utf8),
    Range = <<"00000000-ffffffff">>,
    ShardMap = #{
        <<"_id">> => Db,
        <<"by_node">> => #{Node => [Range]},
        % missing <<"by_range">>, we can tollerate it
        % and not crash the backend
        <<"suffix">> => suffix(),
        <<"_rev">> => <<"1-abc">>,
        <<"_revisions">> => #{
            <<"ids">> => [<<"abc">>],
            <<"start">> => 1
        }
    },
    Docs = #{
        <<"docs">> => [ShardMap],
        <<"new_edits">> => false
    },
    {Code, Res} = req(post, Top ++ ?DBS ++ "/_bulk_docs", Docs),
    ?assertEqual(201, Code),
    ?assertEqual([], Res),
    Deleted = #{
        <<"id">> => Db,
        <<"_rev">> => <<"1-abc">>,
        <<"_deleted">> => true
    },
    ?assertMatch({200, _}, req(post, Top ++ ?DBS, Deleted)).


delete_db(Top, Db) when is_binary(Db) ->
    Url = Top ++ binary_to_list(Db),
    case test_request:get(Url, [?AUTH]) of
        {ok, 404, _, _} ->
            not_found;
        {ok, 200, _, _} ->
            {ok, 200, _, _} = test_request:delete(Url, [?AUTH]),
            ok
    end.


sync_delete_db(Top, Db) when is_binary(Db) ->
    delete_db(Top, Db),
    try
        Shards = mem3:local_shards(Db),
        ShardNames = [mem3:name(S) || S <- Shards],
        [couch_server:delete(N, [?ADMIN_CTX]) || N <- ShardNames],
        ok
    catch
        error:database_does_not_exist ->
            ok
    end.


req(Method, Url, #{} = Body) ->
    req(Method, Url, jiffy:encode(Body));

req(Method, Url, Body) ->
    Headers = [?JSON, ?AUTH],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers, Body),
    {Code, jiffy:decode(Res, [return_maps])}.


suffix() ->
    integer_to_list(erlang:system_time(second)).

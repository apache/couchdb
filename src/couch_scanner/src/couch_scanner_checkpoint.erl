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

-module(couch_scanner_checkpoint).

-export([
    write/2,
    read/1,
    reset/1,
    list_all/0
]).

-include_lib("couch/include/couch_db.hrl").

-define(PREFIX, "scanner-checkpoint-").

write(<<Plugin/binary>>, #{} = State) ->
    with_db(fun(Db) -> update_doc(Db, doc_id(Plugin), State) end).

read(<<Plugin/binary>>) ->
    with_db(fun(Db) -> load_doc(Db, doc_id(Plugin)) end).

reset(<<Plugin/binary>>) ->
    with_db(fun(Db) -> delete_doc(Db, doc_id(Plugin)) end).

list_all() ->
    with_db(fun(Db) -> list_all(Db) end).

% Private functions

doc_id(Plugin) ->
    % Dashes are more conventional for doc ids
    Plugin1 = binary:replace(Plugin, <<"_">>, <<"-">>, [global]),
    <<?LOCAL_DOC_PREFIX, ?PREFIX, Plugin1/binary>>.

plugin_id(<<?LOCAL_DOC_PREFIX, ?PREFIX, DocId/binary>>) ->
    binary:replace(DocId, <<"-">>, <<"_">>, [global]).

delete_doc(Db, DocId) ->
    case couch_db:open_doc(Db, DocId, []) of
        {ok, #doc{revs = {_, RevList}}} ->
            {ok, _} = couch_db:delete_doc(Db, DocId, RevList),
            ok;
        {not_found, _} ->
            ok
    end.

update_doc(Db, DocId, #{} = Body) ->
    EJsonBody = ejson_props(Body#{<<"_id">> => DocId}),
    Doc = couch_doc:from_json_obj(EJsonBody),
    case couch_db:open_doc(Db, DocId, []) of
        {ok, #doc{revs = Revs}} ->
            {ok, _} = couch_db:update_doc(Db, Doc#doc{revs = Revs}, []);
        {not_found, _} ->
            {ok, _} = couch_db:update_doc(Db, Doc, [])
    end,
    ok.

load_doc(Db, DocId) ->
    case couch_db:open_doc(Db, DocId, [ejson_body]) of
        {ok, #doc{body = EJsonBody}} -> ejson_map(EJsonBody);
        {not_found, _} -> not_found
    end.

list_all(Db) ->
    IdPref = <<?LOCAL_DOC_PREFIX, ?PREFIX>>,
    Len = byte_size(IdPref),
    FoldFun = fun(#doc{id = Id, body = Body}, #{} = Acc) ->
        case Id of
            <<IdPref:Len/binary, _/binary>> ->
                {ok, Acc#{plugin_id(Id) => ejson_map(Body)}};
            <<_/binary>> ->
                {stop, Acc}
        end
    end,
    Opts = [{start_key, IdPref}],
    {ok, #{} = Docs} = couch_db:fold_local_docs(Db, FoldFun, #{}, Opts),
    Docs.

with_db(Fun) when is_function(Fun, 1) ->
    DbName = config:get("mem3", "shards_db", "_dbs"),
    case mem3_util:ensure_exists(DbName) of
        {ok, Db} ->
            try
                Fun(Db)
            after
                catch couch_db:close(Db)
            end;
        Else ->
            throw(Else)
    end.

ejson_map(Obj) ->
    couch_scanner_util:ejson_map(Obj).

ejson_props(Obj) ->
    ?JSON_DECODE(?JSON_ENCODE(Obj)).

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

couch_scanner_checkpoint_test_() ->
    {
        foreach,
        fun test_util:start_couch/0,
        fun test_util:stop_couch/1,
        [
            ?TDEF_FE(t_read_write_reset)
        ]
    }.

t_read_write_reset(_) ->
    couch_scanner:reset_checkpoints(),
    Plugin = <<"scanner_plugin_abc">>,
    ?assertEqual(ok, write(Plugin, #{<<"foo">> => 1})),
    ?assertEqual(#{<<"foo">> => 1}, read(Plugin)),
    ?assertEqual(ok, write(Plugin, #{<<"bar">> => 2})),
    ?assertEqual(#{<<"bar">> => 2}, read(Plugin)),
    ?assertEqual(#{<<"scanner_plugin_abc">> => #{<<"bar">> => 2}}, list_all()),
    ?assertEqual(not_found, read(<<"scanner_plugin_other">>)),
    ?assertEqual(ok, reset(Plugin)),
    ?assertEqual(not_found, read(Plugin)).

-endif.

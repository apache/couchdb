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

-module(couch_replicator_httpd).

-export([
    handle_req/1,
    handle_scheduler_req/1
]).

-import(couch_httpd, [
    send_json/2,
    send_json/3,
    send_method_not_allowed/2
]).


-include_lib("couch/include/couch_db.hrl").
-include("couch_replicator.hrl").


-define(DEFAULT_TASK_LIMIT, 100).


handle_scheduler_req(#httpd{method='GET', path_parts=[_,<<"jobs">>]}=Req) ->
    Limit = couch_replicator_utils:parse_int_param(Req, "limit",
        ?DEFAULT_TASK_LIMIT, 0, infinity),
    Skip = couch_replicator_utils:parse_int_param(Req, "skip", 0, 0,
        infinity),
    Jobs1 = couch_replicator:jobs(),
    Total = length(Jobs1),
    Offset = min(Skip, Total),
    Jobs2 = lists:sublist(Jobs1, Offset + 1, Limit),
    send_json(Req, #{
        <<"total_rows">> => Total,
        <<"offset">> => Offset,
        <<"jobs">> => Jobs2
    });
handle_scheduler_req(#httpd{method='GET', path_parts=[_,<<"jobs">>,JobId]}=Req) ->
    case couch_replicator:job(JobId) of
        {ok, JobInfo} ->  send_json(Req, JobInfo);
        {error, not_found} -> throw(not_found)
    end;
handle_scheduler_req(#httpd{method='GET', path_parts=[_,<<"docs">>]}=Req) ->
    handle_scheduler_docs(?REP_DB_NAME, Req);
handle_scheduler_req(#httpd{method='GET', path_parts=[_,<<"docs">>,Db]}=Req)
        when ?IS_REP_DB(Db) ->
    handle_scheduler_docs(Db, Req);
handle_scheduler_req(#httpd{method='GET', path_parts=[_,<<"docs">>,Db,DocId]}
        = Req)  when ?IS_REP_DB(Db) ->
    handle_scheduler_doc(Db, DocId, Req);
% Allow users to pass in unencoded _replicator database names (/ are not
% escaped). This is possible here because _replicator is not a valid document
% ID so can disambiguate between an element of a db path and the document ID.
handle_scheduler_req(#httpd{method='GET', path_parts=[_,<<"docs">>|Unquoted]}
        = Req) ->
    case parse_unquoted_docs_path(Unquoted) of
        {db_only, Db} ->
            handle_scheduler_docs(Db, Req);
        {db_and_doc, Db, DocId} ->
            handle_scheduler_doc(Db, DocId, Req);
        {error, invalid} ->
            throw(bad_request)
    end;
handle_scheduler_req(#httpd{method='GET'} = _Req) ->
    throw(not_found);
handle_scheduler_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").


handle_req(#httpd{method = 'POST', user_ctx = UserCtx} = Req) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    RepDoc = couch_httpd:json_body_obj(Req),
    case couch_replicator:replicate(RepDoc, UserCtx) of
        {error, {Error, Reason}} ->
            send_json(Req, 500, #{
                <<"error">> => couch_util:to_binary(Error),
                <<"reason">> => couch_util:to_binary(Reason)
            });
        {error, not_found} ->
            throw(not_found);
        {error, Reason} ->
            send_json(Req, 500, #{<<"error">> => couch_util:to_binary(Reason)});
        {ok, {cancelled, JobId}} ->
            send_json(Req, 200, #{<<"ok">> => true, <<"_local_id">> => JobId});
        {ok, {continuous, JobId}} ->
            send_json(Req, 202, #{<<"ok">> => true, <<"_local_id">> => JobId});
        {ok, #{} = CheckpointHistory} ->
            Res = maps:merge(#{<<"ok">> => true}, CheckpointHistory),
            send_json(Req, Res)
        end;

handle_req(Req) ->
    send_method_not_allowed(Req, "POST").


handle_scheduler_docs(DbName, #httpd{user_ctx = UserCtx} = Req) ->
    try fabric2_db:open(DbName, [{user_ctx, UserCtx}]) of
        {ok, Db} ->
            ok = fabric2_db:check_is_member(Db),
            StatesQs = chttpd:qs_value(Req, "states"),
            States = couch_replicator_utils:parse_replication_states(StatesQs),
            Docs = couch_replicator:docs(Db, States),
            send_json(Req, #{
                <<"total_rows">> => length(Docs),
                <<"offset">> => 0,
                <<"docs">> => Docs
            })
    catch
        error:database_does_not_exist ->
            throw(not_found)
    end.


handle_scheduler_doc(DbName, DocId, #httpd{user_ctx = UserCtx} = Req) ->
     try fabric2_db:open(DbName, [{user_ctx, UserCtx}]) of
        {ok, Db} ->
             ok = fabric2_db:check_is_member(Db),
             case couch_replicator:doc(Db, DocId) of
                {ok, DocInfo} ->  send_json(Req, DocInfo);
                {error, not_found} -> throw(not_found)
             end
     catch
         error:database_does_not_exist ->
             throw(not_found)
     end.


parse_unquoted_docs_path([_, _ | _] = Unquoted) ->
    DbAndAfter = lists:dropwhile(fun(E) -> E =/= ?REP_DB_NAME end, Unquoted),
    BeforeRDb = lists:takewhile(fun(E) -> E =/= ?REP_DB_NAME end, Unquoted),
    case DbAndAfter of
        [] ->
            {error, invalid};
        [?REP_DB_NAME] ->
            {db_only, filename:join(BeforeRDb ++ [?REP_DB_NAME])};
        [?REP_DB_NAME, DocId] ->
            {db_and_doc, filename:join(BeforeRDb ++ [?REP_DB_NAME]), DocId}
    end.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

unquoted_scheduler_docs_path_test_() ->
    [?_assertEqual(Res, parse_unquoted_docs_path(Path)) || {Res, Path} <- [
        {{error, invalid}, [<<"a">>,<< "b">>]},
        {{db_only, <<"a/_replicator">>}, [<<"a">>, ?REP_DB_NAME]},
        {{db_only, <<"a/b/_replicator">>}, [<<"a">>, <<"b">>,
            ?REP_DB_NAME]},
        {{db_and_doc, <<"_replicator">>, <<"x">>},
            [?REP_DB_NAME, <<"x">>]},
        {{db_and_doc, <<"a/_replicator">>, <<"x">>}, [<<"a">>,
            ?REP_DB_NAME, <<"x">>]},
        {{error, invalid}, [<<"a/_replicator">>,<<"x">>]}
    ]].

-endif.

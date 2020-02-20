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

-module(mango_crud).

-export([
    insert/3,
    find/5,
    update/4,
    delete/3,
    explain/3
]).

-export([
    collect_cb/2
]).


-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").


insert(Db, #doc{}=Doc, Opts) ->
    insert(Db, [Doc], Opts);
insert(Db, {_}=Doc, Opts) ->
    insert(Db, [Doc], Opts);
insert(Db, Docs, Opts) when is_list(Docs) ->
    case fabric2_db:update_docs(Db, Docs, Opts) of
        {ok, Results0} ->
            {ok, lists:zipwith(fun result_to_json/2, Docs, Results0)};
        {accepted, Results0} ->
            {ok, lists:zipwith(fun result_to_json/2, Docs, Results0)};
        {aborted, Errors} ->
            {error, lists:map(fun result_to_json/1, Errors)}
    end.


find(Db, Selector, Callback, UserAcc, Opts) ->
    {ok, Cursor} = mango_cursor:create(Db, Selector, Opts),
    mango_cursor:execute(Cursor, Callback, UserAcc).


update(Db, Selector, Update, Options) ->
    Upsert = proplists:get_value(upsert, Options),
    case collect_docs(Db, Selector, Options) of
        {ok, []} when Upsert ->
            InitDoc = mango_doc:update_as_insert(Update),
            case mango_doc:has_operators(InitDoc) of
                true ->
                    ?MANGO_ERROR(invalid_upsert_with_operators);
                false ->
                    % Probably need to catch and rethrow errors from
                    % this function.
                    Doc = couch_doc:from_json_obj(InitDoc),
                    NewDoc = case Doc#doc.id of
                        <<"">> ->
                            Doc#doc{id=couch_uuids:new(), revs={0, []}};
                        _ ->
                            Doc
                    end,
                    insert(Db, NewDoc, Options)
            end;
        {ok, Docs} ->
            NewDocs = lists:map(fun(Doc) ->
                mango_doc:apply_update(Doc, Update)
            end, Docs),
            insert(Db, NewDocs, Options);
        Else ->
            Else
    end.


delete(Db, Selector, Options) ->
    case collect_docs(Db, Selector, Options) of
        {ok, Docs} ->
            NewDocs = lists:map(fun({Props}) ->
                {[
                    {<<"_id">>, proplists:get_value(<<"_id">>, Props)},
                    {<<"_rev">>, proplists:get_value(<<"_rev">>, Props)},
                    {<<"_deleted">>, true}
                ]}
            end, Docs),
            insert(Db, NewDocs, Options);
        Else ->
            Else
    end.


explain(Db, Selector, Opts) ->
    {ok, Cursor} = mango_cursor:create(Db, Selector, Opts),
    mango_cursor:explain(Cursor).


result_to_json(#doc{id=Id}, Result) ->
    result_to_json(Id, Result);
result_to_json({Props}, Result) ->
    Id = couch_util:get_value(<<"_id">>, Props),
    result_to_json(Id, Result);
result_to_json(DocId, {ok, NewRev}) ->
    {[
        {id, DocId},
        {rev, couch_doc:rev_to_str(NewRev)}
    ]};
result_to_json(DocId, {accepted, NewRev}) ->
    {[
        {id, DocId},
        {rev, couch_doc:rev_to_str(NewRev)},
        {accepted, true}
    ]};
result_to_json(DocId, Error) ->
    % chttpd:error_info/1 because this is coming from fabric
    % and not internal mango operations.
    {_Code, ErrorStr, Reason} = chttpd:error_info(Error),
    {[
        {id, DocId},
        {error, ErrorStr},
        {reason, Reason}
    ]}.


% This is for errors because for some reason we
% need a different return value for errors? Blargh.
result_to_json({{Id, Rev}, Error}) ->
    {_Code, ErrorStr, Reason} = chttpd:error_info(Error),
    {[
        {id, Id},
        {rev, couch_doc:rev_to_str(Rev)},
        {error, ErrorStr},
        {reason, Reason}
    ]}.


collect_docs(Db, Selector, Options) ->
    Cb = fun ?MODULE:collect_cb/2,
    case find(Db, Selector, Cb, [], Options) of
        {ok, Docs} ->
            {ok, lists:reverse(Docs)};
        Else ->
            Else
    end.


collect_cb({row, Doc}, Acc) ->
    {ok, [Doc | Acc]}.


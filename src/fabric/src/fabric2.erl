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

-module(fabric2).










open_doc(Db, DocId, Options) ->
    fabric2_db:with_tx(Db, fun(TxDb) ->
        case fabric2_doc:get_fdi(TxDb, DocId) of
            not_found ->
                {not_found, missing};
            #full_doc_info{} = FDI ->
                {_, Path} = couch_doc:to_doc_info_path(FDI),
                case fabric2_doc:open(TxDb, DocId, Path) of
                    #doc{} = Doc -> {ok, Doc};
                    Error -> Error
                end
        end
    end).


open_revs(Db, DocId, Revs, Options) ->
    fabric2_db:with_tx(Db, fun(TxDb) ->
        case fabrci2_doc:get_fdi(TxDb, DocId) of
            not_found ->
                {not_found, missing};
            #full_doc_info{} = FDI ->
                case fabric2_doc:open_revs(TxDb, FDI, Revs, Options) of
                    [_ | _] = Opened -> {ok, Opened};
                    Error -> Error
                end
        end
    end).


update_doc(Db, Doc, Options) ->
    fabric2_db:with_tx(Db, fun(TxDb) ->
        case fabric2_doc:update(TxDb, Doc, opts(Options)) of
            {ok, []} ->
                % replication no-op
                #doc{revs = {Pos, [RevId | _]}} = doc(Db, Doc),
                {ok, {Pos, RevId}};
            {ok, NewRev} ->
                {ok, NewRev};
           {error, Error} ->
               throw(Error)
        end
    end).


update_docs(DbName, Docs, Options) when is_binary(DbName) ->
    update_docs(open_db(DbName, Options), Docs, Options);

update_docs(Db, Docs, Options) ->
    fabric2_db:with_tx(Db, fun(TxDb) ->
        {Resps, Status} = lists:mapfoldl(fun(Doc, Acc) ->
            case fabric2_doc:update(TxDb, Doc, opts(Options)) of
                {ok, _} = Resp ->
                    {Resp, Acc};
                {error, _} = Resp ->
                    {Resp, error}
            end
        end, ok, Docs),
        {Status, Resps}
    end).


docs(Db, Docs) ->
    lists:map(fun(Doc) -> doc(Db, Doc) end, Docs).


doc(_Db, #doc{} = Doc) ->
    Doc;

doc(Db, {_} = Doc) ->
    couch_db:doc_from_json_obj_validate(Db, Doc);

doc(_Db, Doc) ->
    erlang:error({illegal_doc_format, Doc}).


opts(Options) ->
    lists:foldl(fun(Opt, Acc) ->
        add_option(Opt, Acc)
    end, Options, [user_ctx, io_priority]).


add_option(Key, Options) ->
    case couch_util:get_value(Key, Options) of
        undefined ->
            case erlang:get(Key) of
                undefined ->
                    Options;
                Value ->
                    [{Key, Value} | Options]
            end;
        _ ->
            Options
    end.

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


-module(mango_fdb_special).

-include_lib("couch/include/couch_db.hrl").
-include("mango_cursor.hrl").


-export([
    query/4
]).


query(Db, CallBack, Cursor, Args) ->
    Acc = #{
        cursor => Cursor,
        callback => CallBack
    },
    Opts = args_to_fdb_opts(Args),
    {ok, Acc1} = fabric2_db:fold_docs(Db, fun fold_cb/2, Acc, Opts),
    {ok, maps:get(cursor, Acc1)}.


args_to_fdb_opts(Args) ->
    #{
        start_key := StartKey,
        end_key := EndKey
    } = Args,

    mango_fdb:base_fold_opts(Args)
        ++ [{include_docs, true}]
        ++ start_key_opts(StartKey)
        ++ end_key_opts(EndKey).


start_key_opts(StartKey) ->
    [{start_key, fabric2_util:encode_all_doc_key(StartKey)}].


end_key_opts(?MAX_STR) ->
    [];

end_key_opts(EndKey) ->
    [{end_key, fabric2_util:encode_all_doc_key(EndKey)}].


fold_cb({row, Props}, Acc) ->
    #{
        cursor := Cursor,
        callback := Callback
    } = Acc,
    case is_design_doc(Props) of
        true ->
            {ok, Acc};
        false ->
            Doc = couch_util:get_value(doc, Props),
            Key = couch_util:get_value(key, Props),
            {Go, Cursor1} = Callback({doc, Key, Doc}, Cursor),
            {Go, Acc#{cursor := Cursor1}}
    end;

fold_cb(Message, Acc) ->
    #{
        cursor := Cursor,
        callback := Callback
    } = Acc,
    {Go, Cursor1} = Callback(Message, Cursor),
    {Go, Acc#{cursor := Cursor1}}.


is_design_doc(RowProps) ->
    case couch_util:get_value(id, RowProps) of
        <<"_design/", _/binary>> -> true;
        _ -> false
    end.

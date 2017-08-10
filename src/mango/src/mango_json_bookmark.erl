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


-module(mango_json_bookmark).

-export([
    update_args/2,
    create/1
]).


-include_lib("couch_mrview/include/couch_mrview.hrl").
-include("mango_cursor.hrl").
-include("mango.hrl").

update_args(EncodedBookmark, Args) ->
    Bookmark = unpack(EncodedBookmark),
    update_args2(Bookmark, Args).


update_args2(Bookmark, #mrargs{skip = Skip} = Args) when is_list(Bookmark) ->
    case lists:keymember(startkey, 1, Bookmark) andalso lists:keymember(startkey_docid, 1, Bookmark) of
        true -> 
            {startkey, Startkey} = lists:keyfind(startkey, 1, Bookmark),
            {startkey_docid, StartkeyDocId} = lists:keyfind(startkey_docid, 1, Bookmark),
            Args2 = Args#mrargs{
                start_key = Startkey,
                start_key_docid = StartkeyDocId,
                skip = 1 + Skip
            },
            Args2;
        false ->
            Args
    end;
update_args2(_Bookmark, Args) ->
    Args.
    

create(#cursor{bookmark_docid = BookmarkDocId, bookmark_key = BookmarkKey}) when BookmarkKey =/= undefined ->
    QueryArgs = [
        {startkey_docid, BookmarkDocId},
        {startkey, BookmarkKey}
    ],
    Bin = term_to_binary(QueryArgs, [compressed, {minor_version,1}]),
    couch_util:encodeBase64Url(Bin);
create(#cursor{bookmark = Bookmark}) ->
    Bookmark.


unpack(nil) ->
    nil;
unpack(Packed) ->
    try
        binary_to_term(couch_util:decodeBase64Url(Packed))
    catch _:_ ->
        ?MANGO_ERROR({invalid_bookmark, Packed})
    end.
   
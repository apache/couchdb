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

-module(couch_mrview_debug).

-export([
    help/0,
    help/1
]).

-export([
    view_signature/2
]).

-include_lib("couch_mrview/include/couch_mrview.hrl").

help() ->
    [
        view_signature
    ].

-spec help(Function :: atom()) -> ok.
%% erlfmt-ignore
help(view_signature) ->
    io:format("
    view_signature(ShardName, DDocName)
    --------------
    Returns a view signature for given ddoc for a given (non clustered) database.
    ---
    ", []);
help(Unknown) ->
    io:format("Unknown function: `~p`. Please try one of the following:~n", [Unknown]),
    [io:format("    - ~s~n", [Function]) || Function <- help()],
    io:format("    ---~n", []),
    ok.

view_signature(DbName, DDocName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, DDoc} = couch_db:open_doc_int(Db, <<"_design/", DDocName/binary>>, []),
    {ok, IdxState} = couch_mrview_util:ddoc_to_mrst(DDocName, DDoc),
    couch_util:to_hex(IdxState#mrst.sig).

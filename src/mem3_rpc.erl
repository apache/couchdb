% Copyright 2013 Cloudant
%
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

-module(mem3_rpc).


-export([
    get_missing_revs/4,
    update_docs/4,
    load_checkpoint/4,
    save_checkpoint/4
]).


-include("mem3.hrl").
-include_lib("couch/include/couch_db.hrl").


-define(CTX, #user_ctx{roles = [<<"_admin">>]}).


get_missing_revs(Node, DbName, IdsRevs, Options) ->
    rexi_call(Node, {fabric_rpc, get_missing_revs, [DbName, IdsRevs, Options]}).


update_docs(Node, DbName, Docs, Options) ->
    rexi_call(Node, {fabric_rpc, update_docs, [DbName, Docs, Options]}).


load_checkpoint(Node, DbName, DocId, Opts) ->
    rexi_call(Node, {fabric_rpc, open_doc, [DbName, DocId, Opts]}).


save_checkpoint(Node, DbName, Doc, Options) ->
    rexi_call(Node, {fabric_rpc, update_docs, [DbName, [Doc], Options]}).


rexi_call(Node, MFA) ->
    Mon = rexi_monitor:start([rexi_utils:server_pid(Node)]),
    Ref = rexi:cast(Node, self(), MFA, [sync]),
    try
        receive {Ref, {ok, Reply}} ->
            Reply;
        {Ref, Error} ->
            erlang:error(Error);
        {rexi_DOWN, Mon, _, Reason} ->
            erlang:error({rexi_DOWN, {Node, Reason}})
        after 600000 ->
            erlang:error(timeout)
        end
    after
        rexi_monitor:stop(Mon)
    end.

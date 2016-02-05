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

-module(test_engine_attachments).
-compile(export_all).


-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


cet_write_attachment() ->
    {ok, Engine, DbPath, St1} = test_engine_util:init_engine(dbpath),

    AttBin = crypto:rand_bytes(32768),

    try
        [Att0] = test_engine_util:prep_atts(Engine, St1, [
                {<<"ohai.txt">>, AttBin}
            ]),

        {stream, Stream} = couch_att:fetch(data, Att0),
        ?assertEqual(true, Engine:is_active_stream(St1, Stream)),

        Actions = [{create, {<<"first">>, [], [Att0]}}],
        {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions),
        {ok, St3} = Engine:commit_data(St2),
        Engine:terminate(normal, St3),

        {ok, St4} = Engine:init(DbPath, []),
        [FDI] = Engine:open_docs(St4, [<<"first">>]),

        #rev_info{
            rev = {RevPos, PrevRevId},
            deleted = Deleted,
            body_sp = DocPtr
        } = test_engine_util:prev_rev(FDI),

        Doc0 = #doc{
            id = <<"foo">>,
            revs = {RevPos, [PrevRevId]},
            deleted = Deleted,
            body = DocPtr
        },

        Doc1 = Engine:read_doc_body(St4, Doc0),
        Atts1 = if not is_binary(Doc1#doc.atts) -> Doc1#doc.atts; true ->
            couch_compress:decompress(Doc1#doc.atts)
        end,

        StreamSrc = fun(Sp) -> Engine:open_read_stream(St4, Sp) end,
        [Att1] = [couch_att:from_disk_term(StreamSrc, T) || T <- Atts1],
        ReadBin = couch_att:to_binary(Att1),
        ?assertEqual(AttBin, ReadBin)
    catch throw:not_supported ->
        ok
    end.


% N.B. This test may be overly specific for some theoretical
% storage engines that don't re-initialize their
% attachments streams when restarting (for instance if
% we ever have something that stores attachemnts in
% an external object store)
cet_inactive_stream() ->
    {ok, Engine, DbPath, St1} = test_engine_util:init_engine(dbpath),

    AttBin = crypto:rand_bytes(32768),

    try
        [Att0] = test_engine_util:prep_atts(Engine, St1, [
                {<<"ohai.txt">>, AttBin}
            ]),

        {stream, Stream} = couch_att:fetch(data, Att0),
        ?assertEqual(true, Engine:is_active_stream(St1, Stream)),

        Engine:terminate(normal, St1),
        {ok, St2} = Engine:init(DbPath, []),

        ?assertEqual(false, Engine:is_active_stream(St2, Stream))
    catch throw:not_supported ->
        ok
    end.

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

-module(cpse_test_attachments).
-compile(export_all).


-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


cet_write_attachment() ->
    {ok, Db1} = test_engine_util:create_db(),

    AttBin = crypto:strong_rand_bytes(32768),

    try
        [Att0] = test_engine_util:prep_atts(Db1, [
                {<<"ohai.txt">>, AttBin}
            ]),

        {stream, Stream} = couch_att:fetch(data, Att0),
        ?assertEqual(true, couch_db_engine:is_active_stream(Db1, Stream)),

        Actions = [{create, {<<"first">>, {[]}, [Att0]}}],
        {ok, Db2} = test_engine_util:apply_actions(Db1, Actions),
        {ok, _} = couch_db:ensure_full_commit(Db2),
        test_engine_util:shutdown_db(Db2),

        {ok, Db3} = couch_db:reopen(Db2),

        [FDI] = couch_db_engine:open_docs(Db3, [<<"first">>]),

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

        Doc1 = couch_db_engine:read_doc_body(Db3, Doc0),
        Atts1 = if not is_binary(Doc1#doc.atts) -> Doc1#doc.atts; true ->
            couch_compress:decompress(Doc1#doc.atts)
        end,

        StreamSrc = fun(Sp) -> couch_db_engine:open_read_stream(Db3, Sp) end,
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
    {ok, Db1} = test_engine_util:create_db(),

    AttBin = crypto:strong_rand_bytes(32768),

    try
        [Att0] = test_engine_util:prep_atts(Db1, [
                {<<"ohai.txt">>, AttBin}
            ]),

        {stream, Stream} = couch_att:fetch(data, Att0),
        ?assertEqual(true, couch_db_engine:is_active_stream(Db1, Stream)),

        test_engine_util:shutdown_db(Db1),
        {ok, Db2} = couch_db:reopen(Db1),

        ?assertEqual(false, couch_db_engine:is_active_stream(Db2, Stream))
    catch throw:not_supported ->
        ok
    end.

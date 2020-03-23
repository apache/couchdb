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

-module(couch_views_info_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("fabric/test/fabric2_test.hrl").

-define(MAP_FUN1, <<"map_fun1">>).

setup() ->
    Ctx = test_util:start_couch([
            fabric,
            couch_jobs,
            couch_js,
            couch_views
        ]),
    Ctx.

cleanup(Ctx) ->
    test_util:stop_couch(Ctx).

foreach_setup() ->
    {ok, Db} = fabric2_db:create(?tempdb(), [{user_ctx, ?ADMIN_USER}]),
    DDoc = create_ddoc(),
    Doc1 = doc(0, 1),

    {ok, _} = fabric2_db:update_doc(Db, DDoc, []),
    {ok, _} = fabric2_db:update_doc(Db, Doc1, []),

    run_query(Db, DDoc, ?MAP_FUN1),
    {ok, Info} = couch_views:get_views_info(Db, DDoc),
    {Db, Info}.

foreach_teardown({Db, _}) ->
    meck:unload(),
    ok = fabric2_db:delete(fabric2_db:name(Db), []).

views_info_test_() ->
    {
        "Views index info test",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            {
                foreach,
                fun foreach_setup/0,
                fun foreach_teardown/1,
                [
                    fun sig_is_binary/1,
                    fun language_is_js/1,
                    fun update_seq_is_binary/1,
                    fun updater_running_is_boolean/1,
                    fun data_size_is_non_neg_int/1
                ]
            }
        }
    }.

sig_is_binary({_, Info}) ->
    ?_assert(is_binary(prop(signature, Info))).

language_is_js({_, Info}) ->
    ?_assertEqual(<<"javascript">>, prop(language, Info)).

data_size_is_non_neg_int({_, Info}) ->
    ?_assert(check_non_neg_int(data_size, Info)).

updater_running_is_boolean({_, Info}) ->
    ?_assert(is_boolean(prop(updater_running, Info))).

update_seq_is_binary({_, Info}) ->
    ?_assert(is_binary(prop(update_seq, Info))).

check_non_neg_int(Key, Info) ->
    Size = prop(Key, Info),
    is_integer(Size) andalso Size >= 0.

prop(Key, {Props}) when is_list(Props) ->
    prop(Key, Props);
prop([Key], Info) ->
    prop(Key, Info);
prop([Key | Rest], Info) ->
    prop(Rest, prop(Key, Info));
prop(Key, Info) when is_atom(Key), is_list(Info) ->
    couch_util:get_value(Key, Info).

create_ddoc() ->
    couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/bar">>},
        {<<"views">>, {[
            {?MAP_FUN1, {[
                {<<"map">>, <<"function(doc) {emit(doc.val, doc.val);}">>}
            ]}}
        ]}}
    ]}).

doc(Id, Val) ->
    couch_doc:from_json_obj({[
        {<<"_id">>, list_to_binary(integer_to_list(Id))},
        {<<"val">>, Val}
    ]}).

fold_fun({meta, _Meta}, Acc) ->
    {ok, Acc};
fold_fun({row, _} = Row, Acc) ->
    {ok, [Row | Acc]};
fold_fun(complete, Acc) ->
    {ok, lists:reverse(Acc)}.

run_query(#{} = Db, DDoc, <<_/binary>> = View) ->
    couch_views:query(Db, DDoc, View, fun fold_fun/2, [], #mrargs{}).

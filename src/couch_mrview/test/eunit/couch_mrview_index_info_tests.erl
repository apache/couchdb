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

-module(couch_mrview_index_info_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(TIMEOUT, 1000).

setup() ->
    {ok, Db} = couch_mrview_test_util:init_db(?tempdb(), map),
    couch_mrview:query_view(Db, <<"_design/bar">>, <<"baz">>),
    {ok, Info} = couch_mrview:get_info(Db, <<"_design/bar">>),
    {Db, Info}.

teardown({Db, _}) ->
    couch_db:close(Db),
    couch_server:delete(couch_db:name(Db), [?ADMIN_CTX]),
    ok.

view_info_test_() ->
    {
        "Views index tests",
        {
            setup,
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun sig_is_binary/1,
                    fun language_is_js/1,
                    fun file_size_is_non_neg_int/1,
                    fun active_size_is_non_neg_int/1,
                    fun external_size_is_non_neg_int/1,
                    fun active_size_less_than_file_size/1,
                    fun update_seq_is_non_neg_int/1,
                    fun purge_seq_is_non_neg_int/1,
                    fun update_opts_is_bin_list/1
                ]
            }
        }
    }.

sig_is_binary({_, Info}) ->
    ?_assert(is_binary(prop(signature, Info))).

language_is_js({_, Info}) ->
    ?_assertEqual(<<"javascript">>, prop(language, Info)).

file_size_is_non_neg_int({_, Info}) ->
    ?_assert(check_non_neg_int([sizes, file], Info)).

active_size_is_non_neg_int({_, Info}) ->
    ?_assert(check_non_neg_int([sizes, active], Info)).

external_size_is_non_neg_int({_, Info}) ->
    ?_assert(check_non_neg_int([sizes, external], Info)).

active_size_less_than_file_size({_, Info}) ->
    ?_assert(prop([sizes, active], Info) < prop([sizes, file], Info)).

update_seq_is_non_neg_int({_, Info}) ->
    ?_assert(check_non_neg_int(update_seq, Info)).

purge_seq_is_non_neg_int({_, Info}) ->
    ?_assert(check_non_neg_int(purge_seq, Info)).

update_opts_is_bin_list({_, Info}) ->
    Opts = prop(update_options, Info),
    ?_assert(
        is_list(Opts) andalso
            (Opts == [] orelse lists:all([is_binary(B) || B <- Opts]))
    ).

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

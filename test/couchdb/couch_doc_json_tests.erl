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

-module(couch_doc_json_tests).

-include("couch_eunit.hrl").
-include_lib("couchdb/couch_db.hrl").


setup() ->
    couch_config:start_link(?CONFIG_CHAIN),
    couch_config:set("attachments", "compression_level", "0", false),
    ok.

teardown(_) ->
    couch_config:stop().


json_doc_test_() ->
    {
        setup,
        fun setup/0, fun teardown/1,
        [
            {
                "Document from JSON",
                [
                    from_json_success_cases(),
                    from_json_error_cases()
                ]
            },
            {
                "Document to JSON",
                [
                    to_json_success_cases()
                ]
            }
        ]
    }.

from_json_success_cases() ->
    Cases = [
        {
            {[]},
            #doc{},
            "Return an empty document for an empty JSON object."
        },
        {
            {[{<<"_id">>, <<"zing!">>}]},
            #doc{id = <<"zing!">>},
            "Parses document ids."
        },
        {
            {[{<<"_id">>, <<"_design/foo">>}]},
            #doc{id = <<"_design/foo">>},
            "_design/document ids."
        },
        {
            {[{<<"_id">>, <<"_local/bam">>}]},
            #doc{id = <<"_local/bam">>},
            "_local/document ids."
        },
        {
            {[{<<"_rev">>, <<"4-230234">>}]},
            #doc{revs = {4, [<<"230234">>]}},
            "_rev stored in revs."
        },
        {
            {[{<<"soap">>, 35}]},
            #doc{body = {[{<<"soap">>, 35}]}},
            "Non underscore prefixed fields stored in body."
        },
        {
            {[{<<"_attachments">>, {[
                {<<"my_attachment.fu">>, {[
                    {<<"stub">>, true},
                    {<<"content_type">>, <<"application/awesome">>},
                    {<<"length">>, 45}
                ]}},
                {<<"noahs_private_key.gpg">>, {[
                    {<<"data">>, <<"SSBoYXZlIGEgcGV0IGZpc2gh">>},
                    {<<"content_type">>, <<"application/pgp-signature">>}
                ]}}
            ]}}]},
            #doc{atts = [
                #att{
                    name = <<"my_attachment.fu">>,
                    data = stub,
                    type = <<"application/awesome">>,
                    att_len = 45,
                    disk_len = 45,
                    revpos = nil
                },
                #att{
                    name = <<"noahs_private_key.gpg">>,
                    data = <<"I have a pet fish!">>,
                    type = <<"application/pgp-signature">>,
                    att_len = 18,
                    disk_len = 18,
                    revpos = 0
                }
            ]},
            "Attachments are parsed correctly."
        },
        {
            {[{<<"_deleted">>, true}]},
            #doc{deleted = true},
            "_deleted controls the deleted field."
        },
        {
            {[{<<"_deleted">>, false}]},
            #doc{},
            "{\"_deleted\": false} is ok."
        },
        {
            {[
                 {<<"_revisions">>,
                  {[{<<"start">>, 4},
                    {<<"ids">>, [<<"foo1">>, <<"phi3">>, <<"omega">>]}]}},
                 {<<"_rev">>, <<"6-something">>}
             ]},
            #doc{revs = {4, [<<"foo1">>, <<"phi3">>, <<"omega">>]}},
            "_revisions attribute are preferred to _rev."
        },
        {
            {[{<<"_revs_info">>, dropping}]},
            #doc{},
            "Drops _revs_info."
        },
        {
            {[{<<"_local_seq">>, dropping}]},
            #doc{},
            "Drops _local_seq."
        },
        {
            {[{<<"_conflicts">>, dropping}]},
            #doc{},
            "Drops _conflicts."
        },
        {
            {[{<<"_deleted_conflicts">>, dropping}]},
            #doc{},
            "Drops _deleted_conflicts."
        }
    ],
    lists:map(
        fun({EJson, Expect, Msg}) ->
            {Msg, ?_assertMatch(Expect, couch_doc:from_json_obj(EJson))}
        end,
        Cases).

from_json_error_cases() ->
    Cases = [
        {
            [],
            {bad_request, "Document must be a JSON object"},
            "arrays are invalid"
        },
        {
            4,
            {bad_request, "Document must be a JSON object"},
            "integers are invalid"
        },
        {
            true,
            {bad_request, "Document must be a JSON object"},
            "literals are invalid"
        },
        {
            {[{<<"_id">>, {[{<<"foo">>, 5}]}}]},
            {bad_request, <<"Document id must be a string">>},
            "Document id must be a string."
        },
        {
            {[{<<"_id">>, <<"_random">>}]},
            {bad_request,
             <<"Only reserved document ids may start with underscore.">>},
            "Disallow arbitrary underscore prefixed docids."
        },
        {
            {[{<<"_rev">>, 5}]},
            {bad_request, <<"Invalid rev format">>},
            "_rev must be a string"
        },
        {
            {[{<<"_rev">>, "foobar"}]},
            {bad_request, <<"Invalid rev format">>},
            "_rev must be %d-%s"
        },
        {
            {[{<<"_rev">>, "foo-bar"}]},
            "Error if _rev's integer expection is broken."
        },
        {
            {[{<<"_revisions">>, {[{<<"start">>, true}]}}]},
            {doc_validation, "_revisions.start isn't an integer."},
            "_revisions.start must be an integer."
        },
        {
            {[{<<"_revisions">>, {[{<<"start">>, 0}, {<<"ids">>, 5}]}}]},
            {doc_validation, "_revisions.ids isn't a array."},
            "_revions.ids must be a list."
        },
        {
            {[{<<"_revisions">>, {[{<<"start">>, 0}, {<<"ids">>, [5]}]}}]},
            {doc_validation, "RevId isn't a string"},
            "Revision ids must be strings."
        },
        {
            {[{<<"_something">>, 5}]},
            {doc_validation, <<"Bad special document member: _something">>},
            "Underscore prefix fields are reserved."
        }
    ],

    lists:map(fun
        ({EJson, Expect, Msg}) ->
            Error = (catch couch_doc:from_json_obj(EJson)),
            {Msg, ?_assertMatch(Expect, Error)};
        ({EJson, Msg}) ->
            try
                couch_doc:from_json_obj(EJson),
                {"Conversion failed to raise an exception", ?_assert(false)}
            catch
                _:_ -> {Msg, ?_assert(true)}
            end
    end, Cases).

to_json_success_cases() ->
    Cases = [
        {
            #doc{},
            {[{<<"_id">>, <<"">>}]},
            "Empty docs are {\"_id\": \"\"}"
        },
        {
            #doc{id = <<"foo">>},
            {[{<<"_id">>, <<"foo">>}]},
            "_id is added."
        },
        {
            #doc{revs = {5, ["foo"]}},
            {[{<<"_id">>, <<>>}, {<<"_rev">>, <<"5-foo">>}]},
            "_rev is added."
        },
        {
            [revs],
            #doc{revs = {5, [<<"first">>, <<"second">>]}},
            {[
                 {<<"_id">>, <<>>},
                 {<<"_rev">>, <<"5-first">>},
                 {<<"_revisions">>, {[
                     {<<"start">>, 5},
                     {<<"ids">>, [<<"first">>, <<"second">>]}
                 ]}}
             ]},
            "_revisions include with revs option"
        },
        {
            #doc{body = {[{<<"foo">>, <<"bar">>}]}},
            {[{<<"_id">>, <<>>}, {<<"foo">>, <<"bar">>}]},
            "Arbitrary fields are added."
        },
        {
            #doc{deleted = true, body = {[{<<"foo">>, <<"bar">>}]}},
            {[{<<"_id">>, <<>>}, {<<"foo">>, <<"bar">>}, {<<"_deleted">>, true}]},
            "Deleted docs no longer drop body members."
        },
        {
            #doc{meta = [
                {revs_info, 4, [{<<"fin">>, deleted}, {<<"zim">>, missing}]}
            ]},
            {[
                 {<<"_id">>, <<>>},
                 {<<"_revs_info">>, [
                     {[{<<"rev">>, <<"4-fin">>}, {<<"status">>, <<"deleted">>}]},
                     {[{<<"rev">>, <<"3-zim">>}, {<<"status">>, <<"missing">>}]}
                 ]}
             ]},
            "_revs_info field is added correctly."
        },
        {
            #doc{meta = [{local_seq, 5}]},
            {[{<<"_id">>, <<>>}, {<<"_local_seq">>, 5}]},
            "_local_seq is added as an integer."
        },
        {
            #doc{meta = [{conflicts, [{3, <<"yep">>}, {1, <<"snow">>}]}]},
            {[
                {<<"_id">>, <<>>},
                {<<"_conflicts">>, [<<"3-yep">>, <<"1-snow">>]}
            ]},
            "_conflicts is added as an array of strings."
        },
        {
            #doc{meta = [{deleted_conflicts, [{10923, <<"big_cowboy_hat">>}]}]},
            {[
                 {<<"_id">>, <<>>},
                 {<<"_deleted_conflicts">>, [<<"10923-big_cowboy_hat">>]}
             ]},
            "_deleted_conflicsts is added as an array of strings."
        },
        {
            #doc{atts = [
                #att{
                    name = <<"big.xml">>,
                    type = <<"xml/sucks">>,
                    data = fun() -> ok end,
                    revpos = 1,
                    att_len = 400,
                    disk_len = 400
                },
                #att{
                    name = <<"fast.json">>,
                    type = <<"json/ftw">>,
                    data = <<"{\"so\": \"there!\"}">>,
                    revpos = 1,
                    att_len = 16,
                    disk_len = 16
                }
            ]},
            {[
                 {<<"_id">>, <<>>},
                 {<<"_attachments">>, {[
                       {<<"big.xml">>, {[
                           {<<"content_type">>, <<"xml/sucks">>},
                           {<<"revpos">>, 1},
                           {<<"length">>, 400},
                           {<<"stub">>, true}
                       ]}},
                       {<<"fast.json">>, {[
                           {<<"content_type">>, <<"json/ftw">>},
                           {<<"revpos">>, 1},
                           {<<"length">>, 16},
                           {<<"stub">>, true}
                       ]}}
                ]}}
            ]},
            "Attachments attached as stubs only include a length."
        },
        {
            [attachments],
            #doc{atts = [
                #att{
                    name = <<"stuff.txt">>,
                    type = <<"text/plain">>,
                    data = fun() -> <<"diet pepsi">> end,
                    revpos = 1,
                    att_len = 10,
                    disk_len = 10
                },
                #att{
                    name = <<"food.now">>,
                    type = <<"application/food">>,
                    revpos = 1,
                    data = <<"sammich">>
                }
            ]},
            {[
                {<<"_id">>, <<>>},
                {<<"_attachments">>, {[
                   {<<"stuff.txt">>, {[
                       {<<"content_type">>, <<"text/plain">>},
                       {<<"revpos">>, 1},
                       {<<"data">>, <<"ZGlldCBwZXBzaQ==">>}
                   ]}},
                   {<<"food.now">>, {[
                       {<<"content_type">>, <<"application/food">>},
                       {<<"revpos">>, 1},
                       {<<"data">>, <<"c2FtbWljaA==">>}
                   ]}}
                ]}}
            ]},
            "Attachments included inline with attachments option."
        }
    ],

    lists:map(fun
        ({Doc, EJson, Msg}) ->
            {Msg, ?_assertMatch(EJson, couch_doc:to_json_obj(Doc, []))};
        ({Options, Doc, EJson, Msg}) ->
            {Msg, ?_assertMatch(EJson, couch_doc:to_json_obj(Doc, Options))}
    end, Cases).

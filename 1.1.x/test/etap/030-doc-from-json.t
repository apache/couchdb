#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./src/couchdb -pa ./src/mochiweb -sasl errlog_type false -noshell

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

%% XXX: Figure out how to -include("couch_db.hrl")
-record(doc, {id= <<"">>, revs={0, []}, body={[]},
            atts=[], deleted=false, meta=[]}).
-record(att, {name, type, att_len, disk_len, md5= <<>>, revpos=0, data,
            encoding=identity}).

main(_) ->
    test_util:init_code_path(),
    etap:plan(26),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail()
    end,
    ok.

test() ->
    couch_config:start_link(test_util:config_files()),
    couch_config:set("attachments", "compression_level", "0", false),
    ok = test_from_json_success(),
    ok = test_from_json_errors(),
    ok.

test_from_json_success() ->
    Cases = [
        {
            {[]},
            #doc{},
            "Return an empty document for an empty JSON object."
        },
        {
            {[{<<"_id">>, <<"zing!">>}]},
            #doc{id= <<"zing!">>},
            "Parses document ids."
        },
        {
            {[{<<"_id">>, <<"_design/foo">>}]},
            #doc{id= <<"_design/foo">>},
            "_design/document ids."
        },
        {
            {[{<<"_id">>, <<"_local/bam">>}]},
            #doc{id= <<"_local/bam">>},
            "_local/document ids."
        },
        {
            {[{<<"_rev">>, <<"4-230234">>}]},
            #doc{revs={4, [<<"230234">>]}},
            "_rev stored in revs."
        },
        {
            {[{<<"soap">>, 35}]},
            #doc{body={[{<<"soap">>, 35}]}},
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
            #doc{atts=[
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
            #doc{deleted=true},
            "_deleted controls the deleted field."
        },
        {
            {[{<<"_deleted">>, false}]},
            #doc{},
            "{\"_deleted\": false} is ok."
        },
        {
            {[
                {<<"_revisions">>, {[
                    {<<"start">>, 4},
                    {<<"ids">>, [<<"foo1">>, <<"phi3">>, <<"omega">>]}
                ]}},
                {<<"_rev">>, <<"6-something">>}
            ]},
            #doc{revs={4, [<<"foo1">>, <<"phi3">>, <<"omega">>]}},
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

    lists:foreach(fun({EJson, Expect, Mesg}) ->
        etap:is(couch_doc:from_json_obj(EJson), Expect, Mesg)
    end, Cases),
    ok.

test_from_json_errors() ->
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
            {[{<<"_revisions">>, {[
                {<<"start">>, 0},
                {<<"ids">>, 5}
            ]}}]},
            {doc_validation, "_revisions.ids isn't a array."},
            "_revions.ids must be a list."
        },
        {
            {[{<<"_revisions">>, {[
                {<<"start">>, 0},
                {<<"ids">>, [5]}
            ]}}]},
            {doc_validation, "RevId isn't a string"},
            "Revision ids must be strings."
        },
        {
            {[{<<"_something">>, 5}]},
            {doc_validation, <<"Bad special document member: _something">>},
            "Underscore prefix fields are reserved."
        }
    ],

    lists:foreach(fun
        ({EJson, Expect, Mesg}) ->
            Error = (catch couch_doc:from_json_obj(EJson)),
            etap:is(Error, Expect, Mesg);
        ({EJson, Mesg}) ->
            try
                couch_doc:from_json_obj(EJson),
                etap:ok(false, "Conversion failed to raise an exception.")
            catch
                _:_ -> etap:ok(true, Mesg)
            end
    end, Cases),
    ok.

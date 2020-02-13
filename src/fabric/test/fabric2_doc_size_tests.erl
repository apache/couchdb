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

-module(fabric2_doc_size_tests).


-include_lib("couch/include/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").


% Doc body size calculations
% ID: size(Doc#doc.id)
% Rev: size(erlfdb_tuple:encode(Start)) + size(Rev) % where Rev is usually 16
% Deleted: 1 % (binary value is one byte)
% Body: couch_ejson_size:external_size(Body) % Where empty is {} which is 2)


-define(NUM_RANDOM_TESTS, 1000).


-define(DOC_IDS, [
    {0, <<>>},
    {1, <<"a">>},
    {3, <<"foo">>},
    {6, <<"foobar">>},
    {32, <<"af196ae095631b020eedf8f69303e336">>}
]).

-define(REV_STARTS, [
    {1, 0},
    {2, 1},
    {2, 255},
    {3, 256},
    {3, 65535},
    {4, 65536},
    {4, 16777215},
    {5, 16777216},
    {5, 4294967295},
    {6, 4294967296},
    {6, 1099511627775},
    {7, 1099511627776},
    {7, 281474976710655},
    {8, 281474976710656},
    {8, 72057594037927935},
    {9, 72057594037927936},
    {9, 18446744073709551615},

    % The jump from 9 to 11 bytes is because when we
    % spill over into the bigint range of 9-255
    % bytes we have an extra byte that encodes the
    % length of the bigint.
    {11, 18446744073709551616}
]).

-define(REVS, [
    {0, <<>>},
    {8, <<"foobarba">>},
    {16, <<"foobarbazbambang">>}
]).

-define(DELETED, [
    {1, true},
    {1, false}
]).

-define(BODIES, [
    {2, {[]}},
    {13, {[{<<"foo">>, <<"bar">>}]}},
    {28, {[{<<"b">>, <<"a">>}, {<<"c">>, [true, null, []]}]}}
]).

-define(ATT_NAMES, [
    {5, <<"a.txt">>},
    {7, <<"foo.csv">>},
    {29, <<"a-longer-name-for-example.bat">>}
]).

-define(ATT_TYPES, [
    {24, <<"application/octet-stream">>},
    {10, <<"text/plain">>},
    {9, <<"image/png">>}
]).

-define(ATT_BODIES, [
    {0, <<>>},
    {1, <<"g">>},
    {6, <<"foobar">>},
    {384, <<
        "xlasdjfsapoiewrposdlfadfuaducvwerwlkdsfljdfusfsd"
        "xlasdjfsapoiewrposdlfadfuaducvwerwlkdsfljdfusfsd"
        "xlasdjfsapoiewrposdlfadfuaducvwerwlkdsfljdfusfsd"
        "xlasdjfsapoiewrposdlfadfuaducvwerwlkdsfljdfusfsd"
        "xlasdjfsapoiewrposdlfadfuaducvwerwlkdsfljdfusfsd"
        "xlasdjfsapoiewrposdlfadfuaducvwerwlkdsfljdfusfsd"
        "xlasdjfsapoiewrposdlfadfuaducvwerwlkdsfljdfusfsd"
        "xlasdjfsapoiewrposdlfadfuaducvwerwlkdsfljdfusfsd"
    >>}
]).

-define(LDOC_IDS, [
    {8, <<"_local/a">>},
    {10, <<"_local/foo">>},
    {13, <<"_local/foobar">>},
    {39, <<"_local/af196ae095631b020eedf8f69303e336">>}
]).

-define(LDOC_REVS, [
    {1, <<"0">>},
    {2, <<"10">>},
    {3, <<"100">>},
    {4, <<"1000">>},
    {5, <<"10000">>},
    {6, <<"100000">>},
    {7, <<"1000000">>}
]).


empty_doc_test() ->
    ?assertEqual(4, fabric2_util:rev_size(#doc{})).


docid_size_test() ->
    lists:foreach(fun({Size, DocId}) ->
        ?assertEqual(4 + Size, fabric2_util:rev_size(#doc{id = DocId}))
    end, ?DOC_IDS).


rev_size_test() ->
    lists:foreach(fun({StartSize, Start}) ->
        lists:foreach(fun({RevSize, Rev}) ->
            Doc = #doc{
                revs = {Start, [Rev]}
            },
            ?assertEqual(3 + StartSize + RevSize, fabric2_util:rev_size(Doc))
        end, ?REVS)
    end, ?REV_STARTS).


deleted_size_test() ->
    lists:foreach(fun({Size, Deleted}) ->
        ?assertEqual(3 + Size, fabric2_util:rev_size(#doc{deleted = Deleted}))
    end, ?DELETED).


body_size_test() ->
    lists:foreach(fun({Size, Body}) ->
        ?assertEqual(2 + Size, fabric2_util:rev_size(#doc{body = Body}))
    end, ?BODIES).


att_names_test() ->
    lists:foreach(fun({Size, AttName}) ->
        Att = mk_att(AttName, <<>>, <<>>, false),
        Doc = #doc{atts = [Att]},
        ?assertEqual(4 + Size, fabric2_util:rev_size(Doc))
    end, ?ATT_NAMES).


att_types_test() ->
    lists:foreach(fun({Size, AttType}) ->
        Att = mk_att(<<"foo">>, AttType, <<>>, false),
        Doc = #doc{atts = [Att]},
        ?assertEqual(7 + Size, fabric2_util:rev_size(Doc))
    end, ?ATT_TYPES).


att_bodies_test() ->
    lists:foreach(fun({Size, AttBody}) ->
        Att1 = mk_att(<<"foo">>, <<>>, AttBody, false),
        Doc1 = #doc{atts = [Att1]},
        ?assertEqual(7 + Size, fabric2_util:rev_size(Doc1)),

        Att2 = mk_att(<<"foo">>, <<>>, AttBody, true),
        Doc2 = #doc{atts = [Att2]},
        ?assertEqual(7 + 16 + Size, fabric2_util:rev_size(Doc2))
    end, ?ATT_BODIES).


local_doc_ids_test() ->
    lists:foreach(fun({Size, LDocId}) ->
        ?assertEqual(3 + Size, fabric2_util:ldoc_size(mk_ldoc(LDocId, 0)))
    end, ?LDOC_IDS).


local_doc_revs_test() ->
    lists:foreach(fun({Size, Rev}) ->
        Doc = mk_ldoc(<<"_local/foo">>, Rev),
        ?assertEqual(12 + Size, fabric2_util:ldoc_size(Doc))
    end, ?LDOC_REVS).


local_doc_bodies_test() ->
    lists:foreach(fun({Size, Body}) ->
        Doc = mk_ldoc(<<"_local/foo">>, 0, Body),
        ?assertEqual(11 + Size, fabric2_util:ldoc_size(Doc))
    end, ?BODIES).


doc_combinatorics_test() ->
    Elements = [
        {?DOC_IDS, fun(Doc, DocId) -> Doc#doc{id = DocId} end},
        {?REV_STARTS, fun(Doc, RevStart) ->
            #doc{revs = {_, RevIds}} = Doc,
            Doc#doc{revs = {RevStart, RevIds}}
        end},
        {?REVS, fun(Doc, Rev) ->
           #doc{revs = {Start, _}} = Doc,
           Doc#doc{revs = {Start, [Rev]}}
        end},
        {?DELETED, fun(Doc, Deleted) -> Doc#doc{deleted = Deleted} end},
        {?BODIES, fun(Doc, Body) -> Doc#doc{body = Body} end}
    ],
    doc_combine(Elements, 0, #doc{}).


doc_combine([], TotalSize, Doc) ->
    ?assertEqual(TotalSize, fabric2_util:rev_size(Doc));

doc_combine([{Elems, UpdateFun} | Rest], TotalSize, Doc) ->
    lists:foreach(fun({Size, Elem}) ->
        doc_combine(Rest, TotalSize + Size, UpdateFun(Doc, Elem))
    end, Elems).


local_doc_combinatorics_test() ->
    Elements = [
        {?LDOC_IDS, fun(Doc, DocId) -> Doc#doc{id = DocId} end},
        {?LDOC_REVS, fun(Doc, Rev) -> Doc#doc{revs = {0, [Rev]}} end},
        {?BODIES, fun(Doc, Body) -> Doc#doc{body = Body} end}
    ],
    local_doc_combine(Elements, 0, #doc{}).


local_doc_combine([], TotalSize, Doc) ->
    ?assertEqual(TotalSize, fabric2_util:ldoc_size(Doc));

local_doc_combine([{Elems, UpdateFun} | Rest], TotalSize, Doc) ->
    lists:foreach(fun({Size, Elem}) ->
        local_doc_combine(Rest, TotalSize + Size, UpdateFun(Doc, Elem))
    end, Elems).


random_docs_test() ->
    lists:foreach(fun(_) ->
        {DocIdSize, DocId} = choose(?DOC_IDS),
        {RevStartSize, RevStart} = choose(?REV_STARTS),
        {RevSize, Rev} = choose(?REVS),
        {DeletedSize, Deleted} = choose(?DELETED),
        {BodySize, Body} = choose(?BODIES),
        NumAtts = choose([0, 1, 2, 5]),
        {Atts, AttSize} = lists:mapfoldl(fun(_, Acc) ->
            {S, A} = random_att(),
            {A, Acc + S}
        end, 0, lists:seq(1, NumAtts)),
        Doc = #doc{
            id = DocId,
            revs = {RevStart, [Rev]},
            deleted = Deleted,
            body = Body,
            atts = Atts
        },
        Expect = lists:sum([
            DocIdSize,
            RevStartSize,
            RevSize,
            DeletedSize,
            BodySize,
            AttSize
        ]),
        ?assertEqual(Expect, fabric2_util:rev_size(Doc))
    end, lists:seq(1, ?NUM_RANDOM_TESTS)).


random_att() ->
    {NameSize, Name} = choose(?ATT_NAMES),
    {TypeSize, Type} = choose(?ATT_TYPES),
    {BodySize, Body} = choose(?ATT_BODIES),
    {Md5Size, AddMd5} = choose([{0, false}, {16, true}]),
    AttSize = lists:sum([NameSize, TypeSize, BodySize, Md5Size]),
    {AttSize, mk_att(Name, Type, Body, AddMd5)}.


mk_att(Name, Type, Data, AddMd5) ->
    Md5 = if not AddMd5 -> <<>>; true ->
        erlang:md5(Data)
    end,
    couch_att:new([
        {name, Name},
        {type, Type},
        {att_len, size(Data)},
        {data, Data},
        {encoding, identity},
        {md5, Md5}
    ]).


mk_ldoc(DocId, Rev) ->
    mk_ldoc(DocId, Rev, {[]}).


mk_ldoc(DocId, Rev, Body) ->
    #doc{
        id = DocId,
        revs = {0, [Rev]},
        body = Body
    }.


choose(Options) ->
    Pos = rand:uniform(length(Options)),
    lists:nth(Pos, Options).

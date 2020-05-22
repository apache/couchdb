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

-module(couch_views_http).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-export([
    parse_body_and_query/2,
    parse_body_and_query/3,
    parse_params/2,
    parse_params/4,
    row_to_obj/1,
    row_to_obj/2,
    view_cb/2,
    paginated/5,
    paginated/6
]).

-define(BOOKMARK_VSN, 1).

parse_body_and_query(#httpd{method='POST'} = Req, Keys) ->
    Props = chttpd:json_body_obj(Req),
    parse_body_and_query(Req, Props, Keys);

parse_body_and_query(Req, Keys) ->
    parse_params(chttpd:qs(Req), Keys, #mrargs{keys=Keys, group=undefined,
        group_level=undefined}, [keep_group_level]).

parse_body_and_query(Req, {Props}, Keys) ->
    Args = #mrargs{keys=Keys, group=undefined, group_level=undefined},
    BodyArgs = parse_params(Props, Keys, Args, [decoded]),
    parse_params(chttpd:qs(Req), Keys, BodyArgs, [keep_group_level]).

parse_params(#httpd{}=Req, Keys) ->
    parse_params(chttpd:qs(Req), Keys);
parse_params(Props, Keys) ->
    Args = #mrargs{},
    parse_params(Props, Keys, Args).


parse_params(Props, Keys, Args) ->
    parse_params(Props, Keys, Args, []).


parse_params([{"bookmark", Bookmark}], _Keys, #mrargs{}, _Options) ->
    bookmark_decode(Bookmark);

parse_params(Props, Keys, #mrargs{}=Args, Options) ->
    case couch_util:get_value("bookmark", Props, nil) of
        nil ->
            ok;
        _ ->
            throw({bad_request, "Cannot use `bookmark` with other options"})
    end,
    couch_mrview_http:parse_params(Props, Keys, Args, Options).


row_to_obj(Row) ->
    Id = couch_util:get_value(id, Row),
    row_to_obj(Id, Row).


row_to_obj(Id, Row) ->
    couch_mrview_http:row_to_obj(Id, Row).


view_cb(Msg, #vacc{paginated = false}=Acc) ->
    couch_mrview_http:view_cb(Msg, Acc);
view_cb(Msg, #vacc{paginated = true}=Acc) ->
    paginated_cb(Msg, Acc).


paginated_cb({row, Row}, #vacc{buffer=Buf}=Acc) ->
    {ok, Acc#vacc{buffer = [row_to_obj(Row) | Buf]}};

paginated_cb({error, Reason}, #vacc{}=_Acc) ->
    throw({error, Reason});

paginated_cb(complete, #vacc{buffer=Buf}=Acc) ->
    {ok, Acc#vacc{buffer=lists:reverse(Buf)}};

paginated_cb({meta, Meta}, #vacc{}=VAcc) ->
    MetaMap = lists:foldl(fun(MetaData, Acc) ->
        case MetaData of
            {_Key, undefined} ->
                Acc;
            {total, _Value} ->
                %% We set total_rows elsewere
                Acc;
            {Key, Value} ->
                maps:put(list_to_binary(atom_to_list(Key)), Value, Acc)
        end
    end, #{}, Meta),
    {ok, VAcc#vacc{meta=MetaMap}}.


paginated(Req, EtagTerm, #mrargs{page_size = PageSize} = Args, KeyFun, Fun) ->
    Etag = couch_httpd:make_etag(EtagTerm),
    chttpd:etag_respond(Req, Etag, fun() ->
        hd(do_paginated(PageSize, [Args], KeyFun, Fun))
    end).


paginated(Req, EtagTerm, PageSize, QueriesArgs, KeyFun, Fun) when is_list(QueriesArgs) ->
    Etag = couch_httpd:make_etag(EtagTerm),
    chttpd:etag_respond(Req, Etag, fun() ->
        Results = do_paginated(PageSize, QueriesArgs, KeyFun, Fun),
        #{results => Results}
    end).


do_paginated(PageSize, QueriesArgs, KeyFun, Fun) when is_list(QueriesArgs) ->
    {_N, Results} = lists:foldl(fun(Args0, {Limit, Acc}) ->
        case Limit > 0 of
            true ->
                {OriginalLimit, Args} = set_limit(Args0#mrargs{page_size = Limit}),
                {Meta, Items} = Fun(Args),
                Result0 = maybe_add_next_bookmark(
                    OriginalLimit, PageSize, Args, Meta, Items, KeyFun),
                Result = maybe_add_previous_bookmark(Args, Result0, KeyFun),
                #{total_rows := Total} = Result,
                {Limit - Total, [Result | Acc]};
            false ->
                Bookmark = bookmark_encode(Args0),
                Result = #{
                    rows => [],
                    next => Bookmark,
                    total_rows => 0
                },
                {Limit, [Result | Acc]}
        end
    end, {PageSize, []}, QueriesArgs),
    lists:reverse(Results).


maybe_add_next_bookmark(OriginalLimit, PageSize, Args0, Response, Items, KeyFun) ->
    #mrargs{
        page_size = RequestedLimit,
        extra = Extra
    } = Args0,
    case check_completion(OriginalLimit, RequestedLimit, Items) of
        {Rows, nil} ->
            maps:merge(Response, #{
                rows => Rows,
                total_rows => length(Rows)
            });
        {Rows, Next} ->
            FirstKey = first_key(KeyFun, Rows),
            NextKey = KeyFun(Next),
            if is_binary(NextKey) -> ok; true ->
                throw("Provided KeyFun should return binary")
            end,
            Args = Args0#mrargs{
                page_size = PageSize,
                start_key = NextKey,
                extra = lists:keystore(fk, 1, Extra, {fk, FirstKey})
            },
            Bookmark = bookmark_encode(Args),
            maps:merge(Response, #{
                rows => Rows,
                next => Bookmark,
                total_rows => length(Rows)
            })
    end.


maybe_add_previous_bookmark(#mrargs{extra = Extra} = Args, #{rows := Rows} = Result, KeyFun) ->
    StartKey = couch_util:get_value(fk, Extra),
    case first_key(KeyFun, Rows) of
        undefined ->
            Result;
        EndKey ->
            Bookmark = bookmark_encode(
                Args#mrargs{
                    start_key = StartKey,
                    end_key = EndKey,
                    inclusive_end = false
                }
            ),
            maps:put(previous, Bookmark, Result)
    end.


first_key(_KeyFun, []) ->
    undefined;

first_key(KeyFun, [First | _]) ->
    KeyFun(First).


set_limit(#mrargs{page_size = PageSize, limit = Limit} = Args)
        when is_integer(PageSize) andalso Limit > PageSize ->
    {Limit, Args#mrargs{limit = PageSize + 1}};

set_limit(#mrargs{page_size = PageSize, limit = Limit} = Args)
        when is_integer(PageSize)  ->
    {Limit, Args#mrargs{limit = Limit + 1}}.


check_completion(OriginalLimit, RequestedLimit, Items)
        when is_integer(OriginalLimit) andalso OriginalLimit =< RequestedLimit ->
    {Rows, _} = split(OriginalLimit, Items),
    {Rows, nil};

check_completion(_OriginalLimit, RequestedLimit, Items) ->
    split(RequestedLimit, Items).


split(Limit, Items) when length(Items) > Limit ->
    case lists:split(Limit, Items) of
        {Head, [NextItem | _]} ->
            {Head, NextItem};
        {Head, []} ->
            {Head, nil}
    end;

split(_Limit, Items) ->
    {Items, nil}.


bookmark_encode(Args0) ->
    Defaults = #mrargs{},
    {RevTerms, Mask, _} = lists:foldl(fun(Value, {Acc, Mask, Idx}) ->
        case element(Idx, Defaults) of
            Value ->
                {Acc, Mask, Idx + 1};
            _Default when Idx == #mrargs.bookmark ->
                {Acc, Mask, Idx + 1};
            _Default ->
                % Its `(Idx - 1)` because the initial `1`
                % value already accounts for one bit.
                {[Value | Acc], (1 bsl (Idx - 1)) bor Mask, Idx + 1}
        end
    end, {[], 0, 1}, tuple_to_list(Args0)),
    Terms = lists:reverse(RevTerms),
    TermBin = term_to_binary(Terms, [compressed, {minor_version, 2}]),
    MaskBin = binary:encode_unsigned(Mask),
    RawBookmark = <<?BOOKMARK_VSN, MaskBin/binary, TermBin/binary>>,
    couch_util:encodeBase64Url(RawBookmark).


bookmark_decode(Bookmark) ->
    try
        RawBin = couch_util:decodeBase64Url(Bookmark),
        <<?BOOKMARK_VSN, MaskBin:4/binary, TermBin/binary>> = RawBin,
        Mask = binary:decode_unsigned(MaskBin),
        Index = mask_to_index(Mask, 1, []),
        Terms = binary_to_term(TermBin, [safe]),
        lists:foldl(fun({Idx, Value}, Acc) ->
            setelement(Idx, Acc, Value)
        end, #mrargs{}, lists:zip(Index, Terms))
    catch _:_ ->
        throw({bad_request, <<"Invalid bookmark">>})
    end.


mask_to_index(0, _Pos, Acc) ->
    lists:reverse(Acc);
mask_to_index(Mask, Pos, Acc) when is_integer(Mask), Mask > 0 ->
    NewAcc = case Mask band 1 of
        0 -> Acc;
        1 -> [Pos | Acc]
    end,
    mask_to_index(Mask bsr 1, Pos + 1, NewAcc).


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

bookmark_encode_decode_test() ->
    ?assertEqual(
        #mrargs{page_size = 5},
        bookmark_decode(bookmark_encode(#mrargs{page_size = 5}))
    ),

    Randomized = lists:foldl(fun(Idx, Acc) ->
        if Idx == #mrargs.bookmark -> Acc; true ->
            setelement(Idx, Acc, couch_uuids:random())
        end
    end, #mrargs{}, lists:seq(1, record_info(size, mrargs))),

    ?assertEqual(
        Randomized,
        bookmark_decode(bookmark_encode(Randomized))
    ).


check_completion_test() ->
    ?assertEqual(
        {[], nil},
        check_completion(100, 1, [])
    ),
    ?assertEqual(
        {[1], nil},
        check_completion(100, 1, [1])
    ),
    ?assertEqual(
        {[1], 2},
        check_completion(100, 1, [1, 2])
    ),
    ?assertEqual(
        {[1], 2},
        check_completion(100, 1, [1, 2, 3])
    ),
    ?assertEqual(
        {[1, 2], nil},
        check_completion(100, 3, [1, 2])
    ),
    ?assertEqual(
        {[1, 2, 3], nil},
        check_completion(100, 3, [1, 2, 3])
    ),
    ?assertEqual(
        {[1, 2, 3], 4},
        check_completion(100, 3, [1, 2, 3, 4])
    ),
    ?assertEqual(
        {[1, 2, 3], 4},
        check_completion(100, 3, [1, 2, 3, 4, 5])
    ),
    ?assertEqual(
        {[1], nil},
        check_completion(1, 1, [1])
    ),
    ?assertEqual(
        {[1, 2], nil},
        check_completion(2, 3, [1, 2])
    ),
    ?assertEqual(
        {[1, 2], nil},
        check_completion(2, 3, [1, 2, 3])
    ),
    ?assertEqual(
        {[1, 2], nil},
        check_completion(2, 3, [1, 2, 3, 4, 5])
    ),
    ok.
-endif.
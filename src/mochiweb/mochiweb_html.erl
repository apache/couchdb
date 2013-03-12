%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc Loosely tokenizes and generates parse trees for HTML 4.
-module(mochiweb_html).
-export([tokens/1, parse/1, parse_tokens/1, to_tokens/1, escape/1,
         escape_attr/1, to_html/1]).

%% This is a macro to placate syntax highlighters..
-define(QUOTE, $\").
-define(SQUOTE, $\').
-define(ADV_COL(S, N),
        S#decoder{column=N+S#decoder.column,
                  offset=N+S#decoder.offset}).
-define(INC_COL(S),
        S#decoder{column=1+S#decoder.column,
                  offset=1+S#decoder.offset}).
-define(INC_LINE(S),
        S#decoder{column=1,
                  line=1+S#decoder.line,
                  offset=1+S#decoder.offset}).
-define(INC_CHAR(S, C),
        case C of
            $\n ->
                S#decoder{column=1,
                          line=1+S#decoder.line,
                          offset=1+S#decoder.offset};
            _ ->
                S#decoder{column=1+S#decoder.column,
                          offset=1+S#decoder.offset}
        end).

-define(IS_WHITESPACE(C),
        (C =:= $\s orelse C =:= $\t orelse C =:= $\r orelse C =:= $\n)).
-define(IS_LITERAL_SAFE(C),
        ((C >= $A andalso C =< $Z) orelse (C >= $a andalso C =< $z)
         orelse (C >= $0 andalso C =< $9))).
-define(PROBABLE_CLOSE(C),
        (C =:= $> orelse ?IS_WHITESPACE(C))).

-record(decoder, {line=1,
                  column=1,
                  offset=0}).

%% @type html_node() = {string(), [html_attr()], [html_node() | string()]}
%% @type html_attr() = {string(), string()}
%% @type html_token() = html_data() | start_tag() | end_tag() | inline_html() | html_comment() | html_doctype()
%% @type html_data() = {data, string(), Whitespace::boolean()}
%% @type start_tag() = {start_tag, Name, [html_attr()], Singleton::boolean()}
%% @type end_tag() = {end_tag, Name}
%% @type html_comment() = {comment, Comment}
%% @type html_doctype() = {doctype, [Doctype]}
%% @type inline_html() = {'=', iolist()}

%% External API.

%% @spec parse(string() | binary()) -> html_node()
%% @doc tokenize and then transform the token stream into a HTML tree.
parse(Input) ->
    parse_tokens(tokens(Input)).

%% @spec parse_tokens([html_token()]) -> html_node()
%% @doc Transform the output of tokens(Doc) into a HTML tree.
parse_tokens(Tokens) when is_list(Tokens) ->
    %% Skip over doctype, processing instructions
    F = fun (X) ->
                case X of
                    {start_tag, _, _, false} ->
                        false;
                    _ ->
                        true
                end
        end,
    [{start_tag, Tag, Attrs, false} | Rest] = lists:dropwhile(F, Tokens),
    {Tree, _} = tree(Rest, [norm({Tag, Attrs})]),
    Tree.

%% @spec tokens(StringOrBinary) -> [html_token()]
%% @doc Transform the input UTF-8 HTML into a token stream.
tokens(Input) ->
    tokens(iolist_to_binary(Input), #decoder{}, []).

%% @spec to_tokens(html_node()) -> [html_token()]
%% @doc Convert a html_node() tree to a list of tokens.
to_tokens({Tag0}) ->
    to_tokens({Tag0, [], []});
to_tokens(T={'=', _}) ->
    [T];
to_tokens(T={doctype, _}) ->
    [T];
to_tokens(T={comment, _}) ->
    [T];
to_tokens({Tag0, Acc}) ->
    %% This is only allowed in sub-tags: {p, [{"class", "foo"}]}
    to_tokens({Tag0, [], Acc});
to_tokens({Tag0, Attrs, Acc}) ->
    Tag = to_tag(Tag0),
    case is_singleton(Tag) of 
        true ->
            to_tokens([], [{start_tag, Tag, Attrs, true}]);
        false ->
            to_tokens([{Tag, Acc}], [{start_tag, Tag, Attrs, false}])
    end.

%% @spec to_html([html_token()] | html_node()) -> iolist()
%% @doc Convert a list of html_token() to a HTML document.
to_html(Node) when is_tuple(Node) ->
    to_html(to_tokens(Node));
to_html(Tokens) when is_list(Tokens) ->
    to_html(Tokens, []).

%% @spec escape(string() | atom() | binary()) -> binary()
%% @doc Escape a string such that it's safe for HTML (amp; lt; gt;).
escape(B) when is_binary(B) ->
    escape(binary_to_list(B), []);
escape(A) when is_atom(A) ->
    escape(atom_to_list(A), []);
escape(S) when is_list(S) ->
    escape(S, []).

%% @spec escape_attr(string() | binary() | atom() | integer() | float()) -> binary()
%% @doc Escape a string such that it's safe for HTML attrs
%%      (amp; lt; gt; quot;).
escape_attr(B) when is_binary(B) ->
    escape_attr(binary_to_list(B), []);
escape_attr(A) when is_atom(A) ->
    escape_attr(atom_to_list(A), []);
escape_attr(S) when is_list(S) ->
    escape_attr(S, []);
escape_attr(I) when is_integer(I) ->
    escape_attr(integer_to_list(I), []);
escape_attr(F) when is_float(F) ->
    escape_attr(mochinum:digits(F), []).

to_html([], Acc) ->
    lists:reverse(Acc);
to_html([{'=', Content} | Rest], Acc) ->
    to_html(Rest, [Content | Acc]);
to_html([{pi, Bin} | Rest], Acc) ->
    Open = [<<"<?">>,
            Bin,
            <<"?>">>],
    to_html(Rest, [Open | Acc]);
to_html([{pi, Tag, Attrs} | Rest], Acc) ->
    Open = [<<"<?">>,
            Tag,
            attrs_to_html(Attrs, []),
            <<"?>">>],
    to_html(Rest, [Open | Acc]);
to_html([{comment, Comment} | Rest], Acc) ->
    to_html(Rest, [[<<"<!--">>, Comment, <<"-->">>] | Acc]);
to_html([{doctype, Parts} | Rest], Acc) ->
    Inside = doctype_to_html(Parts, Acc),
    to_html(Rest, [[<<"<!DOCTYPE">>, Inside, <<">">>] | Acc]);
to_html([{data, Data, _Whitespace} | Rest], Acc) ->
    to_html(Rest, [escape(Data) | Acc]);
to_html([{start_tag, Tag, Attrs, Singleton} | Rest], Acc) ->
    Open = [<<"<">>,
            Tag,
            attrs_to_html(Attrs, []),
            case Singleton of
                true -> <<" />">>;
                false -> <<">">>
            end],
    to_html(Rest, [Open | Acc]);
to_html([{end_tag, Tag} | Rest], Acc) ->
    to_html(Rest, [[<<"</">>, Tag, <<">">>] | Acc]).

doctype_to_html([], Acc) ->
    lists:reverse(Acc);
doctype_to_html([Word | Rest], Acc) ->
    case lists:all(fun (C) -> ?IS_LITERAL_SAFE(C) end,
                   binary_to_list(iolist_to_binary(Word))) of
        true ->
            doctype_to_html(Rest, [[<<" ">>, Word] | Acc]);
        false ->
            doctype_to_html(Rest, [[<<" \"">>, escape_attr(Word), ?QUOTE] | Acc])
    end.

attrs_to_html([], Acc) ->
    lists:reverse(Acc);
attrs_to_html([{K, V} | Rest], Acc) ->
    attrs_to_html(Rest,
                  [[<<" ">>, escape(K), <<"=\"">>,
                    escape_attr(V), <<"\"">>] | Acc]).

escape([], Acc) ->
    list_to_binary(lists:reverse(Acc));
escape("<" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&lt;", Acc));
escape(">" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&gt;", Acc));
escape("&" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&amp;", Acc));
escape([C | Rest], Acc) ->
    escape(Rest, [C | Acc]).

escape_attr([], Acc) ->
    list_to_binary(lists:reverse(Acc));
escape_attr("<" ++ Rest, Acc) ->
    escape_attr(Rest, lists:reverse("&lt;", Acc));
escape_attr(">" ++ Rest, Acc) ->
    escape_attr(Rest, lists:reverse("&gt;", Acc));
escape_attr("&" ++ Rest, Acc) ->
    escape_attr(Rest, lists:reverse("&amp;", Acc));
escape_attr([?QUOTE | Rest], Acc) ->
    escape_attr(Rest, lists:reverse("&quot;", Acc));
escape_attr([C | Rest], Acc) ->
    escape_attr(Rest, [C | Acc]).

to_tag(A) when is_atom(A) ->
    norm(atom_to_list(A));
to_tag(L) ->
    norm(L).

to_tokens([], Acc) ->
    lists:reverse(Acc);
to_tokens([{Tag, []} | Rest], Acc) ->
    to_tokens(Rest, [{end_tag, to_tag(Tag)} | Acc]);
to_tokens([{Tag0, [{T0} | R1]} | Rest], Acc) ->
    %% Allow {br}
    to_tokens([{Tag0, [{T0, [], []} | R1]} | Rest], Acc);
to_tokens([{Tag0, [T0={'=', _C0} | R1]} | Rest], Acc) ->
    %% Allow {'=', iolist()}
    to_tokens([{Tag0, R1} | Rest], [T0 | Acc]);
to_tokens([{Tag0, [T0={comment, _C0} | R1]} | Rest], Acc) ->
    %% Allow {comment, iolist()}
    to_tokens([{Tag0, R1} | Rest], [T0 | Acc]);
to_tokens([{Tag0, [T0={pi, _S0} | R1]} | Rest], Acc) ->
    %% Allow {pi, binary()}
    to_tokens([{Tag0, R1} | Rest], [T0 | Acc]);
to_tokens([{Tag0, [T0={pi, _S0, _A0} | R1]} | Rest], Acc) ->
    %% Allow {pi, binary(), list()}
    to_tokens([{Tag0, R1} | Rest], [T0 | Acc]);
to_tokens([{Tag0, [{T0, A0=[{_, _} | _]} | R1]} | Rest], Acc) ->
    %% Allow {p, [{"class", "foo"}]}
    to_tokens([{Tag0, [{T0, A0, []} | R1]} | Rest], Acc);
to_tokens([{Tag0, [{T0, C0} | R1]} | Rest], Acc) ->
    %% Allow {p, "content"} and {p, <<"content">>}
    to_tokens([{Tag0, [{T0, [], C0} | R1]} | Rest], Acc);
to_tokens([{Tag0, [{T0, A1, C0} | R1]} | Rest], Acc) when is_binary(C0) ->
    %% Allow {"p", [{"class", "foo"}], <<"content">>}
    to_tokens([{Tag0, [{T0, A1, binary_to_list(C0)} | R1]} | Rest], Acc);
to_tokens([{Tag0, [{T0, A1, C0=[C | _]} | R1]} | Rest], Acc)
  when is_integer(C) ->
    %% Allow {"p", [{"class", "foo"}], "content"}
    to_tokens([{Tag0, [{T0, A1, [C0]} | R1]} | Rest], Acc);
to_tokens([{Tag0, [{T0, A1, C1} | R1]} | Rest], Acc) ->
    %% Native {"p", [{"class", "foo"}], ["content"]}
    Tag = to_tag(Tag0),
    T1 = to_tag(T0),
    case is_singleton(norm(T1)) of
        true ->
            to_tokens([{Tag, R1} | Rest], [{start_tag, T1, A1, true} | Acc]);
        false ->
            to_tokens([{T1, C1}, {Tag, R1} | Rest],
                      [{start_tag, T1, A1, false} | Acc])
    end;
to_tokens([{Tag0, [L | R1]} | Rest], Acc) when is_list(L) ->
    %% List text
    Tag = to_tag(Tag0),
    to_tokens([{Tag, R1} | Rest], [{data, iolist_to_binary(L), false} | Acc]);
to_tokens([{Tag0, [B | R1]} | Rest], Acc) when is_binary(B) ->
    %% Binary text
    Tag = to_tag(Tag0),
    to_tokens([{Tag, R1} | Rest], [{data, B, false} | Acc]).

tokens(B, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary>> ->
            lists:reverse(Acc);
        _ ->
            {Tag, S1} = tokenize(B, S),
            case parse_flag(Tag) of
                script ->
                    {Tag2, S2} = tokenize_script(B, S1),
                    tokens(B, S2, [Tag2, Tag | Acc]);
                textarea ->
                    {Tag2, S2} = tokenize_textarea(B, S1),
                    tokens(B, S2, [Tag2, Tag | Acc]);
                none ->
                    tokens(B, S1, [Tag | Acc])
            end
    end.

parse_flag({start_tag, B, _, false}) ->
    case string:to_lower(binary_to_list(B)) of
        "script" ->
            script;
        "textarea" ->
            textarea;
        _ ->
            none
    end;
parse_flag(_) ->
    none.

tokenize(B, S=#decoder{offset=O}) ->
    case B of
        <<_:O/binary, "<!--", _/binary>> ->
            tokenize_comment(B, ?ADV_COL(S, 4));
        <<_:O/binary, "<!DOCTYPE", _/binary>> ->
            tokenize_doctype(B, ?ADV_COL(S, 10));
        <<_:O/binary, "<![CDATA[", _/binary>> ->
            tokenize_cdata(B, ?ADV_COL(S, 9));
        <<_:O/binary, "<?php", _/binary>> ->
            {Body, S1} = raw_qgt(B, ?ADV_COL(S, 2)),
            {{pi, Body}, S1};
        <<_:O/binary, "<?", _/binary>> ->
            {Tag, S1} = tokenize_literal(B, ?ADV_COL(S, 2)),
            {Attrs, S2} = tokenize_attributes(B, S1),
            S3 = find_qgt(B, S2),
            {{pi, Tag, Attrs}, S3};
        <<_:O/binary, "&", _/binary>> ->
            tokenize_charref(B, ?INC_COL(S));
        <<_:O/binary, "</", _/binary>> ->
            {Tag, S1} = tokenize_literal(B, ?ADV_COL(S, 2)),
            {S2, _} = find_gt(B, S1),
            {{end_tag, Tag}, S2};
        <<_:O/binary, "<", C, _/binary>> 
                when ?IS_WHITESPACE(C); not ?IS_LITERAL_SAFE(C) ->
            %% This isn't really strict HTML
            {{data, Data, _Whitespace}, S1} = tokenize_data(B, ?INC_COL(S)),
            {{data, <<$<, Data/binary>>, false}, S1};
        <<_:O/binary, "<", _/binary>> ->
            {Tag, S1} = tokenize_literal(B, ?INC_COL(S)),
            {Attrs, S2} = tokenize_attributes(B, S1),
            {S3, HasSlash} = find_gt(B, S2),
            Singleton = HasSlash orelse is_singleton(Tag),
            {{start_tag, Tag, Attrs, Singleton}, S3};
        _ ->
            tokenize_data(B, S)
    end.

tree_data([{data, Data, Whitespace} | Rest], AllWhitespace, Acc) ->
    tree_data(Rest, (Whitespace andalso AllWhitespace), [Data | Acc]);
tree_data(Rest, AllWhitespace, Acc) ->
    {iolist_to_binary(lists:reverse(Acc)), AllWhitespace, Rest}.

tree([], Stack) ->
    {destack(Stack), []};
tree([{end_tag, Tag} | Rest], Stack) ->
    case destack(norm(Tag), Stack) of
        S when is_list(S) ->
            tree(Rest, S);
        Result ->
            {Result, []}
    end;
tree([{start_tag, Tag, Attrs, true} | Rest], S) ->
    tree(Rest, append_stack_child(norm({Tag, Attrs}), S));
tree([{start_tag, Tag, Attrs, false} | Rest], S) ->
    tree(Rest, stack(norm({Tag, Attrs}), S));
tree([T={pi, _Raw} | Rest], S) ->
    tree(Rest, append_stack_child(T, S));
tree([T={pi, _Tag, _Attrs} | Rest], S) ->
    tree(Rest, append_stack_child(T, S));
tree([T={comment, _Comment} | Rest], S) ->
    tree(Rest, append_stack_child(T, S));
tree(L=[{data, _Data, _Whitespace} | _], S) ->
    case tree_data(L, true, []) of
        {_, true, Rest} ->
            tree(Rest, S);
        {Data, false, Rest} ->
            tree(Rest, append_stack_child(Data, S))
    end;
tree([{doctype, _} | Rest], Stack) ->
    tree(Rest, Stack).

norm({Tag, Attrs}) ->
    {norm(Tag), [{norm(K), iolist_to_binary(V)} || {K, V} <- Attrs], []};
norm(Tag) when is_binary(Tag) ->
    Tag;
norm(Tag) ->
    list_to_binary(string:to_lower(Tag)).

stack(T1={TN, _, _}, Stack=[{TN, _, _} | _Rest])
  when TN =:= <<"li">> orelse TN =:= <<"option">> ->
    [T1 | destack(TN, Stack)];
stack(T1={TN0, _, _}, Stack=[{TN1, _, _} | _Rest])
  when (TN0 =:= <<"dd">> orelse TN0 =:= <<"dt">>) andalso
       (TN1 =:= <<"dd">> orelse TN1 =:= <<"dt">>) ->
    [T1 | destack(TN1, Stack)];
stack(T1, Stack) ->
    [T1 | Stack].

append_stack_child(StartTag, [{Name, Attrs, Acc} | Stack]) ->
    [{Name, Attrs, [StartTag | Acc]} | Stack].

destack(<<"br">>, Stack) ->
    %% This is an ugly hack to make dumb_br_test() pass,
    %% this makes it such that br can never have children.
    Stack;
destack(TagName, Stack) when is_list(Stack) ->
    F = fun (X) ->
                case X of
                    {TagName, _, _} ->
                        false;
                    _ ->
                        true
                end
        end,
    case lists:splitwith(F, Stack) of
        {_, []} ->
            %% If we're parsing something like XML we might find
            %% a <link>tag</link> that is normally a singleton
            %% in HTML but isn't here
            case {is_singleton(TagName), Stack} of
                {true, [{T0, A0, Acc0} | Post0]} ->
                    case lists:splitwith(F, Acc0) of
                        {_, []} ->
                            %% Actually was a singleton
                            Stack;
                        {Pre, [{T1, A1, Acc1} | Post1]} ->
                            [{T0, A0, [{T1, A1, Acc1 ++ lists:reverse(Pre)} | Post1]}
                             | Post0]
                    end;
                _ ->
                    %% No match, no state change
                    Stack
            end;
        {_Pre, [_T]} ->
            %% Unfurl the whole stack, we're done
            destack(Stack);
        {Pre, [T, {T0, A0, Acc0} | Post]} ->
            %% Unfurl up to the tag, then accumulate it
            [{T0, A0, [destack(Pre ++ [T]) | Acc0]} | Post]
    end.

destack([{Tag, Attrs, Acc}]) ->
    {Tag, Attrs, lists:reverse(Acc)};
destack([{T1, A1, Acc1}, {T0, A0, Acc0} | Rest]) ->
    destack([{T0, A0, [{T1, A1, lists:reverse(Acc1)} | Acc0]} | Rest]).

is_singleton(<<"br">>) -> true;
is_singleton(<<"hr">>) -> true;
is_singleton(<<"img">>) -> true;
is_singleton(<<"input">>) -> true;
is_singleton(<<"base">>) -> true;
is_singleton(<<"meta">>) -> true;
is_singleton(<<"link">>) -> true;
is_singleton(<<"area">>) -> true;
is_singleton(<<"param">>) -> true;
is_singleton(<<"col">>) -> true;
is_singleton(_) -> false.

tokenize_data(B, S=#decoder{offset=O}) ->
    tokenize_data(B, S, O, true).

tokenize_data(B, S=#decoder{offset=O}, Start, Whitespace) ->
    case B of
        <<_:O/binary, C, _/binary>> when (C =/= $< andalso C =/= $&) ->
            tokenize_data(B, ?INC_CHAR(S, C), Start,
                          (Whitespace andalso ?IS_WHITESPACE(C)));
        _ ->
            Len = O - Start,
            <<_:Start/binary, Data:Len/binary, _/binary>> = B,
            {{data, Data, Whitespace}, S}
    end.

tokenize_attributes(B, S) ->
    tokenize_attributes(B, S, []).

tokenize_attributes(B, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary>> ->
            {lists:reverse(Acc), S};
        <<_:O/binary, C, _/binary>> when (C =:= $> orelse C =:= $/) ->
            {lists:reverse(Acc), S};
        <<_:O/binary, "?>", _/binary>> ->
            {lists:reverse(Acc), S};
        <<_:O/binary, C, _/binary>> when ?IS_WHITESPACE(C) ->
            tokenize_attributes(B, ?INC_CHAR(S, C), Acc);
        _ ->
            {Attr, S1} = tokenize_literal(B, S),
            {Value, S2} = tokenize_attr_value(Attr, B, S1),
            tokenize_attributes(B, S2, [{Attr, Value} | Acc])
    end.

tokenize_attr_value(Attr, B, S) ->
    S1 = skip_whitespace(B, S),
    O = S1#decoder.offset,
    case B of
        <<_:O/binary, "=", _/binary>> ->
            S2 = skip_whitespace(B, ?INC_COL(S1)),
            tokenize_quoted_or_unquoted_attr_value(B, S2);
        _ ->
            {Attr, S1}
    end.

tokenize_quoted_or_unquoted_attr_value(B, S=#decoder{offset=O}) ->
    case B of
        <<_:O/binary>> ->
            { [], S };
        <<_:O/binary, Q, _/binary>> when Q =:= ?QUOTE orelse
                                         Q =:= ?SQUOTE ->
            tokenize_quoted_attr_value(B, ?INC_COL(S), [], Q);
        <<_:O/binary, _/binary>> ->
            tokenize_unquoted_attr_value(B, S, [])
    end.

tokenize_quoted_attr_value(B, S=#decoder{offset=O}, Acc, Q) ->
    case B of
        <<_:O/binary>> ->
            { iolist_to_binary(lists:reverse(Acc)), S };
        <<_:O/binary, $&, _/binary>> ->
            {{data, Data, false}, S1} = tokenize_charref(B, ?INC_COL(S)),
            tokenize_quoted_attr_value(B, S1, [Data|Acc], Q);
        <<_:O/binary, Q, _/binary>> ->
            { iolist_to_binary(lists:reverse(Acc)), ?INC_COL(S) };
        <<_:O/binary, C, _/binary>> ->
            tokenize_quoted_attr_value(B, ?INC_COL(S), [C|Acc], Q)
    end.

tokenize_unquoted_attr_value(B, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary>> ->
            { iolist_to_binary(lists:reverse(Acc)), S };
        <<_:O/binary, $&, _/binary>> ->
            {{data, Data, false}, S1} = tokenize_charref(B, ?INC_COL(S)),
            tokenize_unquoted_attr_value(B, S1, [Data|Acc]);
        <<_:O/binary, $/, $>, _/binary>> ->
            { iolist_to_binary(lists:reverse(Acc)), S };
        <<_:O/binary, C, _/binary>> when ?PROBABLE_CLOSE(C) ->
            { iolist_to_binary(lists:reverse(Acc)), S };
        <<_:O/binary, C, _/binary>> ->
            tokenize_unquoted_attr_value(B, ?INC_COL(S), [C|Acc])
    end.

skip_whitespace(B, S=#decoder{offset=O}) ->
    case B of
        <<_:O/binary, C, _/binary>> when ?IS_WHITESPACE(C) ->
            skip_whitespace(B, ?INC_CHAR(S, C));
        _ ->
            S
    end.

tokenize_literal(Bin, S=#decoder{offset=O}) ->
    case Bin of
        <<_:O/binary, C, _/binary>> when C =:= $>
                                    orelse C =:= $/
                                    orelse C =:= $= ->
            %% Handle case where tokenize_literal would consume
            %% 0 chars. http://github.com/mochi/mochiweb/pull/13
            {[C], ?INC_COL(S)};
        _ ->
            tokenize_literal(Bin, S, [])
    end.

tokenize_literal(Bin, S=#decoder{offset=O}, Acc) ->
    case Bin of
        <<_:O/binary, $&, _/binary>> ->
            {{data, Data, false}, S1} = tokenize_charref(Bin, ?INC_COL(S)),
            tokenize_literal(Bin, S1, [Data | Acc]);
        <<_:O/binary, C, _/binary>> when not (?IS_WHITESPACE(C)
                                              orelse C =:= $>
                                              orelse C =:= $/
                                              orelse C =:= $=) ->
            tokenize_literal(Bin, ?INC_COL(S), [C | Acc]);
        _ ->
            {iolist_to_binary(string:to_lower(lists:reverse(Acc))), S}
    end.

raw_qgt(Bin, S=#decoder{offset=O}) ->
    raw_qgt(Bin, S, O).

raw_qgt(Bin, S=#decoder{offset=O}, Start) ->
    case Bin of
        <<_:O/binary, "?>", _/binary>> ->
            Len = O - Start,
            <<_:Start/binary, Raw:Len/binary, _/binary>> = Bin,
            {Raw, ?ADV_COL(S, 2)};
        <<_:O/binary, C, _/binary>> ->
            raw_qgt(Bin, ?INC_CHAR(S, C), Start);
        <<_:O/binary>> ->
            <<_:Start/binary, Raw/binary>> = Bin,
            {Raw, S}
    end.

find_qgt(Bin, S=#decoder{offset=O}) ->
    case Bin of
        <<_:O/binary, "?>", _/binary>> ->
            ?ADV_COL(S, 2);
        <<_:O/binary, ">", _/binary>> ->
			?ADV_COL(S, 1);
        <<_:O/binary, "/>", _/binary>> ->
			?ADV_COL(S, 2);
        %% tokenize_attributes takes care of this state:
        %% <<_:O/binary, C, _/binary>> ->
        %%     find_qgt(Bin, ?INC_CHAR(S, C));
        <<_:O/binary>> ->
            S
    end.

find_gt(Bin, S) ->
    find_gt(Bin, S, false).

find_gt(Bin, S=#decoder{offset=O}, HasSlash) ->
    case Bin of
        <<_:O/binary, $/, _/binary>> ->
            find_gt(Bin, ?INC_COL(S), true);
        <<_:O/binary, $>, _/binary>> ->
            {?INC_COL(S), HasSlash};
        <<_:O/binary, C, _/binary>> ->
            find_gt(Bin, ?INC_CHAR(S, C), HasSlash);
        _ ->
            {S, HasSlash}
    end.

tokenize_charref(Bin, S=#decoder{offset=O}) ->
    try
        tokenize_charref(Bin, S, O)
    catch
        throw:invalid_charref ->
            {{data, <<"&">>, false}, S}
    end.

tokenize_charref(Bin, S=#decoder{offset=O}, Start) ->
    case Bin of
        <<_:O/binary>> ->
            throw(invalid_charref);
        <<_:O/binary, C, _/binary>> when ?IS_WHITESPACE(C)
                                         orelse C =:= ?SQUOTE
                                         orelse C =:= ?QUOTE
                                         orelse C =:= $/
                                         orelse C =:= $> ->
            throw(invalid_charref);
        <<_:O/binary, $;, _/binary>> ->
            Len = O - Start,
            <<_:Start/binary, Raw:Len/binary, _/binary>> = Bin,
            Data = case mochiweb_charref:charref(Raw) of
                       undefined ->
                           throw(invalid_charref);
                       Unichar when is_integer(Unichar) ->
                           mochiutf8:codepoint_to_bytes(Unichar);
                       Unichars when is_list(Unichars) ->
                           unicode:characters_to_binary(Unichars)
                   end,
            {{data, Data, false}, ?INC_COL(S)};
        _ ->
            tokenize_charref(Bin, ?INC_COL(S), Start)
    end.

tokenize_doctype(Bin, S) ->
    tokenize_doctype(Bin, S, []).

tokenize_doctype(Bin, S=#decoder{offset=O}, Acc) ->
    case Bin of
        <<_:O/binary>> ->
            {{doctype, lists:reverse(Acc)}, S};
        <<_:O/binary, $>, _/binary>> ->
            {{doctype, lists:reverse(Acc)}, ?INC_COL(S)};
        <<_:O/binary, C, _/binary>> when ?IS_WHITESPACE(C) ->
            tokenize_doctype(Bin, ?INC_CHAR(S, C), Acc);
        _ ->
            {Word, S1} = tokenize_word_or_literal(Bin, S),
            tokenize_doctype(Bin, S1, [Word | Acc])
    end.

tokenize_word_or_literal(Bin, S=#decoder{offset=O}) ->
    case Bin of
        <<_:O/binary, C, _/binary>> when C =:= ?QUOTE orelse C =:= ?SQUOTE ->
            tokenize_word(Bin, ?INC_COL(S), C);
        <<_:O/binary, C, _/binary>> when not ?IS_WHITESPACE(C) ->
            %% Sanity check for whitespace
            tokenize_literal(Bin, S)
    end.

tokenize_word(Bin, S, Quote) ->
    tokenize_word(Bin, S, Quote, []).

tokenize_word(Bin, S=#decoder{offset=O}, Quote, Acc) ->
    case Bin of
        <<_:O/binary>> ->
            {iolist_to_binary(lists:reverse(Acc)), S};
        <<_:O/binary, Quote, _/binary>> ->
            {iolist_to_binary(lists:reverse(Acc)), ?INC_COL(S)};
        <<_:O/binary, $&, _/binary>> ->
            {{data, Data, false}, S1} = tokenize_charref(Bin, ?INC_COL(S)),
            tokenize_word(Bin, S1, Quote, [Data | Acc]);
        <<_:O/binary, C, _/binary>> ->
            tokenize_word(Bin, ?INC_CHAR(S, C), Quote, [C | Acc])
    end.

tokenize_cdata(Bin, S=#decoder{offset=O}) ->
    tokenize_cdata(Bin, S, O).

tokenize_cdata(Bin, S=#decoder{offset=O}, Start) ->
    case Bin of
        <<_:O/binary, "]]>", _/binary>> ->
            Len = O - Start,
            <<_:Start/binary, Raw:Len/binary, _/binary>> = Bin,
            {{data, Raw, false}, ?ADV_COL(S, 3)};
        <<_:O/binary, C, _/binary>> ->
            tokenize_cdata(Bin, ?INC_CHAR(S, C), Start);
        _ ->
            <<_:O/binary, Raw/binary>> = Bin,
            {{data, Raw, false}, S}
    end.

tokenize_comment(Bin, S=#decoder{offset=O}) ->
    tokenize_comment(Bin, S, O).

tokenize_comment(Bin, S=#decoder{offset=O}, Start) ->
    case Bin of
        <<_:O/binary, "-->", _/binary>> ->
            Len = O - Start,
            <<_:Start/binary, Raw:Len/binary, _/binary>> = Bin,
            {{comment, Raw}, ?ADV_COL(S, 3)};
        <<_:O/binary, C, _/binary>> ->
            tokenize_comment(Bin, ?INC_CHAR(S, C), Start);
        <<_:Start/binary, Raw/binary>> ->
            {{comment, Raw}, S}
    end.

tokenize_script(Bin, S=#decoder{offset=O}) ->
    tokenize_script(Bin, S, O).

tokenize_script(Bin, S=#decoder{offset=O}, Start) ->
    case Bin of
        %% Just a look-ahead, we want the end_tag separately
        <<_:O/binary, $<, $/, SS, CC, RR, II, PP, TT, ZZ, _/binary>>
        when (SS =:= $s orelse SS =:= $S) andalso
             (CC =:= $c orelse CC =:= $C) andalso
             (RR =:= $r orelse RR =:= $R) andalso
             (II =:= $i orelse II =:= $I) andalso
             (PP =:= $p orelse PP =:= $P) andalso
             (TT=:= $t orelse TT =:= $T) andalso
             ?PROBABLE_CLOSE(ZZ) ->
            Len = O - Start,
            <<_:Start/binary, Raw:Len/binary, _/binary>> = Bin,
            {{data, Raw, false}, S};
        <<_:O/binary, C, _/binary>> ->
            tokenize_script(Bin, ?INC_CHAR(S, C), Start);
        <<_:Start/binary, Raw/binary>> ->
            {{data, Raw, false}, S}
    end.

tokenize_textarea(Bin, S=#decoder{offset=O}) ->
    tokenize_textarea(Bin, S, O).

tokenize_textarea(Bin, S=#decoder{offset=O}, Start) ->
    case Bin of
        %% Just a look-ahead, we want the end_tag separately
        <<_:O/binary, $<, $/, TT, EE, XX, TT2, AA, RR, EE2, AA2, ZZ, _/binary>>
        when (TT =:= $t orelse TT =:= $T) andalso
             (EE =:= $e orelse EE =:= $E) andalso
             (XX =:= $x orelse XX =:= $X) andalso
             (TT2 =:= $t orelse TT2 =:= $T) andalso
             (AA =:= $a orelse AA =:= $A) andalso
             (RR =:= $r orelse RR =:= $R) andalso
             (EE2 =:= $e orelse EE2 =:= $E) andalso
             (AA2 =:= $a orelse AA2 =:= $A) andalso
             ?PROBABLE_CLOSE(ZZ) ->
            Len = O - Start,
            <<_:Start/binary, Raw:Len/binary, _/binary>> = Bin,
            {{data, Raw, false}, S};
        <<_:O/binary, C, _/binary>> ->
            tokenize_textarea(Bin, ?INC_CHAR(S, C), Start);
        <<_:Start/binary, Raw/binary>> ->
            {{data, Raw, false}, S}
    end.


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

to_html_test() ->
    ?assertEqual(
       <<"<html><head><title>hey!</title></head><body><p class=\"foo\">what's up<br /></p><div>sucka</div>RAW!<!-- comment! --></body></html>">>,
       iolist_to_binary(
         to_html({html, [],
                  [{<<"head">>, [],
                    [{title, <<"hey!">>}]},
                   {body, [],
                    [{p, [{class, foo}], [<<"what's">>, <<" up">>, {br}]},
                     {'div', <<"sucka">>},
                     {'=', <<"RAW!">>},
                     {comment, <<" comment! ">>}]}]}))),
    ?assertEqual(
       <<"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">">>,
       iolist_to_binary(
         to_html({doctype,
                  [<<"html">>, <<"PUBLIC">>,
                   <<"-//W3C//DTD XHTML 1.0 Transitional//EN">>,
                   <<"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">>]}))),
    ?assertEqual(
       <<"<html><?xml:namespace prefix=\"o\" ns=\"urn:schemas-microsoft-com:office:office\"?></html>">>,
       iolist_to_binary(
         to_html({<<"html">>,[],
                  [{pi, <<"xml:namespace">>,
                    [{<<"prefix">>,<<"o">>},
                     {<<"ns">>,<<"urn:schemas-microsoft-com:office:office">>}]}]}))),
    ok.

escape_test() ->
    ?assertEqual(
       <<"&amp;quot;\"word &gt;&lt;&lt;up!&amp;quot;">>,
       escape(<<"&quot;\"word ><<up!&quot;">>)),
    ?assertEqual(
       <<"&amp;quot;\"word &gt;&lt;&lt;up!&amp;quot;">>,
       escape("&quot;\"word ><<up!&quot;")),
    ?assertEqual(
       <<"&amp;quot;\"word &gt;&lt;&lt;up!&amp;quot;">>,
       escape('&quot;\"word ><<up!&quot;')),
    ok.

escape_attr_test() ->
    ?assertEqual(
       <<"&amp;quot;&quot;word &gt;&lt;&lt;up!&amp;quot;">>,
       escape_attr(<<"&quot;\"word ><<up!&quot;">>)),
    ?assertEqual(
       <<"&amp;quot;&quot;word &gt;&lt;&lt;up!&amp;quot;">>,
       escape_attr("&quot;\"word ><<up!&quot;")),
    ?assertEqual(
       <<"&amp;quot;&quot;word &gt;&lt;&lt;up!&amp;quot;">>,
       escape_attr('&quot;\"word ><<up!&quot;')),
    ?assertEqual(
       <<"12345">>,
       escape_attr(12345)),
    ?assertEqual(
       <<"1.5">>,
       escape_attr(1.5)),
    ok.

tokens_test() ->
    ?assertEqual(
       [{start_tag, <<"foo">>, [{<<"bar">>, <<"baz">>},
                                {<<"wibble">>, <<"wibble">>},
                                {<<"alice">>, <<"bob">>}], true}],
       tokens(<<"<foo bar=baz wibble='wibble' alice=\"bob\"/>">>)),
    ?assertEqual(
       [{start_tag, <<"foo">>, [{<<"bar">>, <<"baz">>},
                                {<<"wibble">>, <<"wibble">>},
                                {<<"alice">>, <<"bob">>}], true}],
       tokens(<<"<foo bar=baz wibble='wibble' alice=bob/>">>)),
    ?assertEqual(
       [{comment, <<"[if lt IE 7]>\n<style type=\"text/css\">\n.no_ie { display: none; }\n</style>\n<![endif]">>}],
       tokens(<<"<!--[if lt IE 7]>\n<style type=\"text/css\">\n.no_ie { display: none; }\n</style>\n<![endif]-->">>)),
    ?assertEqual(
       [{start_tag, <<"script">>, [{<<"type">>, <<"text/javascript">>}], false},
        {data, <<" A= B <= C ">>, false},
        {end_tag, <<"script">>}],
       tokens(<<"<script type=\"text/javascript\"> A= B <= C </script>">>)),
    ?assertEqual(
       [{start_tag, <<"script">>, [{<<"type">>, <<"text/javascript">>}], false},
        {data, <<" A= B <= C ">>, false},
        {end_tag, <<"script">>}],
       tokens(<<"<script type =\"text/javascript\"> A= B <= C </script>">>)),
    ?assertEqual(
       [{start_tag, <<"script">>, [{<<"type">>, <<"text/javascript">>}], false},
        {data, <<" A= B <= C ">>, false},
        {end_tag, <<"script">>}],
       tokens(<<"<script type = \"text/javascript\"> A= B <= C </script>">>)),
    ?assertEqual(
       [{start_tag, <<"script">>, [{<<"type">>, <<"text/javascript">>}], false},
        {data, <<" A= B <= C ">>, false},
        {end_tag, <<"script">>}],
       tokens(<<"<script type= \"text/javascript\"> A= B <= C </script>">>)),
    ?assertEqual(
       [{start_tag, <<"textarea">>, [], false},
        {data, <<"<html></body>">>, false},
        {end_tag, <<"textarea">>}],
       tokens(<<"<textarea><html></body></textarea>">>)),
    ?assertEqual(
       [{start_tag, <<"textarea">>, [], false},
        {data, <<"<html></body></textareaz>">>, false}],
       tokens(<<"<textarea ><html></body></textareaz>">>)),
    ?assertEqual(
       [{pi, <<"xml:namespace">>,
         [{<<"prefix">>,<<"o">>},
          {<<"ns">>,<<"urn:schemas-microsoft-com:office:office">>}]}],
       tokens(<<"<?xml:namespace prefix=\"o\" ns=\"urn:schemas-microsoft-com:office:office\"?>">>)),
    ?assertEqual(
       [{pi, <<"xml:namespace">>,
         [{<<"prefix">>,<<"o">>},
          {<<"ns">>,<<"urn:schemas-microsoft-com:office:office">>}]}],
       tokens(<<"<?xml:namespace prefix=o ns=urn:schemas-microsoft-com:office:office \n?>">>)),
    ?assertEqual(
       [{pi, <<"xml:namespace">>,
         [{<<"prefix">>,<<"o">>},
          {<<"ns">>,<<"urn:schemas-microsoft-com:office:office">>}]}],
       tokens(<<"<?xml:namespace prefix=o ns=urn:schemas-microsoft-com:office:office">>)),
    ?assertEqual(
       [{data, <<"<">>, false}],
       tokens(<<"&lt;">>)),
    ?assertEqual(
       [{data, <<"not html ">>, false},
        {data, <<"< at all">>, false}],
       tokens(<<"not html < at all">>)),
    ok.

parse_test() ->
    D0 = <<"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">
<html>
 <head>
   <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">
   <title>Foo</title>
   <link rel=\"stylesheet\" type=\"text/css\" href=\"/static/rel/dojo/resources/dojo.css\" media=\"screen\">
   <link rel=\"stylesheet\" type=\"text/css\" href=\"/static/foo.css\" media=\"screen\">
   <!--[if lt IE 7]>
   <style type=\"text/css\">
     .no_ie { display: none; }
   </style>
   <![endif]-->
   <link rel=\"icon\" href=\"/static/images/favicon.ico\" type=\"image/x-icon\">
   <link rel=\"shortcut icon\" href=\"/static/images/favicon.ico\" type=\"image/x-icon\">
 </head>
 <body id=\"home\" class=\"tundra\"><![CDATA[&lt;<this<!-- is -->CDATA>&gt;]]></body>
</html>">>,
    ?assertEqual(
       {<<"html">>, [],
        [{<<"head">>, [],
          [{<<"meta">>,
            [{<<"http-equiv">>,<<"Content-Type">>},
             {<<"content">>,<<"text/html; charset=UTF-8">>}],
            []},
           {<<"title">>,[],[<<"Foo">>]},
           {<<"link">>,
            [{<<"rel">>,<<"stylesheet">>},
             {<<"type">>,<<"text/css">>},
             {<<"href">>,<<"/static/rel/dojo/resources/dojo.css">>},
             {<<"media">>,<<"screen">>}],
            []},
           {<<"link">>,
            [{<<"rel">>,<<"stylesheet">>},
             {<<"type">>,<<"text/css">>},
             {<<"href">>,<<"/static/foo.css">>},
             {<<"media">>,<<"screen">>}],
            []},
           {comment,<<"[if lt IE 7]>\n   <style type=\"text/css\">\n     .no_ie { display: none; }\n   </style>\n   <![endif]">>},
           {<<"link">>,
            [{<<"rel">>,<<"icon">>},
             {<<"href">>,<<"/static/images/favicon.ico">>},
             {<<"type">>,<<"image/x-icon">>}],
            []},
           {<<"link">>,
            [{<<"rel">>,<<"shortcut icon">>},
             {<<"href">>,<<"/static/images/favicon.ico">>},
             {<<"type">>,<<"image/x-icon">>}],
            []}]},
         {<<"body">>,
          [{<<"id">>,<<"home">>},
           {<<"class">>,<<"tundra">>}],
          [<<"&lt;<this<!-- is -->CDATA>&gt;">>]}]},
       parse(D0)),
    ?assertEqual(
       {<<"html">>,[],
        [{pi, <<"xml:namespace">>,
          [{<<"prefix">>,<<"o">>},
           {<<"ns">>,<<"urn:schemas-microsoft-com:office:office">>}]}]},
       parse(
         <<"<html><?xml:namespace prefix=\"o\" ns=\"urn:schemas-microsoft-com:office:office\"?></html>">>)),
    ?assertEqual(
       {<<"html">>, [],
        [{<<"dd">>, [], [<<"foo">>]},
         {<<"dt">>, [], [<<"bar">>]}]},
       parse(<<"<html><dd>foo<dt>bar</html>">>)),
    %% Singleton sadness
    ?assertEqual(
       {<<"html">>, [],
        [{<<"link">>, [], []},
         <<"foo">>,
         {<<"br">>, [], []},
         <<"bar">>]},
       parse(<<"<html><link>foo<br>bar</html>">>)),
    ?assertEqual(
       {<<"html">>, [],
        [{<<"link">>, [], [<<"foo">>,
                           {<<"br">>, [], []},
                           <<"bar">>]}]},
       parse(<<"<html><link>foo<br>bar</link></html>">>)),
    %% Case insensitive tags
    ?assertEqual(
       {<<"html">>, [],
        [{<<"head">>, [], [<<"foo">>,
                           {<<"br">>, [], []},
                           <<"BAR">>]},
         {<<"body">>, [{<<"class">>, <<"">>}, {<<"bgcolor">>, <<"#Aa01fF">>}], []}
        ]},
       parse(<<"<html><Head>foo<bR>BAR</head><body Class=\"\" bgcolor=\"#Aa01fF\"></BODY></html>">>)),
    ok.

exhaustive_is_singleton_test() ->
    T = mochiweb_cover:clause_lookup_table(?MODULE, is_singleton),
    [?assertEqual(V, is_singleton(K)) || {K, V} <- T].

tokenize_attributes_test() ->
    ?assertEqual(
       {<<"foo">>,
        [{<<"bar">>, <<"b\"az">>},
         {<<"wibble">>, <<"wibble">>},
         {<<"taco", 16#c2, 16#a9>>, <<"bell">>},
         {<<"quux">>, <<"quux">>}],
        []},
       parse(<<"<foo bar=\"b&quot;az\" wibble taco&copy;=bell quux">>)),
    ok.

tokens2_test() ->
    D0 = <<"<channel><title>from __future__ import *</title><link>http://bob.pythonmac.org</link><description>Bob's Rants</description></channel>">>,
    ?assertEqual(
       [{start_tag,<<"channel">>,[],false},
        {start_tag,<<"title">>,[],false},
        {data,<<"from __future__ import *">>,false},
        {end_tag,<<"title">>},
        {start_tag,<<"link">>,[],true},
        {data,<<"http://bob.pythonmac.org">>,false},
        {end_tag,<<"link">>},
        {start_tag,<<"description">>,[],false},
        {data,<<"Bob's Rants">>,false},
        {end_tag,<<"description">>},
        {end_tag,<<"channel">>}],
       tokens(D0)),
    ok.

to_tokens_test() ->
    ?assertEqual(
       [{start_tag, <<"p">>, [{class, 1}], false},
        {end_tag, <<"p">>}],
       to_tokens({p, [{class, 1}], []})),
    ?assertEqual(
       [{start_tag, <<"p">>, [], false},
        {end_tag, <<"p">>}],
       to_tokens({p})),
    ?assertEqual(
       [{'=', <<"data">>}],
       to_tokens({'=', <<"data">>})),
    ?assertEqual(
       [{comment, <<"comment">>}],
       to_tokens({comment, <<"comment">>})),
    %% This is only allowed in sub-tags:
    %% {p, [{"class", "foo"}]} as {p, [{"class", "foo"}], []}
    %% On the outside it's always treated as follows:
    %% {p, [], [{"class", "foo"}]} as {p, [], [{"class", "foo"}]}
    ?assertEqual(
       [{start_tag, <<"html">>, [], false},
        {start_tag, <<"p">>, [{class, 1}], false},
        {end_tag, <<"p">>},
        {end_tag, <<"html">>}],
       to_tokens({html, [{p, [{class, 1}]}]})),
    ok.

parse2_test() ->
    D0 = <<"<channel><title>from __future__ import *</title><link>http://bob.pythonmac.org<br>foo</link><description>Bob's Rants</description></channel>">>,
    ?assertEqual(
       {<<"channel">>,[],
        [{<<"title">>,[],[<<"from __future__ import *">>]},
         {<<"link">>,[],[
                         <<"http://bob.pythonmac.org">>,
                         {<<"br">>,[],[]},
                         <<"foo">>]},
         {<<"description">>,[],[<<"Bob's Rants">>]}]},
       parse(D0)),
    ok.

parse_tokens_test() ->
    D0 = [{doctype,[<<"HTML">>,<<"PUBLIC">>,<<"-//W3C//DTD HTML 4.01 Transitional//EN">>]},
          {data,<<"\n">>,true},
          {start_tag,<<"html">>,[],false}],
    ?assertEqual(
       {<<"html">>, [], []},
       parse_tokens(D0)),
    D1 = D0 ++ [{end_tag, <<"html">>}],
    ?assertEqual(
       {<<"html">>, [], []},
       parse_tokens(D1)),
    D2 = D0 ++ [{start_tag, <<"body">>, [], false}],
    ?assertEqual(
       {<<"html">>, [], [{<<"body">>, [], []}]},
       parse_tokens(D2)),
    D3 = D0 ++ [{start_tag, <<"head">>, [], false},
                {end_tag, <<"head">>},
                {start_tag, <<"body">>, [], false}],
    ?assertEqual(
       {<<"html">>, [], [{<<"head">>, [], []}, {<<"body">>, [], []}]},
       parse_tokens(D3)),
    D4 = D3 ++ [{data,<<"\n">>,true},
                {start_tag,<<"div">>,[{<<"class">>,<<"a">>}],false},
                {start_tag,<<"a">>,[{<<"name">>,<<"#anchor">>}],false},
                {end_tag,<<"a">>},
                {end_tag,<<"div">>},
                {start_tag,<<"div">>,[{<<"class">>,<<"b">>}],false},
                {start_tag,<<"div">>,[{<<"class">>,<<"c">>}],false},
                {end_tag,<<"div">>},
                {end_tag,<<"div">>}],
    ?assertEqual(
       {<<"html">>, [],
        [{<<"head">>, [], []},
         {<<"body">>, [],
          [{<<"div">>, [{<<"class">>, <<"a">>}], [{<<"a">>, [{<<"name">>, <<"#anchor">>}], []}]},
           {<<"div">>, [{<<"class">>, <<"b">>}], [{<<"div">>, [{<<"class">>, <<"c">>}], []}]}
          ]}]},
       parse_tokens(D4)),
    D5 = [{start_tag,<<"html">>,[],false},
          {data,<<"\n">>,true},
          {data,<<"boo">>,false},
          {data,<<"hoo">>,false},
          {data,<<"\n">>,true},
          {end_tag,<<"html">>}],
    ?assertEqual(
       {<<"html">>, [], [<<"\nboohoo\n">>]},
       parse_tokens(D5)),
    D6 = [{start_tag,<<"html">>,[],false},
          {data,<<"\n">>,true},
          {data,<<"\n">>,true},
          {end_tag,<<"html">>}],
    ?assertEqual(
       {<<"html">>, [], []},
       parse_tokens(D6)),
    D7 = [{start_tag,<<"html">>,[],false},
          {start_tag,<<"ul">>,[],false},
          {start_tag,<<"li">>,[],false},
          {data,<<"word">>,false},
          {start_tag,<<"li">>,[],false},
          {data,<<"up">>,false},
          {end_tag,<<"li">>},
          {start_tag,<<"li">>,[],false},
          {data,<<"fdsa">>,false},
          {start_tag,<<"br">>,[],true},
          {data,<<"asdf">>,false},
          {end_tag,<<"ul">>},
          {end_tag,<<"html">>}],
    ?assertEqual(
       {<<"html">>, [],
        [{<<"ul">>, [],
          [{<<"li">>, [], [<<"word">>]},
           {<<"li">>, [], [<<"up">>]},
           {<<"li">>, [], [<<"fdsa">>,{<<"br">>, [], []}, <<"asdf">>]}]}]},
       parse_tokens(D7)),
    ok.

destack_test() ->
    {<<"a">>, [], []} =
        destack([{<<"a">>, [], []}]),
    {<<"a">>, [], [{<<"b">>, [], []}]} =
        destack([{<<"b">>, [], []}, {<<"a">>, [], []}]),
    {<<"a">>, [], [{<<"b">>, [], [{<<"c">>, [], []}]}]} =
     destack([{<<"c">>, [], []}, {<<"b">>, [], []}, {<<"a">>, [], []}]),
    [{<<"a">>, [], [{<<"b">>, [], [{<<"c">>, [], []}]}]}] =
     destack(<<"b">>,
             [{<<"c">>, [], []}, {<<"b">>, [], []}, {<<"a">>, [], []}]),
    [{<<"b">>, [], [{<<"c">>, [], []}]}, {<<"a">>, [], []}] =
     destack(<<"c">>,
             [{<<"c">>, [], []}, {<<"b">>, [], []},{<<"a">>, [], []}]),
    ok.

doctype_test() ->
    ?assertEqual(
       {<<"html">>,[],[{<<"head">>,[],[]}]},
       mochiweb_html:parse("<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"
                           "<html><head></head></body></html>")),
    %% http://code.google.com/p/mochiweb/issues/detail?id=52
    ?assertEqual(
       {<<"html">>,[],[{<<"head">>,[],[]}]},
       mochiweb_html:parse("<html>"
                           "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"
                           "<head></head></body></html>")),
    %% http://github.com/mochi/mochiweb/pull/13
    ?assertEqual(
       {<<"html">>,[],[{<<"head">>,[],[]}]},
       mochiweb_html:parse("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\"/>"
                           "<html>"
                           "<head></head></body></html>")),
    ok.

dumb_br_test() ->
    %% http://code.google.com/p/mochiweb/issues/detail?id=71
    ?assertEqual(
       {<<"div">>,[],[{<<"br">>, [], []}, {<<"br">>, [], []}, <<"z">>]},
       mochiweb_html:parse("<div><br/><br/>z</br/></br/></div>")),
    ?assertEqual(
       {<<"div">>,[],[{<<"br">>, [], []}, {<<"br">>, [], []}, <<"z">>]},
       mochiweb_html:parse("<div><br><br>z</br/></br/></div>")),
    ?assertEqual(
       {<<"div">>,[],[{<<"br">>, [], []}, {<<"br">>, [], []}, <<"z">>, {<<"br">>, [], []}, {<<"br">>, [], []}]},
       mochiweb_html:parse("<div><br><br>z<br/><br/></div>")),
    ?assertEqual(
       {<<"div">>,[],[{<<"br">>, [], []}, {<<"br">>, [], []}, <<"z">>]},
       mochiweb_html:parse("<div><br><br>z</br></br></div>")).


php_test() ->
    %% http://code.google.com/p/mochiweb/issues/detail?id=71
    ?assertEqual(
       [{pi, <<"php\n">>}],
       mochiweb_html:tokens(
         "<?php\n?>")),
    ?assertEqual(
       {<<"div">>, [], [{pi, <<"php\n">>}]},
       mochiweb_html:parse(
         "<div><?php\n?></div>")),
    ok.

parse_unquoted_attr_test() ->
    D0 = <<"<html><img src=/images/icon.png/></html>">>,
    ?assertEqual(
        {<<"html">>,[],[
            { <<"img">>, [ { <<"src">>, <<"/images/icon.png">> } ], [] }
        ]},
        mochiweb_html:parse(D0)),

    D1 = <<"<html><img src=/images/icon.png></img></html>">>,
        ?assertEqual(
            {<<"html">>,[],[
                { <<"img">>, [ { <<"src">>, <<"/images/icon.png">> } ], [] }
            ]},
            mochiweb_html:parse(D1)),

    D2 = <<"<html><img src=/images/icon&gt;.png width=100></img></html>">>,
        ?assertEqual(
            {<<"html">>,[],[
                { <<"img">>, [ { <<"src">>, <<"/images/icon>.png">> }, { <<"width">>, <<"100">> } ], [] }
            ]},
            mochiweb_html:parse(D2)),
    ok.

parse_quoted_attr_test() ->
    D0 = <<"<html><img src='/images/icon.png'></html>">>,
    ?assertEqual(
        {<<"html">>,[],[
            { <<"img">>, [ { <<"src">>, <<"/images/icon.png">> } ], [] }
        ]},
        mochiweb_html:parse(D0)),

    D1 = <<"<html><img src=\"/images/icon.png'></html>">>,
    ?assertEqual(
        {<<"html">>,[],[
            { <<"img">>, [ { <<"src">>, <<"/images/icon.png'></html>">> } ], [] }
        ]},
        mochiweb_html:parse(D1)),

    D2 = <<"<html><img src=\"/images/icon&gt;.png\"></html>">>,
    ?assertEqual(
        {<<"html">>,[],[
            { <<"img">>, [ { <<"src">>, <<"/images/icon>.png">> } ], [] }
        ]},
        mochiweb_html:parse(D2)),

    %% Quoted attributes can contain whitespace and newlines
    D3 = <<"<html><a href=\"#\" onclick=\"javascript: test(1,\ntrue);\"></html>">>,
    ?assertEqual(
        {<<"html">>,[],[
            { <<"a">>, [ { <<"href">>, <<"#">> }, {<<"onclick">>, <<"javascript: test(1,\ntrue);">>} ], [] }
        ]},
        mochiweb_html:parse(D3)),     
    ok.

parse_missing_attr_name_test() ->
    D0 = <<"<html =black></html>">>,
    ?assertEqual(
        {<<"html">>, [ { <<"=">>, <<"=">> }, { <<"black">>, <<"black">> } ], [] },
       mochiweb_html:parse(D0)),
    ok.

parse_broken_pi_test() ->
	D0 = <<"<html><?xml:namespace prefix = o ns = \"urn:schemas-microsoft-com:office:office\" /></html>">>,
	?assertEqual(
		{<<"html">>, [], [
			{ pi, <<"xml:namespace">>, [ { <<"prefix">>, <<"o">> },
			                             { <<"ns">>, <<"urn:schemas-microsoft-com:office:office">> } ] }
		] },
		mochiweb_html:parse(D0)),
	ok.

parse_funny_singletons_test() ->
	D0 = <<"<html><input><input>x</input></input></html>">>,
	?assertEqual(
		{<<"html">>, [], [
			{ <<"input">>, [], [] },
			{ <<"input">>, [], [ <<"x">> ] }
		] },
		mochiweb_html:parse(D0)),
	ok.

to_html_singleton_test() ->
    D0 = <<"<link />">>,
    T0 = {<<"link">>,[],[]},
    ?assertEqual(D0, iolist_to_binary(to_html(T0))),

    D1 = <<"<head><link /></head>">>,
    T1 = {<<"head">>,[],[{<<"link">>,[],[]}]},
    ?assertEqual(D1, iolist_to_binary(to_html(T1))),

    D2 = <<"<head><link /><link /></head>">>,
    T2 = {<<"head">>,[],[{<<"link">>,[],[]}, {<<"link">>,[],[]}]},
    ?assertEqual(D2, iolist_to_binary(to_html(T2))),

    %% Make sure singletons are converted to singletons.
    D3 = <<"<head><link /></head>">>,
    T3 = {<<"head">>,[],[{<<"link">>,[],[<<"funny">>]}]},
    ?assertEqual(D3, iolist_to_binary(to_html(T3))),

    D4 = <<"<link />">>,
    T4 = {<<"link">>,[],[<<"funny">>]},
    ?assertEqual(D4, iolist_to_binary(to_html(T4))),

    ok.

parse_amp_test_() ->
    [?_assertEqual(
       {<<"html">>,[],
        [{<<"body">>,[{<<"onload">>,<<"javascript:A('1&2')">>}],[]}]},
       mochiweb_html:parse("<html><body onload=\"javascript:A('1&2')\"></body></html>")),
     ?_assertEqual(
        {<<"html">>,[],
         [{<<"body">>,[{<<"onload">>,<<"javascript:A('1& 2')">>}],[]}]},
        mochiweb_html:parse("<html><body onload=\"javascript:A('1& 2')\"></body></html>")),
     ?_assertEqual(
        {<<"html">>,[],
         [{<<"body">>,[],[<<"& ">>]}]},
        mochiweb_html:parse("<html><body>& </body></html>")),
     ?_assertEqual(
        {<<"html">>,[],
         [{<<"body">>,[],[<<"&">>]}]},
        mochiweb_html:parse("<html><body>&</body></html>"))].

parse_unescaped_lt_test() ->
    D1 = <<"<div> < < <a href=\"/\">Back</a></div>">>,
    ?assertEqual(
        {<<"div">>, [], [<<" < < ">>, {<<"a">>, [{<<"href">>, <<"/">>}], 
                                       [<<"Back">>]}]},
        mochiweb_html:parse(D1)),

    D2 = <<"<div> << <a href=\"/\">Back</a></div>">>,
    ?assertEqual(
        {<<"div">>, [], [<<" << ">>, {<<"a">>, [{<<"href">>, <<"/">>}], 
                                      [<<"Back">>]}]},
    mochiweb_html:parse(D2)).

-endif.

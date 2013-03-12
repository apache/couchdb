%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2008 Mochi Media, Inc.

%% @doc Template module for a mochifmt formatter.

-module(mochifmt_std).
-author('bob@mochimedia.com').
-export([new/0, format/3, get_value/3, format_field/3, get_field/3, convert_field/3]).

new() ->
    {?MODULE}.

format(Format, Args, {?MODULE}=THIS) ->
    mochifmt:format(Format, Args, THIS).

get_field(Key, Args, {?MODULE}=THIS) ->
    mochifmt:get_field(Key, Args, THIS).

convert_field(Key, Args, {?MODULE}) ->
    mochifmt:convert_field(Key, Args).

get_value(Key, Args, {?MODULE}) ->
    mochifmt:get_value(Key, Args).

format_field(Arg, Format, {?MODULE}=THIS) ->
    mochifmt:format_field(Arg, Format, THIS).

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

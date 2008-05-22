%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2008 Mochi Media, Inc.

%% @doc Template module for a mochifmt formatter.

-module(mochifmt_std, []).
-author('bob@mochimedia.com').
-export([format/2, get_value/2, format_field/2, get_field/2, convert_field/2]).

format(Format, Args) ->
    mochifmt:format(Format, Args, THIS).

get_field(Key, Args) ->
    mochifmt:get_field(Key, Args, THIS).

convert_field(Key, Args) ->
    mochifmt:convert_field(Key, Args).

get_value(Key, Args) ->
    mochifmt:get_value(Key, Args).

format_field(Arg, Format) ->
    mochifmt:format_field(Arg, Format, THIS).

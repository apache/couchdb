%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2008 Mochi Media, Inc.

%% @doc Formatter that understands records.
%%
%% Usage:
%%
%%    1> M = mochifmt_records:new([{rec, record_info(fields, rec)}]),
%%    M:format("{0.bar}", [#rec{bar=foo}]).
%%    foo

-module(mochifmt_records, [Recs]).
-author('bob@mochimedia.com').
-export([get_value/2]).

get_value(Key, Rec) when is_tuple(Rec) and is_atom(element(1, Rec)) ->
    try begin
            Atom = list_to_existing_atom(Key),
            {_, Fields} = proplists:lookup(element(1, Rec), Recs),
            element(get_rec_index(Atom, Fields, 2), Rec)
        end
    catch error:_ -> mochifmt:get_value(Key, Rec)
    end;
get_value(Key, Args) ->
    mochifmt:get_value(Key, Args).

get_rec_index(Atom, [Atom | _], Index) ->
    Index;
get_rec_index(Atom, [_ | Rest], Index) ->
    get_rec_index(Atom, Rest, 1 + Index).

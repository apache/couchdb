%%%-------------------------------------------------------------------
%%% File    : tft_binary.erl
%%% Author  : Hakan Mattsson <hakan@erix.ericsson.se>
%%% Description : 
%%%
%%% Created : 24 May 2004 by Hakan Mattsson <hakan@erix.ericsson.se>
%%%-------------------------------------------------------------------

-module(tftp_binary).

%%%-------------------------------------------------------------------
%%% Interface
%%%-------------------------------------------------------------------

-behaviour(tftp).

-export([prepare/6, open/6, read/1, write/2, abort/3]).
-export([prepare/5, open/5]).

-record(read_state,  {options, blksize, bin,  is_network_ascii, count}).
-record(write_state, {options, blksize, list, is_network_ascii}).

%%-------------------------------------------------------------------
%% Prepare
%%-------------------------------------------------------------------

prepare(_Peer, Access, Filename, Mode, SuggestedOptions, Initial) ->
    %% Kept for backwards compatibility 
    prepare(Access, Filename, Mode, SuggestedOptions, Initial).

prepare(Access, Bin, Mode, SuggestedOptions, []) ->
    %% Client side
    case catch handle_options(Access, Bin, Mode, SuggestedOptions) of
	{ok, IsNetworkAscii, AcceptedOptions} when Access =:= read, binary(Bin) ->
	    State = #read_state{options  	 = AcceptedOptions,
				blksize  	 = lookup_blksize(AcceptedOptions),
				bin      	 = Bin,
				is_network_ascii = IsNetworkAscii,
			        count            = size(Bin)},
	    {ok, AcceptedOptions, State};
	{ok, IsNetworkAscii, AcceptedOptions} when Access =:= write, Bin =:= binary ->
	    State = #write_state{options  	  = AcceptedOptions,
				 blksize  	  = lookup_blksize(AcceptedOptions),
				 list     	  = [],
				 is_network_ascii = IsNetworkAscii},
	    {ok, AcceptedOptions, State};
	{error, {Code, Text}} ->
	    {error, {Code, Text}}
    end;
prepare(_Access, _Bin, _Mode, _SuggestedOptions, _Initial) ->
    {error, {undef, "Illegal callback options."}}.

%%-------------------------------------------------------------------
%% Open
%%-------------------------------------------------------------------

open(_Peer, Access, Filename, Mode, SuggestedOptions, Initial) ->
    %% Kept for backwards compatibility 
    open(Access, Filename, Mode, SuggestedOptions, Initial).

open(Access, Bin, Mode, SuggestedOptions, []) ->
    %% Server side
    case prepare(Access, Bin, Mode, SuggestedOptions, []) of
	{ok, AcceptedOptions, State} ->
	    open(Access, Bin, Mode, AcceptedOptions, State);
	{error, {Code, Text}} ->
	    {error, {Code, Text}}
    end;
open(Access, Bin, Mode, NegotiatedOptions, State) ->
    %% Both sides
    IsNetworkAscii =
	if
	    is_record(State, write_state) -> State#write_state.is_network_ascii;
	    is_record(State, read_state)  -> State#read_state.is_network_ascii
	end,
    case catch handle_options(Access, Bin, Mode, NegotiatedOptions) of
	{ok, IsNetworkAscii2, Options}
	when Options =:= NegotiatedOptions,
	     IsNetworkAscii =:= IsNetworkAscii2 ->
	    {ok, NegotiatedOptions, State};
	{error, {Code, Text}} ->
	    {error, {Code, Text}}
    end.

%%-------------------------------------------------------------------
%% Read
%%-------------------------------------------------------------------

read(#read_state{bin = Bin} = State) when is_binary(Bin) ->
    BlkSize = State#read_state.blksize,
    if
	size(Bin) >= BlkSize ->
	    <<Block:BlkSize/binary, Bin2/binary>> = Bin,
	    State2 = State#read_state{bin = Bin2},
	    {more, Block, State2};
	size(Bin) < BlkSize ->
	    {last, Bin, State#read_state.count}
    end.

%%-------------------------------------------------------------------
%% Write
%%-------------------------------------------------------------------

write(Bin, #write_state{list = List} = State) when is_binary(Bin), is_list(List) ->
    Size = size(Bin),
    BlkSize = State#write_state.blksize,
    if
	Size =:= BlkSize ->
	    {more, State#write_state{list = [Bin | List]}};
	Size < BlkSize ->
	    Bin2 = list_to_binary(lists:reverse([Bin | List])),
	    {last, Bin2}
    end.

%%-------------------------------------------------------------------
%% Abort
%%-------------------------------------------------------------------

abort(_Code, _Text, #read_state{bin = Bin} = State) 
  when record(State, read_state), binary(Bin) ->
    ok;
abort(_Code, _Text, #write_state{list = List} = State)
  when record(State, write_state), list(List) ->
    ok.

%%-------------------------------------------------------------------
%% Process options
%%-------------------------------------------------------------------

handle_options(Access, Bin, Mode, Options) ->
    IsNetworkAscii = handle_mode(Mode),
    Options2 = do_handle_options(Access, Bin, Options),
    {ok, IsNetworkAscii, Options2}.

handle_mode(Mode) ->
    case Mode of
	%% "netascii" -> true;
	"octet"    -> false;
	_          -> throw({error, {badop, "Illegal mode " ++ Mode}})
    end.

do_handle_options(Access, Bin, [{Key, Val} | T]) ->
    case Key of
	"tsize" ->
	    case Access of
		read when Val =:= "0", binary(Bin) ->
		    Tsize = integer_to_list(size(Bin)),
		    [{Key, Tsize} | do_handle_options(Access, Bin, T)];
		_ ->
		    handle_integer(Access, Bin, Key, Val, T, 0, infinity)
	    end;
	"blksize" ->
	    handle_integer(Access, Bin, Key, Val, T, 8, 65464);
	"timeout" ->
	    handle_integer(Access, Bin, Key, Val, T, 1, 255);
	_ ->
	    do_handle_options(Access, Bin, T)
    end;
do_handle_options(_Access, _Bin, []) ->
    [].


handle_integer(Access, Bin, Key, Val, Options, Min, Max) ->
    case catch list_to_integer(Val) of
	{'EXIT', _} ->
	    do_handle_options(Access, Bin, Options);
	Int when Int >= Min, Int =< Max ->
	    [{Key, Val} | do_handle_options(Access, Bin, Options)];
	Int when Int >= Min, Max =:= infinity ->
	    [{Key, Val} | do_handle_options(Access, Bin, Options)];
	_Int ->
	    throw({error, {badopt, "Illegal " ++ Key ++ " value " ++ Val}})
    end.

lookup_blksize(Options) ->
    case lists:keysearch("blksize", 1, Options) of
	{value, {_, Val}} ->
	    list_to_integer(Val);
	false ->
	    512
    end.

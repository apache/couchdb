%%%-------------------------------------------------------------------
%%% File    : tft_file.erl
%%% Author  : Hakan Mattsson <hakan@erix.ericsson.se>
%%% Description : 
%%%
%%% Created : 24 May 2004 by Hakan Mattsson <hakan@erix.ericsson.se>
%%%-------------------------------------------------------------------

-module(tftp_file).

%%%-------------------------------------------------------------------
%%% Interface
%%%-------------------------------------------------------------------

-behaviour(tftp).

-export([prepare/6, open/6, read/1, write/2, abort/3]).
-export([prepare/5, open/5]).

%%%-------------------------------------------------------------------
%%% Defines
%%%-------------------------------------------------------------------

-include_lib("kernel/include/file.hrl").

-record(state, {access,
		filename,
		root_dir,
		options,
		blksize,
		fd,
		count,
		buffer}).

%%-------------------------------------------------------------------
%% prepare(Peer, Access, Filename, Mode, SuggestedOptions, InitialState) -> 
%%    {ok, AcceptedOptions, NewState} | {error, Code, Text}
%%
%% Peer             = {PeerType, PeerHost, PeerPort}
%% PeerType         = inet | inet6
%% PeerHost         = ip_address()
%% PeerPort         = integer()
%% Acess            = read | write
%% Filename         = string()
%% Mode             = string()
%% SuggestedOptions = [{Key, Value}]
%% AcceptedOptions  = [{Key, Value}]
%% Key              = string()
%% Value            = string()
%% InitialState     = [] | [{root_dir, string()}]
%% NewState         = term()
%% Code             = undef | enoent | eacces | enospc |
%%                    badop | eexist | baduser | badopt |
%%                    integer()
%% Text             = string()
%%
%% Prepares open of a file on the client side.
%% 
%% Will be followed by a call to open/4 before any read/write access
%% is performed. The AcceptedOptions will be sent to the server which
%% will reply with those options that it accepts. The options that are
%% accepted by the server will be forwarded to open/4 as SuggestedOptions.
%%
%% No new options may be added, but the ones that are present as
%% SuggestedOptions may be omitted or replaced with new values
%% in the AcceptedOptions.
%%-------------------------------------------------------------------

prepare(_Peer, Access, Filename, Mode, SuggestedOptions, Initial) ->
    %% Kept for backwards compatibility 
    prepare(Access, Filename, Mode, SuggestedOptions, Initial).

prepare(Access, Filename, Mode, SuggestedOptions, Initial) when is_list(Initial) ->
    %% Client side
    case catch handle_options(Access, Filename, Mode, SuggestedOptions, Initial) of
	{ok, Filename2, AcceptedOptions} ->
	    State = #state{access           = Access,
			   filename         = Filename2,
			   options  	    = AcceptedOptions,
			   blksize  	    = lookup_blksize(AcceptedOptions),
			   count    	    = 0,
			   buffer   	    = []},
	    {ok, AcceptedOptions, State};
	{error, {Code, Text}} ->
	    {error, {Code, Text}}
    end.

%% ---------------------------------------------------------
%% open(Peer, Access, Filename, Mode, SuggestedOptions, State) -> 
%%    {ok, AcceptedOptions, NewState} | {error, Code, Text}
%%
%% Peer             = {PeerType, PeerHost, PeerPort}
%% PeerType         = inet | inet6
%% PeerHost         = ip_address()
%% PeerPort         = integer()
%% Acess            = read | write
%% Filename         = string()
%% Mode             = string()
%% SuggestedOptions = [{Key, Value}]
%% AcceptedOptions  = [{Key, Value}]
%% Key              = string()
%% Value            = string()
%% State            = InitialState | #state{}
%% InitialState     = [] | [{root_dir, string()}]
%% NewState         = term()
%% Code             = undef | enoent | eacces  | enospc |
%%                    badop | eexist | baduser | badopt |
%%                    integer()
%% Text             = string()
%%
%% Opens a file for read or write access.
%% 
%% On the client side where the open/4 call has been preceeded by a
%% call to prepare/4, all options must be accepted or rejected.
%% On the server side, where there are no preceeding prepare/4 call,
%% noo new options may be added, but the ones that are present as
%% SuggestedOptions may be omitted or replaced with new values
%% in the AcceptedOptions.
%%-------------------------------------------------------------------

open(_Peer, Access, Filename, Mode, SuggestedOptions, Initial) ->
    %% Kept for backwards compatibility 
    open(Access, Filename, Mode, SuggestedOptions, Initial).

open(Access, Filename, Mode, SuggestedOptions, Initial) when is_list(Initial) ->
    %% Server side
    case prepare(Access, Filename, Mode, SuggestedOptions, Initial) of
	{ok, AcceptedOptions, State} ->
	    open(Access, Filename, Mode, AcceptedOptions, State);
	{error, {Code, Text}} ->
	    {error, {Code, Text}}
    end;
open(Access, Filename, Mode, NegotiatedOptions, State) when is_record(State, state) ->
    %% Both sides
    case catch handle_options(Access, Filename, Mode, NegotiatedOptions, State) of
	{ok, _Filename2, Options} 
	   when Options =:= NegotiatedOptions ->
	    do_open(State);
	{error, {Code, Text}} ->
	    {error, {Code, Text}}
    end.

do_open(State) when is_record(State, state) ->
    case file:open(State#state.filename, file_options(State)) of
	{ok, Fd} ->
	    {ok, State#state.options, State#state{fd = Fd}};
	{error, Reason} when is_atom(Reason) ->
	    {error, file_error(Reason)}
    end.
	
file_options(State) ->
    case State#state.access of
	read  -> [read, read_ahead, raw, binary];
	write -> [write, delayed_write, raw, binary]
    end.

file_error(Reason) when is_atom(Reason) ->
    Details = file:format_error(Reason),
    case Reason of
	eexist -> {Reason, Details};
	enoent -> {Reason, Details};
	eacces -> {Reason, Details};
	eperm  -> {eacces, Details};
	enospc -> {Reason, Details};
	_      -> {undef,  Details ++ " (" ++ atom_to_list(Reason) ++ ")"}
    end.

%%-------------------------------------------------------------------
%% read(State) ->
%%   {more, Bin, NewState} | {last, Bin, FileSize} | {error, {Code, Text}}
%%
%% State    = term()
%% NewState = term()
%% Bin      = binary()
%% FileSize = integer()
%% Code     = undef | enoent | eacces  | enospc |
%%            badop | eexist | baduser | badopt |
%%            integer()
%% Text     = string()
%%
%% Reads a chunk from the file
%% 
%% The file is automatically closed when the last chunk is read.
%%-------------------------------------------------------------------

read(#state{access = read} = State) ->
    BlkSize = State#state.blksize,
    case file:read(State#state.fd, BlkSize) of
	{ok, Bin} when is_binary(Bin), size(Bin) =:= BlkSize ->
	    Count = State#state.count + size(Bin),
	    {more, Bin, State#state{count = Count}};
	{ok, Bin} when is_binary(Bin), size(Bin) < BlkSize ->
	    file:close(State#state.fd),
	    Count = State#state.count + size(Bin),
	    {last, Bin, Count};
	eof ->
	    {last, <<>>, State#state.count};
	{error, Reason} ->
	    file:close(State#state.fd),
	    {error, file_error(Reason)}
    end.

%%-------------------------------------------------------------------
%% write(Bin, State) ->
%%   {more, NewState} | {last, FileSize} | {error, {Code, Text}}
%%
%% State    = term()
%% NewState = term()
%% Bin      = binary()
%% FileSize = integer()
%% Code     = undef | enoent | eacces  | enospc |
%%            badop | eexist | baduser | badopt |
%%            integer()
%% Text     = string()
%%
%% Writes a chunk to the file
%%
%% The file is automatically closed when the last chunk is written
%%-------------------------------------------------------------------

write(Bin, #state{access = write} = State) when is_binary(Bin) ->
    Size = size(Bin),
    BlkSize = State#state.blksize,
    case file:write(State#state.fd, Bin) of
	ok when Size =:= BlkSize->
	    Count = State#state.count + Size,
	    {more, State#state{count = Count}};
	ok when Size < BlkSize->
	    file:close(State#state.fd),
	    Count = State#state.count + Size,
	    {last, Count};
	{error, Reason}  ->
	    file:close(State#state.fd),
	    file:delete(State#state.filename),
	    {error, file_error(Reason)}
    end.

%%-------------------------------------------------------------------
%% abort(Code, Text, State) -> ok
%% 
%% State    = term()
%% Code     = undef  | enoent | eacces  | enospc |
%%            badop  | eexist | baduser | badopt |
%%            badblk | integer()
%% Text     = string()
%%
%% Aborts the file transfer
%%-------------------------------------------------------------------

abort(_Code, _Text, #state{fd = Fd, access = Access} = State) ->
    file:close(Fd),
    case Access of
	write ->
	    ok = file:delete(State#state.filename);
	read ->
	    ok
    end.

%%-------------------------------------------------------------------
%% Process options
%%-------------------------------------------------------------------

handle_options(Access, Filename, Mode, Options, InitialState) when Mode =:= "octet" ->
    Filename2 = handle_filename(Filename, InitialState),
    Options2 = do_handle_options(Access, Filename2, Options),
    {ok, Filename2, Options2};
handle_options(_Access, _Filename, Mode, _Options, _InitialState) ->
    {error, {badop, "Illegal mode " ++ Mode}}.

handle_filename(Filename, InitialState) when is_list(InitialState) ->
    case lists:keysearch(root_dir, 1, InitialState) of
	{value, {_, Dir}} ->
	    case catch filename_join(Dir, Filename) of
		{'EXIT', _} ->
		    throw({error, {badop, "Internal error. root_dir is not a string"}});
		Filename2 ->
		    Filename2
	    end;
	false ->
	    Filename
    end;
handle_filename(_Filename, State) when is_record(State, state) ->
    State#state.filename.

filename_join(Dir, Filename) ->
    case filename:pathtype(Filename) of
	absolute ->
	    [_ | RelFilename] = filename:split(Filename),
	    filename:join([Dir, RelFilename]);
	_ ->
	    filename:join([Dir, Filename])
    end.

do_handle_options(Access, Filename, [{Key, Val} | T]) ->
    case Key of
	"tsize" ->
	    case Access of
		read when Val =:= "0" ->
		    case file:read_file_info(Filename) of
			{ok, FI} ->
			    Tsize = integer_to_list(FI#file_info.size),
			    [{Key, Tsize} | do_handle_options(Access, Filename, T)];
			{error, _} ->
			    do_handle_options(Access, Filename, T)
		    end;
		_ ->
		    handle_integer(Access, Filename, Key, Val, T, 0, infinity)
	    end;
	"blksize" ->
	    handle_integer(Access, Filename, Key, Val, T, 8, 65464);
	"timeout" ->
	    handle_integer(Access, Filename, Key, Val, T, 1, 255);
	_ ->
	    do_handle_options(Access, Filename, T)
    end;
do_handle_options(_Access, _Filename, []) ->
    [].


handle_integer(Access, Filename, Key, Val, Options, Min, Max) ->
    case catch list_to_integer(Val) of
	{'EXIT', _} ->
	    do_handle_options(Access, Filename, Options);
	Int when Int >= Min, Int =< Max ->
	    [{Key, Val} | do_handle_options(Access, Filename, Options)];
	Int when Int >= Min, Max =:= infinity ->
	    [{Key, Val} | do_handle_options(Access, Filename, Options)];
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

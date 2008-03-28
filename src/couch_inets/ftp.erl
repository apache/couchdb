%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
%% Description: This module implements an ftp client, RFC 959. 
%% It also supports ipv6 RFC 2428.

-module(ftp).

-behaviour(gen_server).

%%  API - Client interface
-export([cd/2, close/1, delete/2, formaterror/1, 
	 lcd/2, lpwd/1, ls/1, ls/2, 
	 mkdir/2, nlist/1, nlist/2, 
	 open/1, open/2, open/3, force_active/1,
	 pwd/1, quote/2,
	 recv/2, recv/3, recv_bin/2, 
	 recv_chunk_start/2, recv_chunk/1, 
	 rename/3, rmdir/2, 
	 send/2, send/3, send_bin/3, 
	 send_chunk_start/2, send_chunk/2, send_chunk_end/1, 
	 type/2, user/3, user/4, account/2,
	 append/3, append/2, append_bin/3,
	 append_chunk/2, append_chunk_end/1, append_chunk_start/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

%% supervisor callbacks
-export([start_link_sup/1]).

-include("ftp_internal.hrl").

%% Constante used in internal state definition
-define(CONNECTION_TIMEOUT, 60*1000).
-define(DEFAULT_MODE, passive).

%% Internal Constants
-define(FTP_PORT, 21).
-define(FILE_BUFSIZE, 4096).

%% Internal state
-record(state, {
	  csock = undefined, % socket() - Control connection socket 
	  dsock = undefined, % socket() - Data connection socket 
	  verbose = false,   % boolean() 
	  ldir = undefined,  % string() - Current local directory
	  type = ftp_server_default,  % atom() - binary | ascii 
	  chunk = false,     % boolean() - Receiving data chunks 
	  mode = ?DEFAULT_MODE,    % passive | active
	  timeout = ?CONNECTION_TIMEOUT, % integer()
	  %% Data received so far on the data connection
	  data = <<>>,   % binary()
	  %% Data received so far on the control connection
	  %% {BinStream, AccLines}. If a binary sequence
	  %% ends with ?CR then keep it in the binary to
	  %% be able to detect if the next received byte is ?LF
	  %% and hence the end of the response is reached!
	  ctrl_data = {<<>>, [], start},  % {binary(), [bytes()], LineStatus}
	  %% pid() - Client pid (note not the same as "From")
	  owner = undefined,   
	  client = undefined,  % "From" to be used in gen_server:reply/2
	  %% Function that activated a connection and maybe some
	  %% data needed further on.
	  caller = undefined, % term()     
	  ip_v6_disabled,     % boolean()
	  progress = ignore   % ignore | pid()	    
	 }).

%%%=========================================================================
%%%  API - CLIENT FUNCTIONS
%%%=========================================================================
%%--------------------------------------------------------------------------
%% open(Host, <Port>, <Flags>) -> {ok, Pid} | {error, ehost}
%%	Host = string(), 
%%      Port = integer(), 
%%      Flags = [Flag], 
%%      Flag = verbose | debug | trace
%%
%% Description:  Start an ftp client and connect to a host.
%%--------------------------------------------------------------------------
%% The only option was the host in textual form
open({option_list, Options})->
    ensure_started(),
    Flags = key_search(flags, Options, []),
    {ok, Pid} =  ftp_sup:start_child([[[{client, self()}, Flags], []]]),
    call(Pid, {open, ip_comm, Options}, pid);
	 
%% The only option was the tuple form of the ip-number
open(Host) when tuple(Host) ->
    open(Host, ?FTP_PORT, []);

%% Host is the string form of the hostname 
open(Host)->
    open(Host, ?FTP_PORT, []).

open(Host, Port) when integer(Port) ->
    open(Host, Port, []);

open(Host, Flags) when list(Flags) ->
    open(Host, ?FTP_PORT, Flags).

open(Host, Port, Flags) when integer(Port), list(Flags) ->
    ensure_started(),
    {ok, Pid} = ftp_sup:start_child([[[{client, self()}, Flags], []]]), 
    Opts = [{host, Host}, {port, Port}| Flags], 
    call(Pid, {open, ip_comm, Opts}, pid).

%%--------------------------------------------------------------------------
%% user(Pid, User, Pass, <Acc>) -> ok | {error, euser} | {error, econn} 
%%                                    | {error, eacct}
%%	Pid = pid(), 
%%      User = Pass =  Acc = string()
%%
%% Description:  Login with or without a supplied account name.
%%--------------------------------------------------------------------------
user(Pid, User, Pass) ->
    call(Pid, {user, User, Pass}, atom).

user(Pid, User, Pass, Acc) ->
    call(Pid, {user, User, Pass, Acc}, atom).

%%--------------------------------------------------------------------------
%% account(Pid, Acc)  -> ok | {error, eacct}
%%	Pid = pid()
%%	Acc= string()
%%
%% Description:  Set a user Account.
%%--------------------------------------------------------------------------
account(Pid, Acc) ->
    call(Pid, {account, Acc}, atom).

%%--------------------------------------------------------------------------
%% pwd(Pid) -> {ok, Dir} | {error, elogin} | {error, econn} 
%%	Pid = pid()
%%      Dir = string()
%%
%% Description:  Get the current working directory at remote server.
%%--------------------------------------------------------------------------
pwd(Pid) ->
    call(Pid, pwd, ctrl).

%%--------------------------------------------------------------------------
%% lpwd(Pid) ->  {ok, Dir} | {error, elogin} 
%%	Pid = pid()
%%      Dir = string()
%%
%% Description:  Get the current working directory at local server.
%%--------------------------------------------------------------------------
lpwd(Pid) ->
    call(Pid, lpwd, string).

%%--------------------------------------------------------------------------
%% cd(Pid, Dir) ->  ok | {error, epath} | {error, elogin} | {error, econn}
%%	Pid = pid()
%%	Dir = string()
%%
%% Description:  Change current working directory at remote server.
%%--------------------------------------------------------------------------
cd(Pid, Dir) ->
    call(Pid, {cd, Dir}, atom).

%%--------------------------------------------------------------------------
%% lcd(Pid, Dir) ->  ok | {error, epath}
%%	Pid = pid()
%%	Dir = string()
%%
%% Description:  Change current working directory for the local client.
%%--------------------------------------------------------------------------
lcd(Pid, Dir) ->
    call(Pid, {lcd, Dir}, string).

%%--------------------------------------------------------------------------
%% ls(Pid, <Dir>) -> {ok, Listing} | {error, epath} | {error, elogin} | 
%%                   {error, econn}
%%	Pid = pid()
%%	Dir = string()
%%      Listing = string()
%%
%% Description: List the contents of current directory (ls/1) or
%% directory Dir (ls/2) at remote server.
%%--------------------------------------------------------------------------
ls(Pid) ->
  ls(Pid, "").
ls(Pid, Dir) ->
    call(Pid, {dir, long, Dir}, string).

%%--------------------------------------------------------------------------
%% nlist(Pid, <Dir>) -> {ok, Listing} | {error, epath} | {error, elogin} | 
%%                      {error, econn}
%%	Pid = pid()
%%	Dir = string()
%%
%% Description:  List the contents of current directory (ls/1) or directory
%%           Dir (ls/2) at remote server. The returned list is a stream
%%           of file names.
%%--------------------------------------------------------------------------
nlist(Pid) ->
  nlist(Pid, "").
nlist(Pid, Dir) ->
    call(Pid, {dir, short, Dir}, string).

%%--------------------------------------------------------------------------
%% rename(Pid, CurrFile, NewFile) ->  ok | {error, epath} | {error, elogin} 
%%                                    | {error, econn}
%%	Pid = pid()
%%	CurrFile = NewFile = string()
%%
%% Description:  Rename a file at remote server.
%%--------------------------------------------------------------------------
rename(Pid, CurrFile, NewFile) ->
    call(Pid, {rename, CurrFile, NewFile}, string).

%%--------------------------------------------------------------------------
%% delete(Pid, File) ->  ok | {error, epath} | {error, elogin} | 
%%                       {error, econn}
%%	Pid = pid()
%%	File = string()
%%
%% Description:  Remove file at remote server.
%%--------------------------------------------------------------------------
delete(Pid, File) ->
    call(Pid, {delete, File}, string).

%%--------------------------------------------------------------------------
%% mkdir(Pid, Dir) -> ok | {error, epath} | {error, elogin} | {error, econn}
%%	Pid = pid(), 
%%	Dir = string()
%%
%% Description:  Make directory at remote server.
%%--------------------------------------------------------------------------
mkdir(Pid, Dir) ->
    call(Pid, {mkdir, Dir}, atom).

%%--------------------------------------------------------------------------
%% rmdir(Pid, Dir) -> ok | {error, epath} | {error, elogin} | {error, econn}
%%	Pid = pid(), 
%%	Dir = string()
%%
%% Description:  Remove directory at remote server.
%%--------------------------------------------------------------------------
rmdir(Pid, Dir) ->
    call(Pid, {rmdir, Dir}, atom).

%%--------------------------------------------------------------------------
%% type(Pid, Type) -> ok | {error, etype} | {error, elogin} | {error, econn}
%%	Pid = pid() 
%%	Type = ascii | binary
%%
%% Description:  Set transfer type.
%%--------------------------------------------------------------------------
type(Pid, Type) ->
    call(Pid, {type, Type}, atom).

%%--------------------------------------------------------------------------
%% recv(Pid, RemoteFileName <LocalFileName>) -> ok | {error, epath} |
%%                                          {error, elogin} | {error, econn}
%%	Pid = pid()
%%	RemoteFileName = LocalFileName = string()
%%
%% Description:  Transfer file from remote server.
%%--------------------------------------------------------------------------
recv(Pid, RemotFileName) ->
  recv(Pid, RemotFileName, RemotFileName).

recv(Pid, RemotFileName, LocalFileName) ->
    call(Pid, {recv, RemotFileName, LocalFileName}, atom).

%%--------------------------------------------------------------------------
%% recv_bin(Pid, RemoteFile) -> {ok, Bin} | {error, epath} | {error, elogin} 
%%			   | {error, econn}
%%	Pid = pid()
%%	RemoteFile = string()
%%      Bin = binary()
%%
%% Description:  Transfer file from remote server into binary.
%%--------------------------------------------------------------------------
recv_bin(Pid, RemoteFile) ->
    call(Pid, {recv_bin, RemoteFile}, bin).

%%--------------------------------------------------------------------------
%% recv_chunk_start(Pid, RemoteFile) -> ok | {error, elogin} | {error, epath} 
%%                                 | {error, econn}
%%	Pid = pid()
%%	RemoteFile = string()
%%
%% Description:  Start receive of chunks of remote file.
%%--------------------------------------------------------------------------
recv_chunk_start(Pid, RemoteFile) ->
    call(Pid, {recv_chunk_start, RemoteFile}, atom).

%%--------------------------------------------------------------------------
%% recv_chunk(Pid, RemoteFile) ->  ok | {ok, Bin} | {error, Reason}
%%	Pid = pid()
%%	RemoteFile = string()
%%
%% Description:  Transfer file from remote server into binary in chunks
%%--------------------------------------------------------------------------
recv_chunk(Pid) ->
    call(Pid, recv_chunk, atom).

%%--------------------------------------------------------------------------
%% send(Pid, LocalFileName <RemotFileName>) -> ok | {error, epath} 
%%                                                | {error, elogin} 
%%                             | {error, econn}
%%	Pid = pid()
%%	LocalFileName = RemotFileName = string()
%%
%% Description:  Transfer file to remote server.
%%--------------------------------------------------------------------------
send(Pid, LocalFileName) ->
  send(Pid, LocalFileName, LocalFileName).

send(Pid, LocalFileName, RemotFileName) ->
    call(Pid, {send, LocalFileName, RemotFileName}, atom).

%%--------------------------------------------------------------------------
%% send_bin(Pid, Bin, RemoteFile) -> ok | {error, epath} | {error, elogin} 
%%                             | {error, enotbinary} | {error, econn}
%%	Pid = pid()
%%	Bin = binary()
%%	RemoteFile = string()
%%
%% Description:  Transfer a binary to a remote file.
%%--------------------------------------------------------------------------
send_bin(Pid, Bin, RemoteFile) when binary(Bin) ->
    call(Pid, {send_bin, Bin, RemoteFile}, atom);
send_bin(_Pid, _Bin, _RemoteFile) ->
  {error, enotbinary}.

%%--------------------------------------------------------------------------
%% send_chunk_start(Pid, RemoteFile) -> ok | {error, elogin} | {error, epath} 
%%                                 | {error, econn}
%%	Pid = pid()
%%	RemoteFile = string()
%%
%% Description:  Start transfer of chunks to remote file.
%%--------------------------------------------------------------------------
send_chunk_start(Pid, RemoteFile) ->
    call(Pid, {send_chunk_start, RemoteFile}, atom).

%%--------------------------------------------------------------------------
%% append_chunk_start(Pid, RemoteFile) -> ok | {error, elogin} | 
%%                                        {error, epath} | {error, econn}
%%	Pid = pid()
%%	RemoteFile = string()
%%
%% Description:  Start append chunks of data to remote file.
%%--------------------------------------------------------------------------
append_chunk_start(Pid, RemoteFile) ->
    call(Pid, {append_chunk_start, RemoteFile}, atom).

%%--------------------------------------------------------------------------
%% send_chunk(Pid, Bin) -> ok | {error, elogin} | {error, enotbinary} 
%%                       | {error, echunk} | {error, econn}
%%      Pid = pid()
%%	Bin = binary().
%%
%% Purpose:  Send chunk to remote file.
%%--------------------------------------------------------------------------
send_chunk(Pid, Bin) when binary(Bin) ->
    call(Pid, {transfer_chunk, Bin}, atom);
send_chunk(_Pid, _Bin) ->
  {error, enotbinary}.

%%--------------------------------------------------------------------------
%% append_chunk(Pid, Bin) -> ok | {error, elogin} | {error, enotbinary} 
%%			     | {error, echunk} | {error, econn}
%%	Pid = pid()
%%	Bin = binary()
%%
%% Description:  Append chunk to remote file.
%%--------------------------------------------------------------------------
append_chunk(Pid, Bin) when binary(Bin) ->
    call(Pid, {transfer_chunk, Bin}, atom);
append_chunk(_Pid, _Bin) ->
  {error, enotbinary}.

%%--------------------------------------------------------------------------
%% send_chunk_end(Pid) -> ok | {error, elogin} | {error, echunk} 
%%			  | {error, econn}
%%	Pid = pid()
%%
%% Description:  End sending of chunks to remote file.
%%--------------------------------------------------------------------------
send_chunk_end(Pid) ->
    call(Pid, chunk_end, atom).

%%--------------------------------------------------------------------------
%% append_chunk_end(Pid) ->  ok | {error, elogin} | {error, echunk} 
%%			     | {error, econn}
%%	Pid = pid()
%%
%% Description:  End appending of chunks to remote file.
%%--------------------------------------------------------------------------
append_chunk_end(Pid) ->
    call(Pid, chunk_end, atom).

%%--------------------------------------------------------------------------
%% append(Pid, LocalFileName, RemotFileName) -> ok | {error, epath} 
%%                                          | {error, elogin} | {error, econn}
%%	Pid = pid()
%%	LocalFileName = RemotFileName = string()
%%
%% Description:  Append the local file to the remote file
%%--------------------------------------------------------------------------
append(Pid, LocalFileName) ->
    append(Pid, LocalFileName, LocalFileName).

append(Pid, LocalFileName, RemotFileName) ->
    call(Pid, {append, LocalFileName, RemotFileName}, atom).

%%--------------------------------------------------------------------------
%% append_bin(Pid, Bin, RemoteFile) -> ok | {error, epath} | {error, elogin} 
%%				  | {error, enotbinary} | {error, econn}
%%	Pid = pid()
%%	Bin = binary()
%%	RemoteFile = string()
%%
%% Purpose:  Append a binary to a remote file.
%%--------------------------------------------------------------------------
append_bin(Pid, Bin, RemoteFile) when binary(Bin) ->
    call(Pid, {append_bin, Bin, RemoteFile}, atom);
append_bin(_Pid, _Bin, _RemoteFile) ->
    {error, enotbinary}.

%%--------------------------------------------------------------------------
%% quote(Pid, Cmd) -> ok
%%	Pid = pid()
%%	Cmd = string()
%%
%% Description: Send arbitrary ftp command.
%%--------------------------------------------------------------------------
quote(Pid, Cmd) when list(Cmd) ->
    call(Pid, {quote, Cmd}, atom).

%%--------------------------------------------------------------------------
%% close(Pid) -> ok
%%	Pid = pid()
%%
%% Description:  End the ftp session.
%%--------------------------------------------------------------------------
close(Pid) ->
    cast(Pid, close),
    ok.

%%--------------------------------------------------------------------------
%% force_active(Pid) -> ok
%%	Pid = pid()
%%
%% Description: Force connection to use active mode. 
%%--------------------------------------------------------------------------
force_active(Pid) ->
    error_logger:info_report("This function is deprecated use the mode flag "
			     "to open/[1,2,3] instead", []),
    call(Pid, force_active, atom).

%%--------------------------------------------------------------------------
%% formaterror(Tag) -> string()
%%	Tag = atom() | {error, atom()}
%%
%% Description:  Return diagnostics.
%%--------------------------------------------------------------------------
formaterror(Tag) ->
  ftp_response:error_string(Tag).

%%%========================================================================
%%% gen_server callback functions 
%%%========================================================================

%%-------------------------------------------------------------------------
%% init(Args) -> {ok, State} | {ok, State, Timeout} | {stop, Reason}
%% Description: Initiates the erlang process that manages a ftp connection.
%%-------------------------------------------------------------------------
init([{client, ClientPid}, Flags]) ->
    process_flag(trap_exit, true),
    erlang:monitor(process, ClientPid),
    inet_db:start(),
    {ok, LDir} = file:get_cwd(),
    State = case is_debug(Flags) or is_trace(Flags) of
		true ->
		    dbg:tracer(),
		    dbg:p(all, [call]),
		    case  is_debug(Flags) of 
			true ->
			    dbg:tp(ftp, [{'_', [], [{return_trace}]}]),
			    dbg:tp(ftp_response, [{'_', [], 
						   [{return_trace}]}]),
			    dbg:tp(ftp_progress, [{'_', [], 
						   [{return_trace}]}]); 
			false -> %trace
			    dbg:tpl(ftp, [{'_', [], [{return_trace}]}]),
			    dbg:tpl(ftp_response, [{'_', [], 
						    [{return_trace}]}]),
			    dbg:tpl(ftp_progress, [{'_', [], 
						    [{return_trace}]}])  
		    end,
		    #state{ldir = LDir};
		false ->
		    case is_verbose(Flags) of
			true ->
			    #state{verbose = true, ldir = LDir};
			false ->
			    #state{ldir = LDir}  
		    end
	    end,
    process_flag(priority, low), 
    {ok, State#state{owner = ClientPid,
	   ip_v6_disabled = is_ipv6_disabled(Flags)}}.


%%--------------------------------------------------------------------------
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State}               |
%%                                      {noreply, State, Timeout}      |
%%                                      {stop, Reason, Reply, State}   |
%% Description: Handle incoming requests. 
%%-------------------------------------------------------------------------
handle_call({Pid, _}, _, #state{owner = Owner} = State) when Owner =/= Pid ->
    {reply, {error, not_connection_owner}, State};

handle_call({_, {open, ip_comm, Opts}}, From, State) ->
    case key_search(host, Opts, undefined) of
	undefined ->
	    {stop, normal, {error, ehost}, State};
	Host ->
	    IsPosInt = fun(Int) when is_integer(Int), Int > 0 ->
			       true;
			  (_) -> 
			       false
		       end,
	    
	    IsModeAtom = fun(active) ->
				 true;
			    (passive) ->
				 true;
			    (_) ->
				 false
			 end,
	    
	    Mode = check_option(IsModeAtom,
				key_search(mode, Opts, ?DEFAULT_MODE),
				?DEFAULT_MODE),
	    Port = check_option(IsPosInt, key_search(port, Opts, ?FTP_PORT), 
				?FTP_PORT),
	    Timeout = check_option(IsPosInt, key_search(timeout, Opts, 
							?CONNECTION_TIMEOUT),
				   ?CONNECTION_TIMEOUT),
	    ProgressOptions = key_search(progress, Opts, ignore),
	    
	    setup_ctrl_connection(Host, Port, Timeout, 
				  State#state{client = From, mode = Mode,
					      progress = 
					      progress(ProgressOptions)})
    end;	

handle_call({_, force_active}, _, State) ->
    {reply, ok, State#state{mode = active}};

handle_call({_, {user, User, Password}}, From, State) ->
    handle_user(User, Password, "", State#state{client = From});

handle_call({_, {user, User, Password, Acc}}, From, State) ->
    handle_user(User, Password, Acc, State#state{client = From});
   
handle_call({_, {account, Acc}}, From, State)->
    handle_user_account(Acc, State#state{client = From});

handle_call({_, pwd}, From, #state{chunk = false} = State) ->
    send_ctrl_message(State, mk_cmd("PWD", [])), 
    activate_ctrl_connection(State),
    {noreply, State#state{client = From, caller = pwd}};

handle_call({_, lpwd}, From,  #state{ldir = LDir} = State) ->
    {reply, {ok, LDir}, State#state{client = From}};

handle_call({_, {cd, Dir}}, From,  #state{chunk = false} 
	    = State) ->
    send_ctrl_message(State, mk_cmd("CWD ~s", [Dir])), 
    activate_ctrl_connection(State),
    {noreply, State#state{client = From, caller = cd}};

handle_call({_,{lcd, Dir}}, _From, #state{ldir = LDir0} = State) ->
    LDir = filename:absname(Dir, LDir0),
    case file:read_file_info(LDir) of %% FIX better check that LDir is a dir.
	{ok, _ } ->
	    {reply, ok, State#state{ldir = LDir}};
	_  ->
	    {reply, {error, epath}, State}
    end;

handle_call({_, {dir, Len, Dir}}, {_Pid, _} = From, 
	    #state{chunk = false} = State) ->
    setup_data_connection(State#state{caller = {dir, Dir, Len},
				      client = From});
handle_call({_, {rename, CurrFile, NewFile}}, From,
	    #state{chunk = false} = State) ->
    send_ctrl_message(State, mk_cmd("RNFR ~s", [CurrFile])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {rename, NewFile}, client = From}};

handle_call({_, {delete, File}}, {_Pid, _} = From, 
	    #state{chunk = false} = State) ->
    send_ctrl_message(State, mk_cmd("DELE ~s", [File])),
    activate_ctrl_connection(State),
    {noreply, State#state{client = From}};

handle_call({_, {mkdir, Dir}}, From,  #state{chunk = false} = State) ->
    send_ctrl_message(State, mk_cmd("MKD ~s", [Dir])),
    activate_ctrl_connection(State),
    {noreply, State#state{client = From}};

handle_call({_,{rmdir, Dir}}, From, #state{chunk = false} = State) ->
    send_ctrl_message(State, mk_cmd("RMD ~s", [Dir])),
    activate_ctrl_connection(State),
    {noreply, State#state{client = From}};

handle_call({_,{type, Type}}, From,  #state{chunk = false} 
	    = State) ->  
    case Type of
	ascii ->
	    send_ctrl_message(State, mk_cmd("TYPE A", [])),
	    activate_ctrl_connection(State),
	    {noreply, State#state{caller = type, type = ascii, 
				  client = From}};
	binary ->
	    send_ctrl_message(State, mk_cmd("TYPE I", [])),
	    activate_ctrl_connection(State),
	    {noreply, State#state{caller = type, type = binary, 
				  client = From}};
	_ ->
	    {reply, {error, etype}, State}
    end;

handle_call({_,{recv, RemoteFile, LocalFile}}, From, 
	    #state{chunk = false, ldir = LocalDir} = State) ->
    progress_report({remote_file, RemoteFile}, State),
    NewLocalFile = filename:absname(LocalFile, LocalDir),

    case file_open(NewLocalFile, write) of
	{ok, Fd} ->
	    setup_data_connection(State#state{client = From,
					      caller = 
					      {recv_file, 
					       RemoteFile, Fd}});
	{error, _What} ->
	    {reply, {error, epath}, State}
    end;

handle_call({_, {recv_bin, RemoteFile}}, From, #state{chunk = false} = 
	    State) ->
    setup_data_connection(State#state{caller = {recv_bin, RemoteFile},
				      client = From});

handle_call({_,{recv_chunk_start, RemoteFile}}, From, #state{chunk = false} 
	    = State) ->
    setup_data_connection(State#state{caller = {start_chunk_transfer,
						"RETR", RemoteFile},
				      client = From});

handle_call({_, recv_chunk}, _, #state{chunk = false} = State) ->
    {reply, {error, "ftp:recv_chunk_start/2 not called"}, State}; 

handle_call({_, recv_chunk}, From, #state{chunk = true} = State) ->
    activate_data_connection(State),
    {noreply, State#state{client = From, caller = recv_chunk}};
    
handle_call({_, {send, LocalFile, RemoteFile}}, From, 
	    #state{chunk = false, ldir = LocalDir} = State) ->
    progress_report({local_file, filename:absname(LocalFile, LocalDir)}, 
		    State),
    setup_data_connection(State#state{caller = {transfer_file,
						   {"STOR", 
						    LocalFile, RemoteFile}},
					 client = From});
handle_call({_, {append, LocalFile, RemoteFile}}, From, 
	    #state{chunk = false} = State) ->
    setup_data_connection(State#state{caller = {transfer_file,
						{"APPE", 
						 LocalFile, RemoteFile}},
				      client = From});
handle_call({_, {send_bin, Bin, RemoteFile}}, From, 
	    #state{chunk = false} = State) ->
    setup_data_connection(State#state{caller = {transfer_data,
					       {"STOR", Bin, RemoteFile}},
				      client = From});
handle_call({_,{append_bin, Bin, RemoteFile}}, From, 
	    #state{chunk = false} = State) ->
    setup_data_connection(State#state{caller = {transfer_data,
						{"APPE", Bin, RemoteFile}},
				      client = From});
handle_call({_, {send_chunk_start, RemoteFile}}, From, #state{chunk = false} 
	    = State) ->
    setup_data_connection(State#state{caller = {start_chunk_transfer,
						"STOR", RemoteFile},
				      client = From});
handle_call({_, {append_chunk_start, RemoteFile}}, From, #state{chunk = false} 
	    = State) ->
    setup_data_connection(State#state{caller = {start_chunk_transfer,
						"APPE", RemoteFile},
				      client = From});
handle_call({_, {transfer_chunk, Bin}}, _, #state{chunk = true} = State) ->
    send_data_message(State, Bin),
    {reply, ok, State};

handle_call({_, chunk_end}, From, #state{chunk = true} = State) ->
    close_data_connection(State),
    activate_ctrl_connection(State),
    {noreply, State#state{client = From, dsock = undefined, 
			  caller = end_chunk_transfer, chunk = false}};

handle_call({_, {quote, Cmd}}, From, #state{chunk = false} = State) ->
    send_ctrl_message(State, mk_cmd(Cmd, [])),
    activate_ctrl_connection(State),
    {noreply, State#state{client = From, caller = quote}};

handle_call(_, _, #state{chunk = true} = State) ->
    {reply, {error, echunk}, State};

%% Catch all -  This can only happen if the application programmer writes 
%% really bad code that violates the API.
handle_call(Request, _Timeout, State) ->
    {stop, {'API_violation_connection_closed', Request},
     {error, {connection_terminated, 'API_violation'}}, State}.

%%--------------------------------------------------------------------------
%% handle_cast(Request, State) -> {noreply, State} | 
%%                                {noreply, State, Timeout} |
%%                                {stop, Reason, State} 
%% Description: Handles cast messages.         
%%-------------------------------------------------------------------------
handle_cast({Pid, close}, #state{owner = Pid} = State) ->
    send_ctrl_message(State, mk_cmd("QUIT", [])),
    close_ctrl_connection(State),
    close_data_connection(State),
    {stop, normal, State#state{csock = undefined, dsock = undefined}};

handle_cast({Pid, close}, State) ->
    error_logger:info_report("A none owner process ~p tried to close an "
			     "ftp connection: ~n", [Pid]),
    {noreply, State};

%% Catch all -  This can oly happen if the application programmer writes 
%% really bad code that violates the API.
handle_cast(Msg, State) ->
  {stop, {'API_violation_connection_colsed', Msg}, State}.

%%--------------------------------------------------------------------------
%% handle_info(Msg, State) -> {noreply, State} | {noreply, State, Timeout} |
%%			      {stop, Reason, State}
%% Description: Handles tcp messages from the ftp-server.
%% Note: The order of the function clauses is significant.
%%--------------------------------------------------------------------------

handle_info(timeout, #state{caller = open} = State) ->
    {stop, timeout, State};

handle_info(timeout, State) ->
    {noreply, State};

%%% Data socket messages %%%
handle_info({tcp, Socket, Data}, 
	    #state{dsock = Socket, 
		   caller = {recv_file, Fd}} = State) ->    
    file_write(binary_to_list(Data), Fd),
    progress_report({binary, Data}, State),
    activate_data_connection(State),
    {noreply, State};

handle_info({tcp, Socket, Data}, #state{dsock = Socket, client = From,	
					caller = recv_chunk} 
	    = State)  ->    
    gen_server:reply(From, {ok, Data}),
    {noreply, State#state{client = undefined, data = <<>>}};

handle_info({tcp, Socket, Data}, #state{dsock = Socket} = State) ->
    activate_data_connection(State),
    {noreply, State#state{data = <<(State#state.data)/binary,
				  Data/binary>>}};

handle_info({tcp_closed, Socket}, #state{dsock = Socket,
					 caller = {recv_file, Fd}} 
	    = State) ->
    file_close(Fd),
    progress_report({transfer_size, 0}, State),
    activate_ctrl_connection(State),
    {noreply, State#state{dsock = undefined, data = <<>>}};

handle_info({tcp_closed, Socket}, #state{dsock = Socket, client = From,
					 caller = recv_chunk} 
	    = State) ->
    gen_server:reply(From, ok),
    {noreply, State#state{dsock = undefined, client = undefined,
			  data = <<>>, caller = undefined,
			  chunk = false}};

handle_info({tcp_closed, Socket}, #state{dsock = Socket, caller = recv_bin, 
					 data = Data} = State) ->
    activate_ctrl_connection(State),
    {noreply, State#state{dsock = undefined, data = <<>>, 
			  caller = {recv_bin, Data}}};

handle_info({tcp_closed, Socket}, #state{dsock = Socket, data = Data,
					 caller = {handle_dir_result, Dir}} 
	    = State) ->
    activate_ctrl_connection(State),
    {noreply, State#state{dsock = undefined, 
			  caller = {handle_dir_result, Dir, Data},
%			  data = <<?CR,?LF>>}};
			  data = <<>>}};
	    
handle_info({tcp_error, Socket, Reason}, #state{dsock = Socket,
						client = From} = State) ->
    gen_server:reply(From, {error, Reason}),
    close_data_connection(State),
    {noreply, State#state{dsock = undefined, client = undefined,
			  data = <<>>, caller = undefined, chunk = false}};

%%% Ctrl socket messages %%%
handle_info({tcp, Socket, Data}, #state{csock = Socket, 
					verbose = Verbose,
					caller = Caller,
					client = From,
					ctrl_data = {CtrlData, AccLines, 
						     LineStatus}} 
	    = State) ->    
    case ftp_response:parse_lines(<<CtrlData/binary, Data/binary>>, 
				  AccLines, LineStatus) of
	{ok, Lines, NextMsgData} ->
	    verbose(Lines, Verbose, 'receive'),
	    CtrlResult = ftp_response:interpret(Lines), 
	    case Caller of
		quote ->
		    gen_server:reply(From, string:tokens(Lines, [?CR, ?LF])),
		    {noreply, State#state{client = undefined, 
					  caller = undefined,
					  ctrl_data = {NextMsgData, [], 
						       start}}};
		_ ->
		    handle_ctrl_result(CtrlResult,
				       State#state{ctrl_data = 
						   {NextMsgData, [], start}})
	    end;
	{continue, NewCtrlData} ->
	    activate_ctrl_connection(State),
	    {noreply, State#state{ctrl_data = NewCtrlData}}
    end;

handle_info({tcp_closed, Socket}, #state{csock = Socket}) ->  
    %% If the server closes the control channel it is 
    %% the expected behavior that connection process terminates.
    exit(normal); %% User will get error message from terminate/2

handle_info({tcp_error, Socket, Reason}, _) ->
    error_logger:error_report("tcp_error on socket: ~p  for reason: ~p~n", 
			      [Socket, Reason]),
    %% If tcp does not work the only option is to terminate,
    %% this is the expected behavior under these circumstances.
    exit(normal); %% User will get error message from terminate/2

%% Monitor messages - if the process owning the ftp connection goes
%% down there is no point in continuing.
handle_info({'DOWN', _Ref, _Type, _Process, normal}, State) ->
    {stop, normal, State#state{client = undefined}};

handle_info({'DOWN', _Ref, _Type, _Process, shutdown}, State) ->
    {stop, normal, State#state{client = undefined}};
    
handle_info({'DOWN', _Ref, _Type, _Process, timeout}, State) ->
    {stop, normal, State#state{client = undefined}};
 
handle_info({'DOWN', _Ref, _Type, Process, Reason}, State) ->
    {stop, {stopped, {'EXIT', Process, Reason}},
     State#state{client = undefined}};

handle_info({'EXIT', Pid, Reason}, #state{progress = Pid} = State) ->
    error_logger:info_report("Progress reporting stopped for reason ~p~n",
			     Reason),
    {noreply, State#state{progress = ignore}};
   
%% Catch all - throws away unknown messages (This could happen by "accident"
%% so we do not want to crash, but we make a log entry as it is an
%% unwanted behaviour.) 
handle_info(Info, State) ->
    error_logger:info_report("ftp : ~p : Unexpected message: ~p\n", 
			     [self(), Info]),
    {noreply, State}.

%%--------------------------------------------------------------------------
%% terminate/2 and code_change/3
%%--------------------------------------------------------------------------
terminate(normal, State) ->
    %% If terminate reason =/= normal the progress reporting process will
    %% be killed by the exit signal.
    progress_report(stop, State), 
    do_termiante({error, econn}, State);
terminate(Reason, State) -> 
    error_logger:error_report("Ftp connection closed due to: ~p~n", [Reason]),
    do_termiante({error, eclosed}, State).

do_termiante(ErrorMsg, State) ->
    close_data_connection(State),
    close_ctrl_connection(State),
    case State#state.client of
	undefined ->
	    ok;
	From ->
	    gen_server:reply(From, ErrorMsg)
    end,
    ok. 

code_change(_, State, _) ->
    {ok, State}.

%%%=========================================================================
%% Start/stop
%%%=========================================================================
%%--------------------------------------------------------------------------
%% start_link_sup([Args, Options]) -> {ok, Pid} | {error, Reason} 
%%                                    
%% Description: Callback function for the ftp supervisor. It is called 
%%            : when open/[1,3] calls ftp_sup:start_child/1 to start an 
%%            : instance of the ftp process.
%%--------------------------------------------------------------------------
start_link_sup([Args, Options]) ->
    gen_server:start_link(?MODULE, Args, Options).

%%% Stop functionality is handled by close/1

%%%========================================================================
%%% Internal functions
%%%========================================================================

%%--------------------------------------------------------------------------
%%% Help functions to handle_call and/or handle_ctrl_result
%%--------------------------------------------------------------------------
%% User handling 
handle_user(User, Password, Acc, State) ->
    send_ctrl_message(State, mk_cmd("USER ~s", [User])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {handle_user, Password, Acc}}}.

handle_user_passwd(Password, Acc, State) ->
    send_ctrl_message(State, mk_cmd("PASS ~s", [Password])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {handle_user_passwd, Acc}}}.

handle_user_account(Acc, State) ->
    send_ctrl_message(State, mk_cmd("ACCT ~s", [Acc])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = handle_user_account}}.

%%--------------------------------------------------------------------------
%% handle_ctrl_result 
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
%% Handling of control connection setup
handle_ctrl_result({pos_compl, _}, #state{caller = open, client = From} 
		   = State) ->
    gen_server:reply(From,  {ok, self()}),
    {noreply, State#state{client = undefined, 
			  caller = undefined }};
handle_ctrl_result({_, Lines}, #state{caller = open} = State) ->
    ctrl_result_response(econn, State, {error, Lines});

%%--------------------------------------------------------------------------
%% Data connection setup active mode 
handle_ctrl_result({pos_compl, _Lines}, 
		   #state{mode   = active,
			  caller = {setup_data_connection, 
				    {LSock, Caller}}} = State) ->
    handle_caller(State#state{caller = Caller, dsock = {lsock, LSock}});

handle_ctrl_result({Status, Lines}, 
		   #state{mode   = active, 
			  caller = {setup_data_connection, {LSock, _}}} 
		   = State) ->
    close_connection(LSock),
    ctrl_result_response(Status, State, {error, Lines});

%% Data connection setup passive mode 
handle_ctrl_result({pos_compl, Lines}, #state{mode = passive,
					      ip_v6_disabled = false,
					      client=From,
					      caller = 
					      {setup_data_connection, 
					       Caller},
					      csock = CSock,
					      timeout = Timeout} 
		   = State) ->
    [_, PortStr | _] =  lists:reverse(string:tokens(Lines, "|")),
    {ok, {IP, _}} = inet:peername(CSock),
    case connect(IP, list_to_integer(PortStr), Timeout, State) of
	{_,{ok, Socket}} ->	       
	    handle_caller(State#state{caller = Caller, dsock = Socket});
	{_,{error,Reason}} ->
	    gen_server:reply(From,{error,Reason}),
	    {noreply,State#state{client = undefined, caller = undefined}}
    end;

handle_ctrl_result({pos_compl, Lines}, 
		   #state{mode = passive, ip_v6_disabled = true,
			  client=From,
			  caller = {setup_data_connection, Caller}, 
			  timeout = Timeout} = State) ->
    
    {_, [?LEFT_PAREN | Rest]} = 
	lists:splitwith(fun(?LEFT_PAREN) -> false; (_) -> true end, Lines),
    {NewPortAddr, _} =
	lists:splitwith(fun(?RIGHT_PAREN) -> false; (_) -> true end, Rest),
    [A1, A2, A3, A4, P1, P2] = lists:map(fun(X) -> list_to_integer(X) end,
					 string:tokens(NewPortAddr, [$,])),
    case connect({A1, A2, A3, A4}, (P1 * 256) + P2, Timeout, State) of
	{_,{ok,Socket}} ->
	    handle_caller(State#state{caller = Caller, dsock = Socket});
	{_,{error,Reason}} ->
	    gen_server:reply(From,{error,Reason}),
	    {noreply,State#state{client = undefined, caller = undefined}}
    end;

%% FTP server does not support passive mode try to fallback on active mode
handle_ctrl_result(_, #state{mode = passive, caller = {setup_data_connection, 
						       Caller}} = State) ->
    setup_data_connection(State#state{mode = active, caller = Caller});
    
%%--------------------------------------------------------------------------
%% User handling 
handle_ctrl_result({pos_interm, _}, #state{caller =
					   {handle_user, PassWord, Acc}}
		   = State) ->
    handle_user_passwd(PassWord, Acc, State);
handle_ctrl_result({Status, _}, 
		   #state{caller = {handle_user, _, _}} = State) ->
    ctrl_result_response(Status, State, {error, euser});

%% Accounts 
handle_ctrl_result({pos_interm_acct, _}, #state{caller = 
						{handle_user_passwd, Acc}} = 
		   State) when Acc =/= "" ->
    handle_user_account(Acc, State);
handle_ctrl_result({Status, _},
		   #state{caller = {handle_user_passwd, _}} = State) ->
    ctrl_result_response(Status, State, {error, euser});

%%--------------------------------------------------------------------------
%% Print current working directory
handle_ctrl_result({pos_compl, Lines}, #state{caller = pwd, 
					      client = From} = State) ->
    Dir = pwd_result(Lines),
    gen_server:reply(From, {ok, Dir}),
    {noreply, State#state{client = undefined, caller = undefined}};

%%--------------------------------------------------------------------------
%% Directory listing 
handle_ctrl_result({pos_prel, _}, #state{caller = {dir, Dir}} = State) ->
    NewState = accept_data_connection(State),
    activate_data_connection(NewState),
    {noreply, NewState#state{caller = {handle_dir_result, Dir}}};

handle_ctrl_result({pos_compl, _}, #state{caller = {handle_dir_result, Dir,
						    Data}, client = From} 
		   = State) ->
    case Dir of
	"" -> % Current directory
	    gen_server:reply(From, {ok, Data}),
	    {noreply, State#state{client = undefined, 
				  caller = undefined}};
	_ ->
	    %% If there is only one line it might be a directory with on
	    %% file but it might be an error message that the directory
	    %% was not found. So in this case we have to endure a little
	    %% overhead to be able to give a good return value. Alas not
	    %% all ftp implementations behave the same and returning
	    %% an error string is allowed by the FTP RFC. 
	    case lists:dropwhile(fun(?CR) -> false;(_) -> true end, 
				 binary_to_list(Data)) of
		L when L == [?CR, ?LF]; L == [] ->	
		    send_ctrl_message(State, mk_cmd("PWD", [])),
		    activate_ctrl_connection(State),
		    {noreply, 
		     State#state{caller = {handle_dir_data, Dir, Data}}};
		_ ->
		    gen_server:reply(From, {ok, Data}),
		    {noreply, State#state{client = undefined,
					  caller = undefined}}
	    end
    end;

handle_ctrl_result({pos_compl, Lines}, 
		   #state{caller = {handle_dir_data, Dir, DirData}} = 
		   State) ->
    OldDir = pwd_result(Lines),    
    send_ctrl_message(State, mk_cmd("CWD ~s", [Dir])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {handle_dir_data_second_phase, OldDir,
				    DirData}}};
handle_ctrl_result({Status, _},
		   #state{caller = {handle_dir_data, _, _}} = State) ->
    ctrl_result_response(Status, State, {error, epath});

handle_ctrl_result(S={_Status, _},
		   #state{caller = {handle_dir_result, _, _}} = State) ->
    %% OTP-5731, macosx
    ctrl_result_response(S, State, {error, epath});

handle_ctrl_result({pos_compl, _},
		   #state{caller = {handle_dir_data_second_phase, OldDir, 
				    DirData}} = State) ->
    send_ctrl_message(State, mk_cmd("CWD ~s", [OldDir])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {handle_dir_data_third_phase, DirData}}};
handle_ctrl_result({Status, _}, 
		   #state{caller = {handle_dir_data_second_phase, _, _}} 
		   = State) ->
    ctrl_result_response(Status, State, {error, epath});
handle_ctrl_result(_, #state{caller = {handle_dir_data_third_phase, DirData},
			     client = From} = State) ->
    gen_server:reply(From, {ok, DirData}),
    {noreply, State#state{client = undefined, caller = undefined}};

handle_ctrl_result({Status, _}, #state{caller = cd} = State) ->
    ctrl_result_response(Status, State, {error, epath});

handle_ctrl_result(Status={epath, _}, #state{caller = {dir,_}} = State) ->
     ctrl_result_response(Status, State, {error, epath});

%%--------------------------------------------------------------------------
%% File renaming
handle_ctrl_result({pos_interm, _}, #state{caller = {rename, NewFile}} 
		   = State) ->
    send_ctrl_message(State, mk_cmd("RNTO ~s", [NewFile])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = rename_second_phase}}; 

handle_ctrl_result({Status, _}, 
		   #state{caller = {rename, _}} = State) ->
    ctrl_result_response(Status, State, {error, epath});

handle_ctrl_result({Status, _},
		   #state{caller = rename_second_phase} = State) ->
    ctrl_result_response(Status, State, {error, epath});

%%--------------------------------------------------------------------------
%% File handling - recv_bin
handle_ctrl_result({pos_prel, _}, #state{caller = recv_bin} = State) ->
    NewState = accept_data_connection(State),
    activate_data_connection(NewState),
    {noreply, NewState};

handle_ctrl_result({pos_compl, _}, #state{caller = {recv_bin, Data},
					  client = From} = State) ->
    gen_server:reply(From, {ok, Data}),
    close_data_connection(State),
    {noreply, State#state{client = undefined, caller = undefined}};

handle_ctrl_result({Status, _}, #state{caller = recv_bin} = State) ->
    close_data_connection(State),
    ctrl_result_response(Status, State#state{dsock = undefined}, 
			 {error, epath});

handle_ctrl_result({Status, _}, #state{caller = {recv_bin, _}} = State) ->
    close_data_connection(State),
    ctrl_result_response(Status, State#state{dsock = undefined}, 
			 {error, epath});
%%--------------------------------------------------------------------------
%% File handling - start_chunk_transfer
handle_ctrl_result({pos_prel, _}, #state{client = From,
					 caller = start_chunk_transfer}
		   = State) ->
    NewState = accept_data_connection(State),
    gen_server:reply(From, ok),
    {noreply, NewState#state{chunk = true, client = undefined,
			     caller = undefined}};
%%--------------------------------------------------------------------------
%% File handling - recv_file
handle_ctrl_result({pos_prel, _}, #state{caller = {recv_file, _}} = State) ->
    NewState = accept_data_connection(State),
    activate_data_connection(NewState),
    {noreply, NewState};

handle_ctrl_result({Status, _}, #state{caller = {recv_file, Fd}} = State) ->
    file_close(Fd),
    close_data_connection(State),
    ctrl_result_response(Status, State#state{dsock = undefined}, 
			 {error, epath});
%%--------------------------------------------------------------------------
%% File handling - transfer_*
handle_ctrl_result({pos_prel, _}, #state{caller = {transfer_file, Fd}} 
		   = State) ->
    NewState = accept_data_connection(State),
    send_file(Fd, NewState); 

handle_ctrl_result({pos_prel, _}, #state{caller = {transfer_data, Bin}} 
		   = State) ->
    NewState = accept_data_connection(State),
    send_data_message(NewState, Bin),
    close_data_connection(NewState),
    activate_ctrl_connection(NewState),
    {noreply, NewState#state{caller = transfer_data_second_phase,
			     dsock = undefined}};
%%--------------------------------------------------------------------------
%% Default
handle_ctrl_result({Status, Lines}, #state{client = From} = State) 
  when From =/= undefined ->
    ctrl_result_response(Status, State, {error, Lines}).

%%--------------------------------------------------------------------------
%% Help functions to handle_ctrl_result
%%--------------------------------------------------------------------------
ctrl_result_response(pos_compl, #state{client = From} = State, _)  ->
    gen_server:reply(From, ok),
    {noreply, State#state{client = undefined, caller = undefined}};

ctrl_result_response(Status, #state{client = From} = State, _) when
Status == etnospc; Status == epnospc; Status == efnamena; Status == econn ->
%Status == etnospc; Status == epnospc; Status == econn ->
    gen_server:reply(From, {error, Status}),
%%    {stop, normal, {error, Status}, State#state{client = undefined}};
    {stop, normal, State#state{client = undefined}};

ctrl_result_response(_, #state{client = From} = State, ErrorMsg) ->
    gen_server:reply(From, ErrorMsg),
    {noreply, State#state{client = undefined, caller = undefined}}.

%%--------------------------------------------------------------------------
handle_caller(#state{caller = {dir, Dir, Len}} = State) ->
    Cmd = case Len of
	      short -> "NLST";
	      long -> "LIST"
	  end,
    case Dir of 
	"" ->
	    send_ctrl_message(State, mk_cmd(Cmd, ""));
	_ ->
	    send_ctrl_message(State, mk_cmd(Cmd ++ " ~s", [Dir]))
    end,
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {dir, Dir}}};
     
handle_caller(#state{caller = {recv_bin, RemoteFile}} = State) ->
    send_ctrl_message(State, mk_cmd("RETR ~s", [RemoteFile])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = recv_bin}};

handle_caller(#state{caller = {start_chunk_transfer, Cmd, RemoteFile}} = 
	      State) ->
    send_ctrl_message(State, mk_cmd("~s ~s", [Cmd, RemoteFile])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = start_chunk_transfer}};

handle_caller(#state{caller = {recv_file, RemoteFile, Fd}} = State) ->
    send_ctrl_message(State, mk_cmd("RETR ~s", [RemoteFile])), 
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {recv_file, Fd}}};

handle_caller(#state{caller = {transfer_file, {Cmd, LocalFile, RemoteFile}},
		     ldir = LocalDir, client = From} = State) ->
    case file_open(filename:absname(LocalFile, LocalDir), read) of
	{ok, Fd} ->
	    send_ctrl_message(State, mk_cmd("~s ~s", [Cmd, RemoteFile])),
	    activate_ctrl_connection(State),
	    {noreply, State#state{caller = {transfer_file, Fd}}};
	{error, _} ->
	    gen_server:reply(From, {error, epath}),
	    {noreply, State#state{client = undefined, caller = undefined,
				  dsock = undefined}} 
    end;

handle_caller(#state{caller = {transfer_data, {Cmd, Bin, RemoteFile}}} = 
	      State) ->
    send_ctrl_message(State, mk_cmd("~s ~s", [Cmd, RemoteFile])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {transfer_data, Bin}}}.

%%  ----------- FTP SERVER COMMUNICATION  ------------------------- 

%% Connect to FTP server at Host (default is TCP port 21) 
%% in order to establish a control connection.
setup_ctrl_connection(Host, Port, Timeout, State)->
    MsTime = millisec_time(),
    case connect(Host, Port, Timeout, State) of
	{Ipv, {ok, CSock}} ->
	    NewState = 
		case Ipv of
		    ipv4 -> 
			State#state{csock = CSock, ip_v6_disabled = true};
		    ipv6 ->
			State#state{csock = CSock}
		end,
	    activate_ctrl_connection(NewState),
	    case Timeout - (millisec_time() - MsTime) of
		Timeout2 when (Timeout2 >= 0) ->
		    {noreply, NewState#state{caller = open}, Timeout2};
		_ ->
		    %% Oups: Simulate timeout
		    self() ! timeout,
		    {noreply, NewState#state{caller = open}}
	    end;
	{_,{error, _}} ->
	    gen_server:reply(State#state.client, {error, ehost}),
	    {stop, normal, State#state{client = undefined}}
    end.

setup_data_connection(#state{mode   = active, 
			     caller = Caller, 
			     csock  = CSock} = State) ->    
    IntToString = fun(Element) -> integer_to_list(Element) end,
    
    case (catch inet:sockname(CSock)) of
	{ok, {{_, _, _, _, _, _, _, _} = IP, _}} ->
	    {ok, LSock} = 
		gen_tcp:listen(0, [{ip, IP}, {active, false},
				   inet6, binary, {packet, 0}]),
	    {ok, Port} = inet:port(LSock),
	    Cmd = mk_cmd("EPRT |2|~s:~s:~s:~s:~s:~s:~s:~s|~s|", 
			 lists:map(IntToString, 
				   tuple_to_list(IP) ++ [Port])),
	    send_ctrl_message(State, Cmd),
	    activate_ctrl_connection(State),  
	    {noreply, State#state{caller = {setup_data_connection, 
					    {LSock, Caller}}}};
	{ok, {{_,_,_,_} = IP, _}} ->	    
	    {ok, LSock} = gen_tcp:listen(0, [{ip, IP}, {active, false},
					     binary, {packet, 0}]),
	    {ok, Port} = inet:port(LSock),
	    {IP1, IP2, IP3, IP4} = IP,
	    {Port1, Port2} = {Port div 256, Port rem 256},
	    send_ctrl_message(State, 
			      mk_cmd("PORT ~w,~w,~w,~w,~w,~w",
				     [IP1, IP2, IP3, IP4, Port1, Port2])),
	    activate_ctrl_connection(State),
	    {noreply, State#state{caller = {setup_data_connection, 
					    {LSock, Caller}}}}
    end;

setup_data_connection(#state{mode = passive, ip_v6_disabled = false,
			     caller = Caller} = State) ->
    send_ctrl_message(State, mk_cmd("EPSV", [])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {setup_data_connection, Caller}}};

setup_data_connection(#state{mode = passive, ip_v6_disabled = true,
			     caller = Caller} = State) ->
    send_ctrl_message(State, mk_cmd("PASV", [])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {setup_data_connection, Caller}}}.

connect(Host = {_,_,_,_}, Port, TimeOut, _) ->
    {ipv4, gen_tcp:connect(Host, Port,[binary, {packet, 0}, {active, false}] ,
		    TimeOut)};
connect(Host = {_,_,_,_,_,_,_,_}, Port, TimeOut, 
	#state{ip_v6_disabled = false}) ->
    {ipv6, gen_tcp:connect(Host, Port,
		    [binary, {packet, 0}, {active, false}, inet6],
		    TimeOut)};    
connect(Host, Port, TimeOut, #state{ip_v6_disabled = false}) ->
    {Opts, NewHost, Ipv} = 
	case (inet:getaddr(Host, inet6)) of
	    %% If an ipv4-mapped ipv6 address is returned 
	    %% use ipv4 directly as some ftp-servers does not
	    %% handle "ip4-ipv6-compatiblity" mode well!
	    {ok, IP = {0, 0, 0, 0, 0, 16#ffff, _, _}} ->
		case inet:getaddr(Host, inet) of
		    {ok,NewIP} -> 
			{[binary, {packet, 0}, {active, false}], NewIP, ipv4};
		    _Error ->
			{[binary, {packet, 0}, {active, false}, inet6], 
			 IP,ipv6}
		end;
	    {ok, IP} ->
		{[binary, {packet, 0}, {active, false}, inet6], IP, ipv6};
	    {error, _} ->
		{[binary, {packet, 0}, {active, false}], Host, ipv4}
	end,
    {Ipv, gen_tcp:connect(NewHost, Port, Opts, TimeOut)};

connect(Host, Port, TimeOut, #state{ip_v6_disabled = true}) -> 
    Opts = [binary, {packet, 0}, {active, false}],
    {ipv4, gen_tcp:connect(Host, Port, Opts, TimeOut)}.

accept_data_connection(#state{mode = active,
			      dsock = {lsock, LSock}} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    gen_tcp:close(LSock),
    State#state{dsock = Socket};

accept_data_connection(#state{mode = passive} = State) ->
    State.

send_ctrl_message(#state{csock = Socket,verbose=Verbose}, Message) ->
%    io:format("Sending: ~p~n",[Message]),
    verbose(lists:flatten(Message),Verbose,send),
    send_message(Socket, Message).

send_data_message(#state{dsock = Socket}, Message) ->
    send_message(Socket, Message).

send_message(Socket, Message) ->
    case gen_tcp:send(Socket, Message) of
	ok ->
	    ok;
	{error, Reason} ->
	    error_logger:error_report("gen_tcp:send/2 failed for "
				      "reason ~p~n", [Reason]),
	    %% If tcp does not work the only option is to terminate,
	    %% this is the expected behavior under these circumstances.
	    exit(normal) %% User will get error message from terminate/2
    end.

activate_ctrl_connection(#state{csock = Socket, ctrl_data = {<<>>, _, _}}) ->
    activate_connection(Socket);
activate_ctrl_connection(#state{csock = Socket}) ->
    %% We have already received at least part of the next control message,
    %% that has been saved in ctrl_data, process this first.
    self() ! {tcp, Socket, <<>>}.

activate_data_connection(#state{dsock = Socket}) ->
    activate_connection(Socket).

activate_connection(Socket) ->
    inet:setopts(Socket, [{active, once}]).

close_ctrl_connection(#state{csock = undefined}) ->
    ok;
close_ctrl_connection(#state{csock = Socket}) ->
    close_connection(Socket).

close_data_connection(#state{dsock = undefined}) ->
    ok;
close_data_connection(#state{dsock = {lsock, Socket}}) ->
    close_connection(Socket);
close_data_connection(#state{dsock = Socket}) ->
    close_connection(Socket).

close_connection(Socket) ->
    gen_tcp:close(Socket).

%%  ------------ FILE HANDELING  ----------------------------------------   

send_file(Fd, State) ->
    case file_read(Fd) of
	{ok, N, Bin} when N > 0->
	    send_data_message(State, Bin),
	    progress_report({binary, Bin}, State),
	    send_file(Fd, State);
	{ok, _, _} ->
	    file_close(Fd),
	    close_data_connection(State),
	    progress_report({transfer_size, 0}, State),
	    activate_ctrl_connection(State),
	    {noreply, State#state{caller = transfer_file_second_phase,
				  dsock = undefined}};
        {error, Reason} ->
	    gen_server:reply(State#state.client, {error, Reason}),
	    {stop, normal, State#state{client = undefined}}
    end.

file_open(File, Option) ->
  file:open(File, [raw, binary, Option]).

file_close(Fd) ->
  file:close(Fd).

file_read(Fd) ->				
    case file:read(Fd, ?FILE_BUFSIZE) of
	{ok, Bytes} ->
	    {ok, size(Bytes), Bytes};
	eof ->
	    {ok, 0, []};
	Other ->
	    Other
    end.

file_write(Bytes, Fd) ->
    file:write(Fd, Bytes).

%% --------------  MISC ---------------------------------------------- 

call(GenServer, Msg, Format) ->
    call(GenServer, Msg, Format, infinity).
call(GenServer, Msg, Format, Timeout) ->   

    Result = (catch gen_server:call(GenServer, {self(), Msg}, Timeout)),

    case Result of
	{ok, Bin} when binary(Bin), Format == string ->
	    {ok, binary_to_list(Bin)};
	{'EXIT', _} ->
	    {error, eclosed};
	Result ->
	    Result
    end.

cast(GenServer, Msg) ->
    gen_server:cast(GenServer, {self(), Msg}).

mk_cmd(Fmt, Args) ->
    [io_lib:format(Fmt, Args)| [?CR, ?LF]].		% Deep list ok.

pwd_result(Lines) ->
    {_, [?DOUBLE_QUOTE | Rest]} = 
	lists:splitwith(fun(?DOUBLE_QUOTE) -> false; (_) -> true end, Lines),
    {Dir, _} =
	lists:splitwith(fun(?DOUBLE_QUOTE) -> false; (_) -> true end, Rest),
    Dir.

is_verbose(Params) -> 
    check_param(verbose, Params).

is_debug(Flags) -> 
    check_param(debug, Flags).

is_trace(Flags) -> 
    check_param(trace, Flags).

is_ipv6_disabled(Flags) -> 
    check_param(ip_v6_disabled, Flags).

check_param(Param, Params) -> 
    lists:member(Param, Params).

key_search(Key, List, Default)->	     
    case lists:keysearch(Key, 1, List) of
	{value, {_,Val}} ->
	    Val;
	false ->
	    Default
    end.

check_option(Pred, Value, Default) ->
    case Pred(Value) of
	true ->
	    Value;
	false ->
	    Default
    end.

verbose(Lines, true, Direction) ->
    DirStr =
	case Direction of
	    send ->
		"Sending: ";
	    _ ->
		"Receiving: "
	end,
    Str = string:strip(string:strip(Lines, right, ?LF), right, ?CR),
    erlang:display(DirStr++Str);
verbose(_, false,_) ->
    ok.

ensure_started() ->
    %% Start of the inets application should really be handled by the 
    %% application using inets. 
    case application:start(inets) of
	{error,{already_started,inets}} ->
	    ok;
	{error,{{already_started, _}, % Started as an included application
		{inets_app,start, _}}} ->
	    ok;
	ok ->
	    error_logger:info_report("The inets application was not started."
				     " Has now been started as a temporary" 
				     " application.")
    end.

progress(Options) ->
    ftp_progress:start_link(Options).

progress_report(_, #state{progress = ignore}) ->
    ok;
progress_report(stop, #state{progress = ProgressPid}) ->
    ftp_progress:stop(ProgressPid);
progress_report({binary, Data}, #state{progress = ProgressPid}) ->
    ftp_progress:report(ProgressPid, {transfer_size, size(Data)});
progress_report(Report,  #state{progress = ProgressPid}) ->
    ftp_progress:report(ProgressPid, Report).


millisec_time() ->
    {A,B,C} = erlang:now(),
    A*1000000000+B*1000+(C div 1000).

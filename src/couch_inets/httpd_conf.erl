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
-module(httpd_conf).

%% EWSAPI 
-export([is_directory/1, is_file/1, make_integer/1, clean/1, 
	 custom_clean/3, check_enum/2]).

%% Application internal API
-export([load/1, load/2, load_mime_types/1, store/1, store/2,
	remove/1, remove_all/1, config/1]).

-define(VMODULE,"CONF").
-include("httpd.hrl").

%%%=========================================================================
%%%  EWSAPI
%%%=========================================================================
%%-------------------------------------------------------------------------
%%  is_directory(FilePath) -> Result
%%	FilePath = string()
%%      Result = {ok,Directory} | {error,Reason}
%%      Directory = string()
%%      Reason = string() | enoent | eaccess | enotdir | FileInfo
%%      FileInfo = File info record
%%
%% Description: Checks if FilePath is a directory in which case it is
%% returned. 
%%-------------------------------------------------------------------------
is_directory(Directory) ->
    case file:read_file_info(Directory) of
	{ok,FileInfo} ->
	    #file_info{type = Type, access = Access} = FileInfo,
	    is_directory(Type,Access,FileInfo,Directory);
	{error,Reason} ->
	    {error,Reason}
    end.
is_directory(directory,read,_FileInfo,Directory) ->
    {ok,Directory};
is_directory(directory,read_write,_FileInfo,Directory) ->
    {ok,Directory};
is_directory(_Type,_Access,FileInfo,_Directory) ->
    {error,FileInfo}.
%%-------------------------------------------------------------------------
%% is_file(FilePath) -> Result
%%	FilePath = string()
%%      Result = {ok,File} | {error,Reason}
%%      File = string()
%%      Reason = string() | enoent | eaccess | enotdir | FileInfo
%%      FileInfo = File info record
%%
%% Description: Checks if FilePath is a regular file in which case it
%% is returned.
%%-------------------------------------------------------------------------
is_file(File) ->
    case file:read_file_info(File) of
	{ok,FileInfo} ->
	    #file_info{type = Type, access = Access} = FileInfo,
	    is_file(Type,Access,FileInfo,File);
	{error,Reason} ->
	    {error,Reason}
    end.
is_file(regular,read,_FileInfo,File) ->
    {ok,File};
is_file(regular,read_write,_FileInfo,File) ->
    {ok,File};
is_file(_Type,_Access,FileInfo,_File) ->
    {error,FileInfo}.
%%-------------------------------------------------------------------------
%% make_integer(String) -> Result
%% String = string()
%% Result = {ok,integer()} | {error,nomatch}
%%
%% Description: make_integer/1 returns an integer representation of String. 
%%-------------------------------------------------------------------------
make_integer(String) ->
    case regexp:match(clean(String),"[0-9]+") of
	{match, _, _} ->
	    {ok, list_to_integer(clean(String))};
	nomatch ->
	    {error, nomatch}
    end.
%%-------------------------------------------------------------------------
%% clean(String) -> Stripped
%% String = Stripped = string()
%%
%% Description:clean/1 removes leading and/or trailing white spaces
%% from String.
%%-------------------------------------------------------------------------
clean(String) ->
    {ok,CleanedString,_} = 
	regexp:gsub(String, "^[ \t\n\r\f]*|[ \t\n\r\f]*\$",""),
    CleanedString.
%%-------------------------------------------------------------------------
%% custom_clean(String,Before,After) -> Stripped
%% Before = After = regexp()
%% String = Stripped = string()
%%
%% Description: custom_clean/3 removes leading and/or trailing white
%% spaces and custom characters from String. 
%%-------------------------------------------------------------------------
custom_clean(String,MoreBefore,MoreAfter) ->
    {ok,CleanedString,_} = regexp:gsub(String,"^[ \t\n\r\f"++MoreBefore++
				       "]*|[ \t\n\r\f"++MoreAfter++"]*\$",""),
    CleanedString.
%%-------------------------------------------------------------------------
%% check_enum(EnumString,ValidEnumStrings) -> Result
%%	EnumString = string()
%%      ValidEnumStrings = [string()]
%%      Result = {ok,atom()} | {error,not_valid}
%%
%% Description: check_enum/2 checks if EnumString is a valid
%% enumeration of ValidEnumStrings in which case it is returned as an
%% atom.
%%-------------------------------------------------------------------------
check_enum(_Enum,[]) ->
    {error, not_valid};
check_enum(Enum,[Enum|_Rest]) ->
    {ok, list_to_atom(Enum)};
check_enum(Enum, [_NotValid|Rest]) ->
    check_enum(Enum, Rest).

%%%=========================================================================
%%%  Application internal API
%%%=========================================================================
%% The configuration data is handled in three (3) phases:
%% 1. Parse the config file and put all directives into a key-vale
%%    tuple list (load/1). 
%% 2. Traverse the key-value tuple list store it into an ETS table.
%%    Directives depending on other directives are taken care of here
%%    (store/1).
%% 3. Traverse the ETS table and do a complete clean-up (remove/1).

%% Phase 1: Load
load(ConfigFile) ->
    case read_config_file(ConfigFile) of
	{ok, Config} ->
	    case bootstrap(Config) of
		{error, Reason} ->
		    {error, Reason};
		{ok, Modules} ->
		    load_config(Config, lists:append(Modules, [?MODULE]))
	    end;
	{error, Reason} ->
	    {error, ?NICE("Error while reading config file: "++Reason)}
    end.

load(eof, []) ->
    eof;
load("MaxHeaderSize " ++ MaxHeaderSize, []) ->
    case make_integer(MaxHeaderSize) of
        {ok, Integer} ->
            {ok, [], {max_header_size,Integer}};
        {error, _} ->
            {error, ?NICE(clean(MaxHeaderSize)++
                          " is an invalid number of MaxHeaderSize")}
    end;
load("MaxHeaderAction " ++ Action, []) ->
    {ok, [], {max_header_action,list_to_atom(clean(Action))}};
load("MaxBodySize " ++ MaxBodySize, []) ->
    case make_integer(MaxBodySize) of
        {ok, Integer} ->
            {ok, [], {max_body_size,Integer}};
        {error, _} ->
            {error, ?NICE(clean(MaxBodySize)++
                          " is an invalid number of MaxBodySize")}
    end;
load("MaxBodyAction " ++ Action, []) ->
    {ok, [], {max_body_action,list_to_atom(clean(Action))}};
load("ServerName " ++ ServerName, []) ->
    {ok,[],{server_name,clean(ServerName)}};
load("SocketType " ++ SocketType, []) ->
    case check_enum(clean(SocketType),["ssl","ip_comm"]) of
	{ok, ValidSocketType} ->
	    {ok, [], {com_type,ValidSocketType}};
	{error,_} ->
	    {error, ?NICE(clean(SocketType) ++ " is an invalid SocketType")}
    end;
load("Port " ++ Port, []) ->
    case make_integer(Port) of
	{ok, Integer} ->
	    {ok, [], {port,Integer}};
	{error, _} ->
	    {error, ?NICE(clean(Port)++" is an invalid Port")}
    end;
load("BindAddress " ++ Address, []) ->
    %% If an ipv6 address is provided in URL-syntax strip the
    %% url specific part e.i. "[FEDC:BA98:7654:3210:FEDC:BA98:7654:3210]"
    %% -> "FEDC:BA98:7654:3210:FEDC:BA98:7654:3210"
    NewAddress = string:strip(string:strip(clean(Address), 
					   left, $[), 
			      right, $]),
    case NewAddress of
	"*" ->
	    {ok, [], {bind_address,any}};
	CAddress ->
	    case (catch inet:getaddr(CAddress,inet6)) of
		{ok, {0, 0, 0, 0, 0, 16#ffff, _, _}} ->
		    case inet:getaddr(CAddress, inet) of
			{ok, IPAddr} ->
			    {ok, [], {bind_address,IPAddr}};
			{error, _} ->
			    {error, ?NICE(CAddress++" is an invalid address")}
		    end;
		{ok, IPAddr} ->
		    {ok, [], {bind_address, IPAddr}};
		_ ->
		    case inet:getaddr(CAddress, inet) of
			{ok, IPAddr} ->
			    {ok, [], {bind_address,IPAddr}};
			{error, _} ->
			    {error, ?NICE(CAddress++" is an invalid address")}
		    end
	    end
    end;
load("KeepAlive " ++ OnorOff, []) ->
    case list_to_atom(clean(OnorOff)) of
	off ->
	    {ok, [], {persistent_conn, false}};
	_ ->
	    {ok, [], {persistent_conn, true}}
    end;
load("MaxKeepAliveRequests " ++  MaxRequests, []) ->
    case make_integer(MaxRequests) of
	{ok, Integer} ->
	    {ok, [], {max_keep_alive_request, Integer}};
	{error, _} ->
	    {error, ?NICE(clean(MaxRequests) ++
			  " is an invalid MaxKeepAliveRequests")}
    end;
%% This clause is keept for backwards compability 
load("MaxKeepAliveRequest " ++  MaxRequests, []) ->
    case make_integer(MaxRequests) of
	{ok, Integer} ->
	    {ok, [], {max_keep_alive_request, Integer}};
	{error, _} ->
	    {error, ?NICE(clean(MaxRequests) ++
			  " is an invalid MaxKeepAliveRequest")}
    end;
load("KeepAliveTimeout " ++ Timeout, []) ->
    case make_integer(Timeout) of
	{ok, Integer} ->
	    {ok, [], {keep_alive_timeout, Integer*1000}};
	{error, _} ->
	    {error, ?NICE(clean(Timeout)++" is an invalid KeepAliveTimeout")}
    end;
load("Modules " ++ Modules, []) ->
    {ok, ModuleList} = regexp:split(Modules," "),
    {ok, [], {modules,[list_to_atom(X) || X <- ModuleList]}};
load("ServerAdmin " ++ ServerAdmin, []) ->
    {ok, [], {server_admin,clean(ServerAdmin)}};
load("ServerRoot " ++ ServerRoot, []) ->
    case is_directory(clean(ServerRoot)) of
	{ok, Directory} ->
	    MimeTypesFile = 
		filename:join([clean(ServerRoot),"conf", "mime.types"]),
	    case load_mime_types(MimeTypesFile) of
		{ok, MimeTypesList} ->
		    {ok, [], [{server_root,string:strip(Directory,right,$/)},
			      {mime_types,MimeTypesList}]};
		{error, Reason} ->
		    {error, Reason}
	    end;
	{error, _} ->
	    {error, ?NICE(clean(ServerRoot)++" is an invalid ServerRoot")}
    end;
load("MaxClients " ++ MaxClients, []) ->
    case make_integer(MaxClients) of
	{ok, Integer} ->
	    {ok, [], {max_clients,Integer}};
	{error, _} ->
	    {error, ?NICE(clean(MaxClients) ++
			  " is an invalid number of MaxClients")}
    end;
load("DocumentRoot " ++ DocumentRoot,[]) ->
    case is_directory(clean(DocumentRoot)) of
	{ok, Directory} ->
	    {ok, [], {document_root,string:strip(Directory,right,$/)}};
	{error, _} ->
	    {error, ?NICE(clean(DocumentRoot)++"is an invalid DocumentRoot")}
    end;
load("DefaultType " ++ DefaultType, []) ->
    {ok, [], {default_type,clean(DefaultType)}};
load("SSLCertificateFile " ++ SSLCertificateFile, []) ->
    case is_file(clean(SSLCertificateFile)) of
	{ok, File} ->
	    {ok, [], {ssl_certificate_file,File}};
    {error, _} ->
	    {error, ?NICE(clean(SSLCertificateFile)++
			  " is an invalid SSLCertificateFile")}
    end;
load("SSLCertificateKeyFile " ++ SSLCertificateKeyFile, []) ->
    case is_file(clean(SSLCertificateKeyFile)) of
	{ok, File} ->
	    {ok, [], {ssl_certificate_key_file,File}};
	{error, _} ->
	    {error, ?NICE(clean(SSLCertificateKeyFile)++
			  " is an invalid SSLCertificateKeyFile")}
    end;
load("SSLVerifyClient " ++ SSLVerifyClient, []) ->
    case make_integer(clean(SSLVerifyClient)) of
	{ok, Integer} when Integer >=0,Integer =< 2 ->
	    {ok, [], {ssl_verify_client,Integer}};
	{ok, _Integer} ->
	    {error,?NICE(clean(SSLVerifyClient) ++
			 " is an invalid SSLVerifyClient")};
	{error, nomatch} ->
	    {error,?NICE(clean(SSLVerifyClient) ++ 
			 " is an invalid SSLVerifyClient")}
    end;
load("SSLVerifyDepth " ++ SSLVerifyDepth, []) ->
    case make_integer(clean(SSLVerifyDepth)) of
	{ok, Integer} when Integer > 0 ->
	    {ok, [], {ssl_verify_client_depth,Integer}};
	{ok, _Integer} ->
	    {error,?NICE(clean(SSLVerifyDepth) ++
			 " is an invalid SSLVerifyDepth")};
	{error, nomatch} ->
	    {error,?NICE(clean(SSLVerifyDepth) ++
			 " is an invalid SSLVerifyDepth")}
    end;
load("SSLCiphers " ++ SSLCiphers, []) ->
    {ok, [], {ssl_ciphers, clean(SSLCiphers)}};
load("SSLCACertificateFile " ++ SSLCACertificateFile, []) ->
    case is_file(clean(SSLCACertificateFile)) of
	{ok, File} ->
	    {ok, [], {ssl_ca_certificate_file,File}};
	{error, _} ->
	    {error, ?NICE(clean(SSLCACertificateFile)++
			  " is an invalid SSLCACertificateFile")}
    end;
load("SSLPasswordCallbackModule " ++ SSLPasswordCallbackModule, []) ->
    {ok, [], {ssl_password_callback_module,
	      list_to_atom(clean(SSLPasswordCallbackModule))}};
load("SSLPasswordCallbackFunction " ++ SSLPasswordCallbackFunction, []) ->
    {ok, [], {ssl_password_callback_function,
	      list_to_atom(clean(SSLPasswordCallbackFunction))}};
load("DisableChunkedTransferEncodingSend " ++ TrueOrFalse, []) ->
    case list_to_atom(clean(TrueOrFalse)) of
	true ->
	    {ok, [], {disable_chunked_transfer_encoding_send, true}};
	_ ->
	    {ok, [], {disable_chunked_transfer_encoding_send, false}}
    end.

%%
%% load_mime_types/1 -> {ok, MimeTypes} | {error, Reason}
%%
load_mime_types(MimeTypesFile) ->
    case file:open(MimeTypesFile, read) of
	{ok, Stream} ->
	    parse_mime_types(Stream, []);
	{error, _} ->
	    {error, ?NICE("Can't open " ++ MimeTypesFile)}
    end.

%% Phase 2: Store
store(ConfigList) ->
    Modules = httpd_util:key1search(ConfigList, modules, []),
    Port = httpd_util:key1search(ConfigList, port),
    Addr = httpd_util:key1search(ConfigList,bind_address),
    Name = httpd_util:make_name("httpd_conf",Addr,Port),
    ConfigDB = ets:new(Name, [named_table, bag, protected]),
    store(ConfigDB, ConfigList, lists:append(Modules,[?MODULE]),ConfigList).

store({mime_types,MimeTypesList},ConfigList) ->
    Port = httpd_util:key1search(ConfigList, port),
    Addr = httpd_util:key1search(ConfigList, bind_address),
    Name = httpd_util:make_name("httpd_mime",Addr,Port),
    {ok, MimeTypesDB} = store_mime_types(Name,MimeTypesList),
    {ok, {mime_types,MimeTypesDB}};
store(ConfigListEntry, _ConfigList) ->
    {ok, ConfigListEntry}.

%% Phase 3: Remove
remove_all(ConfigDB) ->
    Modules = httpd_util:lookup(ConfigDB,modules,[]),
    remove_traverse(ConfigDB, lists:append(Modules,[?MODULE])).

remove(ConfigDB) ->
    ets:delete(ConfigDB),
    ok.

config(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,com_type,ip_comm) of
	ssl ->
	    case ssl_certificate_file(ConfigDB) of
		undefined ->
		    {error,
		     "Directive SSLCertificateFile "
		     "not found in the config file"};
		SSLCertificateFile ->
		    {ssl,
		     SSLCertificateFile++
		     ssl_certificate_key_file(ConfigDB)++
		     ssl_verify_client(ConfigDB)++
		     ssl_ciphers(ConfigDB)++
		     ssl_password(ConfigDB)++
		     ssl_verify_depth(ConfigDB)++
		     ssl_ca_certificate_file(ConfigDB)}
	    end;
	ip_comm ->
	    ip_comm
    end.

%%%========================================================================
%%% Internal functions
%%%========================================================================
%%% Phase 1 Load:
bootstrap([]) ->
    {error, ?NICE("Modules must be specified in the config file")};
bootstrap([Line|Config]) ->
    case Line of
	"Modules " ++ Modules ->
	    {ok, ModuleList} = regexp:split(Modules," "),
	    TheMods = [list_to_atom(X) || X <- ModuleList],
	    case verify_modules(TheMods) of
		ok ->
		    {ok, TheMods};
		{error, Reason} ->
		    {error, Reason}
	    end;
	_ ->
	    bootstrap(Config)
    end.

load_config(Config, Modules) ->
    %% Create default contexts for all modules
    Contexts = lists:duplicate(length(Modules), []),
    load_config(Config, Modules, Contexts, []).
load_config([], _Modules, _Contexts, ConfigList) ->
    case a_must(ConfigList, [server_name,port,server_root,document_root]) of
	ok ->
	    {ok, ConfigList};
	{missing, Directive} ->
	    {error, ?NICE(atom_to_list(Directive)++
			  " must be specified in the config file")}
    end;
load_config([Line|Config], Modules, Contexts, ConfigList) ->
    case load_traverse(Line, Contexts, Modules, [], ConfigList, no) of
	{ok, NewContexts, NewConfigList} ->
	    load_config(Config, Modules, NewContexts, NewConfigList);
	{error, Reason} -> 
	    {error, Reason}
    end.


%% This loads the config file into each module specified by Modules
%% Each module has its own context that is passed to and (optionally)
%% returned by the modules load function. The module can also return
%% a ConfigEntry, which will be added to the global configuration
%% list.
%% All configuration directives are guaranteed to be passed to all
%% modules. Each module only implements the function clauses of
%% the load function for the configuration directives it supports,
%% it's ok if an apply returns {'EXIT', {function_clause, ..}}.
load_traverse(Line, [], [], _NewContexts, _ConfigList, no) ->
    {error, ?NICE("Configuration directive not recognized: "++Line)};
load_traverse(_Line, [], [], NewContexts, ConfigList, yes) ->
    {ok, lists:reverse(NewContexts), ConfigList};
load_traverse(Line, [Context|Contexts], [Module|Modules], NewContexts,
	      ConfigList, State) ->
    case catch apply(Module, load, [Line, Context]) of
	{'EXIT', {function_clause, _}} ->
	    load_traverse(Line, Contexts, Modules, 
			  [Context|NewContexts], ConfigList, State);
	{'EXIT',{undef, _}} ->
	    load_traverse(Line, Contexts, Modules,
			  [Context|NewContexts], ConfigList,yes);
	{'EXIT', Reason} ->
	    error_logger:error_report({'EXIT', Reason}),
	    load_traverse(Line, Contexts, Modules, 
			  [Context|NewContexts], ConfigList, State);
	{ok, NewContext} ->
	    load_traverse(Line, Contexts, Modules, 
			  [NewContext|NewContexts], ConfigList,yes);
	{ok, NewContext, ConfigEntry} when tuple(ConfigEntry) ->
	    load_traverse(Line, Contexts, Modules, [NewContext|NewContexts],
			  [ConfigEntry|ConfigList], yes);
	{ok, NewContext, ConfigEntry} when list(ConfigEntry) ->
	    load_traverse(Line, Contexts, Modules, [NewContext|NewContexts],
			  lists:append(ConfigEntry, ConfigList), yes);
	{error, Reason} ->
	    {error, Reason}
    end.
	
%% Verifies that all specified modules are available.
verify_modules([]) ->
    ok;
verify_modules([Mod|Rest]) ->
    case code:which(Mod) of
	non_existing ->
	    {error, ?NICE(atom_to_list(Mod)++" does not exist")};
	_Path ->
	    verify_modules(Rest)
    end.

%% Reads the entire configuration file and returns list of strings or
%% and error.
read_config_file(FileName) ->
    case file:open(FileName, read) of
	{ok, Stream} ->
	    read_config_file(Stream, []);
	{error, _Reason} ->
	    {error, ?NICE("Cannot open "++FileName)}
    end.
read_config_file(Stream, SoFar) ->
    case io:get_line(Stream, []) of
	eof ->
	    file:close(Stream),
	    {ok, lists:reverse(SoFar)};
	{error, Reason} ->
	    file:close(Stream),
	    {error, Reason};
	[$#|_Rest] ->
	    %% Ignore commented lines for efficiency later ..
	    read_config_file(Stream, SoFar);
	Line ->
	    {ok, NewLine, _}=regexp:sub(clean(Line),"[\t\r\f ]"," "),
	    case NewLine of
		[] ->
		    %% Also ignore empty lines ..
		    read_config_file(Stream, SoFar);
		_Other ->
		    read_config_file(Stream, [NewLine|SoFar])
	    end
    end.

parse_mime_types(Stream,MimeTypesList) ->
    Line=
	case io:get_line(Stream,'') of
	    eof ->
		eof;
	    String ->
		clean(String)
	end,
    parse_mime_types(Stream, MimeTypesList, Line).
parse_mime_types(Stream, MimeTypesList, eof) ->
    file:close(Stream),
    {ok, MimeTypesList};
parse_mime_types(Stream, MimeTypesList, "") ->
    parse_mime_types(Stream, MimeTypesList);
parse_mime_types(Stream, MimeTypesList, [$#|_]) ->
    parse_mime_types(Stream, MimeTypesList);
parse_mime_types(Stream, MimeTypesList, Line) ->
    case regexp:split(Line, " ") of
	{ok, [NewMimeType|Suffixes]} ->
	    parse_mime_types(Stream,
			     lists:append(suffixes(NewMimeType,Suffixes),
					  MimeTypesList));
	{ok, _} ->
	    {error, ?NICE(Line)}
    end.

suffixes(_MimeType,[]) ->
    [];
suffixes(MimeType,[Suffix|Rest]) ->
    [{Suffix,MimeType}|suffixes(MimeType,Rest)].

a_must(_ConfigList,[]) ->
    ok;
a_must(ConfigList,[Directive|Rest]) ->
    case httpd_util:key1search(ConfigList,Directive) of
	undefined ->
	    {missing,Directive};
	_ ->
	    a_must(ConfigList,Rest)
    end.

%% Pahse 2: store
store(ConfigDB, _ConfigList, _Modules,[]) ->
    {ok, ConfigDB};
store(ConfigDB, ConfigList, Modules, [ConfigListEntry|Rest]) ->
    case store_traverse(ConfigListEntry,ConfigList,Modules) of
	{ok, ConfigDBEntry} when tuple(ConfigDBEntry) ->
	    ets:insert(ConfigDB,ConfigDBEntry),
	    store(ConfigDB,ConfigList,Modules,Rest);
	{ok, ConfigDBEntry} when list(ConfigDBEntry) ->
	    lists:foreach(fun(Entry) ->
				  ets:insert(ConfigDB,Entry)
			  end,ConfigDBEntry),
	    store(ConfigDB,ConfigList,Modules,Rest);
	{error, Reason} ->
	    {error,Reason}
    end.

store_traverse(_ConfigListEntry, _ConfigList,[]) ->
    {error,?NICE("Unable to store configuration...")};
store_traverse(ConfigListEntry, ConfigList, [Module|Rest]) ->
    case catch apply(Module,store,[ConfigListEntry, ConfigList]) of
	{'EXIT',{function_clause,_}} ->
	    store_traverse(ConfigListEntry,ConfigList,Rest);
	{'EXIT',{undef, _}} ->
	    store_traverse(ConfigListEntry,ConfigList,Rest);
	{'EXIT', Reason} ->
	    error_logger:error_report({'EXIT',Reason}),
	    store_traverse(ConfigListEntry,ConfigList,Rest);
	Result ->
	    Result
    end.

store_mime_types(Name,MimeTypesList) ->
    %% Make sure that the ets table is not duplicated
    %% when reloading configuration
    catch ets:delete(Name),
    MimeTypesDB = ets:new(Name, [named_table, set, protected]),
    store_mime_types1(MimeTypesDB, MimeTypesList).
store_mime_types1(MimeTypesDB,[]) ->
    {ok, MimeTypesDB};
store_mime_types1(MimeTypesDB,[Type|Rest]) ->
    ets:insert(MimeTypesDB, Type),
    store_mime_types1(MimeTypesDB, Rest).


%% Phase 3: remove
remove_traverse(_ConfigDB,[]) ->
    ok;
remove_traverse(ConfigDB,[Module|Rest]) ->
    case (catch apply(Module,remove,[ConfigDB])) of
	{'EXIT',{undef,_}} ->
	    remove_traverse(ConfigDB,Rest);
	{'EXIT',{function_clause,_}} ->
	    remove_traverse(ConfigDB,Rest);
	{'EXIT',Reason} ->
	    error_logger:error_report({'EXIT',Reason}),
	    remove_traverse(ConfigDB,Rest);
	{error,Reason} ->
	    error_logger:error_report(Reason),
	    remove_traverse(ConfigDB,Rest);
	_ ->
	    remove_traverse(ConfigDB,Rest)
    end.

ssl_certificate_file(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_certificate_file) of
	undefined ->
	    undefined;
	SSLCertificateFile ->
	    [{certfile,SSLCertificateFile}]
    end.

ssl_certificate_key_file(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_certificate_key_file) of
	undefined ->
	    [];
	SSLCertificateKeyFile ->
	    [{keyfile,SSLCertificateKeyFile}]
    end.

ssl_verify_client(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_verify_client) of
	undefined ->
	    [];
	SSLVerifyClient ->
	    [{verify,SSLVerifyClient}]
    end.

ssl_ciphers(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_ciphers) of
	undefined ->
	    [];
	Ciphers ->
	    [{ciphers, Ciphers}]
    end.

ssl_password(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_password_callback_module) of
	undefined ->
	    [];
	Module ->
	    case httpd_util:lookup(ConfigDB, 
				   ssl_password_callback_function) of
		undefined ->
		    [];
		Function ->
		    case catch apply(Module, Function, []) of
			Password when list(Password) ->
			    [{password, Password}];
			Error ->
			    error_report(ssl_password,Module,Function,Error),
			    []
		    end
	    end
    end.

ssl_verify_depth(ConfigDB) ->
    case httpd_util:lookup(ConfigDB, ssl_verify_client_depth) of
	undefined ->
	    [];
	Depth ->
	    [{depth, Depth}]
    end.

ssl_ca_certificate_file(ConfigDB) ->
    case httpd_util:lookup(ConfigDB, ssl_ca_certificate_file) of
	undefined ->
	    [];
	File ->
	    [{cacertfile, File}]
    end.

error_report(Where,M,F,Error) ->
    error_logger:error_report([{?MODULE, Where}, 
			       {apply, {M, F, []}}, Error]).


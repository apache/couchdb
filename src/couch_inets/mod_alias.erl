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
-module(mod_alias).

-export([do/1, 
	 real_name/3,
	 real_script_name/3,
	 default_index/2,
	 load/2,
	 path/3]).

-include("httpd.hrl").

-define(VMODULE,"ALIAS").

%% do

do(Info) ->
    case httpd_util:key1search(Info#mod.data, status) of
	%% A status code has been generated!
	{_StatusCode, _PhraseArgs, _Reason} ->
	    {proceed,Info#mod.data};
	%% No status code has been generated!
	undefined ->
	    case httpd_util:key1search(Info#mod.data, response) of
		%% No response has been generated!
		undefined ->
		    do_alias(Info);
		%% A response has been generated or sent!
		_Response ->
		    {proceed, Info#mod.data}
	    end
    end.

do_alias(Info) ->
    {ShortPath, Path, AfterPath} =
	real_name(Info#mod.config_db, 
		  Info#mod.request_uri,
		  httpd_util:multi_lookup(Info#mod.config_db,alias)),
    %% Relocate if a trailing slash is missing else proceed!
    LastChar = lists:last(ShortPath),
    case file:read_file_info(ShortPath) of 
	{ok, FileInfo} when FileInfo#file_info.type == directory, 
	LastChar /= $/ ->
	    ServerName = httpd_util:lookup(Info#mod.config_db, server_name),
	    Port = port_string(httpd_util:lookup(Info#mod.config_db,port, 80)),
	    URL = "http://" ++ ServerName ++ Port ++ 
		Info#mod.request_uri ++ "/",
	    ReasonPhrase = httpd_util:reason_phrase(301),
	    Message = httpd_util:message(301, URL, Info#mod.config_db),
	    {proceed,
	     [{response,
	       {301, ["Location: ", URL, "\r\n"
		      "Content-Type: text/html\r\n",
		      "\r\n",
		      "<HTML>\n<HEAD>\n<TITLE>",ReasonPhrase,
		      "</TITLE>\n</HEAD>\n"
		      "<BODY>\n<H1>",ReasonPhrase,
		      "</H1>\n", Message, 
		      "\n</BODY>\n</HTML>\n"]}}|
	      [{real_name, {Path, AfterPath}} | Info#mod.data]]};
	_NoFile ->
	    {proceed,[{real_name, {Path, AfterPath}} | Info#mod.data]}
    end.

port_string(80) ->
    "";
port_string(Port) ->
    ":"++integer_to_list(Port).

%% real_name

real_name(ConfigDB, RequestURI, []) ->
    DocumentRoot = httpd_util:lookup(ConfigDB, document_root, ""),
    RealName = DocumentRoot ++ RequestURI,
    {ShortPath, _AfterPath} = httpd_util:split_path(RealName),
    {Path, AfterPath} = httpd_util:split_path(default_index(ConfigDB, 
							    RealName)),
    {ShortPath, Path, AfterPath};
real_name(ConfigDB, RequestURI, [{FakeName,RealName}|Rest]) ->
     case regexp:match(RequestURI, "^" ++ FakeName) of
	{match, _, _} ->
	    {ok, ActualName, _} = regexp:sub(RequestURI,
					     "^" ++ FakeName, RealName),
 	    {ShortPath, _AfterPath} = httpd_util:split_path(ActualName),
	    {Path, AfterPath} =
	       httpd_util:split_path(default_index(ConfigDB, ActualName)),
	    {ShortPath, Path, AfterPath};
	 nomatch ->
	     real_name(ConfigDB,RequestURI,Rest)
    end.

%% real_script_name

real_script_name(_ConfigDB, _RequestURI, []) ->
    not_a_script;
real_script_name(ConfigDB, RequestURI, [{FakeName,RealName} | Rest]) ->
    case regexp:match(RequestURI,"^"++FakeName) of
	{match,_,_} ->
	    {ok,ActualName,_}=regexp:sub(RequestURI,"^"++FakeName,RealName),
	    httpd_util:split_script_path(default_index(ConfigDB,ActualName));
	nomatch ->
	    real_script_name(ConfigDB,RequestURI,Rest)
    end.

%% default_index

default_index(ConfigDB, Path) ->
    case file:read_file_info(Path) of
	{ok, FileInfo} when FileInfo#file_info.type == directory ->
	    DirectoryIndex = httpd_util:lookup(ConfigDB, directory_index, []),
	    append_index(Path, DirectoryIndex);
	_ ->
	    Path
    end.

append_index(RealName, []) ->
    RealName;
append_index(RealName, [Index | Rest]) ->
    case file:read_file_info(filename:join(RealName, Index)) of
	{error, _Reason} ->
	    append_index(RealName, Rest);
	_ ->
	    filename:join(RealName, Index)
    end.

%% path

path(Data, ConfigDB, RequestURI) ->
    case httpd_util:key1search(Data, real_name) of
	undefined ->
	    DocumentRoot = httpd_util:lookup(ConfigDB, document_root, ""),
	    {Path, _AfterPath} = 
		httpd_util:split_path(DocumentRoot++RequestURI),
	    Path;
	{Path, _AfterPath} ->
	    Path
    end.

%%
%% Configuration
%%

%% load

load("DirectoryIndex " ++ DirectoryIndex, []) ->
    {ok, DirectoryIndexes} = regexp:split(DirectoryIndex," "),
    {ok,[], {directory_index, DirectoryIndexes}};
load("Alias " ++ Alias,[]) ->
    case regexp:split(Alias," ") of
	{ok, [FakeName, RealName]} ->
	    {ok,[],{alias,{FakeName,RealName}}};
	{ok, _} ->
	    {error,?NICE(httpd_conf:clean(Alias)++" is an invalid Alias")}
    end;
load("ScriptAlias " ++ ScriptAlias, []) ->
    case regexp:split(ScriptAlias, " ") of
	{ok, [FakeName, RealName]} ->
	    %% Make sure the path always has a trailing slash..
	    RealName1 = filename:join(filename:split(RealName)),
	    {ok, [], {script_alias, {FakeName, RealName1++"/"}}};
	{ok, _} ->
	    {error, ?NICE(httpd_conf:clean(ScriptAlias)++
			  " is an invalid ScriptAlias")}
    end.

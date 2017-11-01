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
-module(couch_plugins).
-include("couch_db.hrl").
-export([install/1, uninstall/1]).

% couch_plugins:install({"geocouch", "http://127.0.0.1:8000", "1.0.0", [{"R15B03", "+XOJP6GSzmuO2qKdnjO+mWckXVs="}]}).
% couch_plugins:install({"geocouch", "http://people.apache.org/~jan/", "couchdb1.2.x_v0.3.0-11-gd83ba22", [{"R15B03", "ZetgdHj2bY2w37buulWVf3USOZs="}]}).

plugin_dir() ->
  couch_config:get("couchdb", "plugin_dir").

log(T) ->
  ?LOG_DEBUG("[couch_plugins] ~p ~n", [T]).

%% "geocouch", "http://localhost:8000/dist", "1.0.0"
-type plugin() :: {string(), string(), string(), list()}.
-spec install(plugin()) -> ok | {error, string()}.
install({Name, _BaseUrl, Version, Checksums}=Plugin) ->
  log("Installing " ++ Name),

  {ok, LocalFilename} = download(Plugin),
  log("downloaded to " ++ LocalFilename),

  ok = verify_checksum(LocalFilename, Checksums),
  log("checksum verified"),

  ok = untargz(LocalFilename),
  log("extraction done"),

  ok = add_code_path(Name, Version),
  log("added code path"),

  ok = register_plugin(Name, Version),
  log("registered plugin"),

  load_config(Name, Version),
  log("loaded config"),

  ok.

% Idempotent uninstall, if you uninstall a non-existant
% plugin, you get an `ok`.
-spec uninstall(plugin()) -> ok | {error, string()}.
uninstall({Name, _BaseUrl, Version, _Checksums}) ->
  % unload config
  ok = unload_config(Name, Version),
  log("config unloaded"),

  % delete files
  ok = delete_files(Name, Version),
  log("files deleted"),

  % delete code path
  ok = del_code_path(Name, Version),
  log("deleted code path"),

  % unregister plugin
  ok = unregister_plugin(Name),
  log("unregistered plugin"),

  % done
  ok.

%% * * *


%% Plugin Registration
%% On uninstall:
%%  - add plugins/name = version to config
%% On uninstall:
%%  - remove plugins/name from config

-spec register_plugin(string(), string()) -> ok.
register_plugin(Name, Version) ->
  couch_config:set("plugins", Name, Version).

-spec unregister_plugin(string()) -> ok.
unregister_plugin(Name) ->
  couch_config:delete("plugins", Name).

%% * * *


%% Load Config
%% Parses <plugindir>/priv/default.d/<pluginname.ini> and applies
%% the contents to the config system, or removes them on uninstall

-spec load_config(string(), string()) -> ok.
load_config(Name, Version) ->
    loop_config(Name, Version, fun set_config/1).

-spec unload_config(string(), string()) -> ok.
unload_config(Name, Version) ->
    loop_config(Name, Version, fun delete_config/1).

-spec loop_config(string(), string(), function()) -> ok.
loop_config(Name, Version, Fun) ->
    lists:foreach(fun(File) -> load_config_file(File, Fun) end,
      filelib:wildcard(file_names(Name, Version))).

-spec load_config_file(string(), function()) -> ok.
load_config_file(File, Fun) ->
    {ok, Config} = couch_config:parse_ini_file(File),
    lists:foreach(Fun, Config).

-spec set_config({{string(), string()}, string()}) -> ok.
set_config({{Section, Key}, Value}) ->
    ok = couch_config:set(Section, Key, Value).

-spec delete_config({{string(), string()}, _Value}) -> ok.
delete_config({{Section, Key}, _Value}) ->
    ok = couch_config:delete(Section, Key).

-spec file_names(string(), string()) -> string().
file_names(Name, Version) ->
  filename:join(
    [plugin_dir(), get_file_slug(Name, Version),
     "priv", "default.d", "*.ini"]).

%% * * *


%% Code Path Management
%% The Erlang code path is where the Erlang runtime looks for `.beam`
%% files to load on, say, `application:load()`. Since plugin directories
%% are created on demand and named after CouchDB and Erlang versions,
%% we manage the Erlang code path semi-automatically here.

-spec add_code_path(string(), string()) -> ok | {error, bad_directory}.
add_code_path(Name, Version) ->
  PluginPath = plugin_dir() ++ "/" ++ get_file_slug(Name, Version) ++ "/ebin",
  case code:add_path(PluginPath) of
    true -> ok;
    Else ->
      ?LOG_ERROR("Failed to add PluginPath: '~s'", [PluginPath]),
      Else
  end.

-spec del_code_path(string(), string()) -> ok | {error, atom()}.
del_code_path(Name, Version) ->
  PluginPath = plugin_dir() ++ "/" ++ get_file_slug(Name, Version) ++ "/ebin",
  case code:del_path(PluginPath) of
    true -> ok;
    _Else ->
      ?LOG_DEBUG("Failed to delete PluginPath: '~s', ignoring", [PluginPath]),
      ok
  end.

%% * * *


-spec untargz(string()) -> {ok, string()} | {error, string()}.
untargz(Filename) ->
  % read .gz file
  {ok, GzData} = file:read_file(Filename),
  % gunzip
  log("unzipped"),
  TarData = zlib:gunzip(GzData),
  ok = filelib:ensure_dir(plugin_dir()),
  % untar
  erl_tar:extract({binary, TarData}, [{cwd, plugin_dir()}, keep_old_files]).

-spec delete_files(string(), string()) -> ok | {error, atom()}.
delete_files(Name, Version) ->
  PluginPath = plugin_dir() ++ "/" ++ get_file_slug(Name, Version),
  mochitemp:rmtempdir(PluginPath).


% downloads a pluygin .tar.gz into a local plugins directory
-spec download(string()) -> ok | {error, string()}.
download({Name, _BaseUrl, Version, _Checksums}=Plugin) ->
  TargetFile = "/tmp/" ++ get_filename(Name, Version),
  case file_exists(TargetFile) of
    %% wipe and redownload
    true -> file:delete(TargetFile);
    _Else -> ok
  end,
  Url = get_url(Plugin),
  HTTPOptions = [
    {connect_timeout, 30*1000}, % 30 seconds
    {timeout, 30*1000} % 30 seconds
  ],
  % todo: windows
  Options = [
    {stream, TargetFile}, % /tmp/something
    {body_format, binary},
    {full_result, false}
  ],
  % todo: reduce to just httpc:request()
  case httpc:request(get, {Url, []}, HTTPOptions, Options) of
    {ok, _Result} ->
      log("downloading " ++ Url),
      {ok, TargetFile};
    Error -> Error
  end.

-spec verify_checksum(string(), list()) -> ok | {error, string()}.
verify_checksum(Filename, Checksums) ->

  CouchDBVersion = couchdb_version(),
  case proplists:get_value(CouchDBVersion, Checksums) of
  undefined ->
    ?LOG_ERROR("[couch_plugins] Can't find checksum for CouchDB Version '~s'", [CouchDBVersion]),
    {error, no_couchdb_checksum};
  OTPChecksum ->
    OTPRelease = erlang:system_info(otp_release),
    case proplists:get_value(OTPRelease, OTPChecksum) of
    undefined ->
      ?LOG_ERROR("[couch_plugins] Can't find checksum for Erlang Version '~s'", [OTPRelease]),
      {error, no_erlang_checksum};
    Checksum ->
      do_verify_checksum(Filename, Checksum)
    end
  end.

-spec do_verify_checksum(string(), string()) -> ok | {error, string()}.
do_verify_checksum(Filename, Checksum) ->
  ?LOG_DEBUG("Checking Filename: ~s", [Filename]),
  case file:read_file(Filename) of
  {ok, Data} ->
    ComputedChecksum = binary_to_list(base64:encode(crypto:hash(sha, Data))),
    case ComputedChecksum of
    Checksum -> ok;
    _Else ->
      ?LOG_ERROR("Checksum mismatch. Wanted: '~p'. Got '~p'", [Checksum, ComputedChecksum]),
      {error, checksum_mismatch}
    end;
  Error -> Error
  end.


%% utils

-spec get_url(plugin()) -> string().
get_url({Name, BaseUrl, Version, _Checksums}) ->
  BaseUrl ++ "/" ++ get_filename(Name, Version).

-spec get_filename(string(), string()) -> string().
get_filename(Name, Version) ->
  get_file_slug(Name, Version) ++ ".tar.gz".

-spec get_file_slug(string(), string()) -> string().
get_file_slug(Name, Version) ->
  % OtpRelease does not include patch levels like the -1 in R15B03-1
  OTPRelease = erlang:system_info(otp_release),
  CouchDBVersion = couchdb_version(),
  string:join([Name, Version, OTPRelease, CouchDBVersion], "-").

-spec file_exists(string()) -> boolean().
file_exists(Filename) ->
  does_file_exist(file:read_file_info(Filename)).
-spec does_file_exist(term()) -> boolean().
does_file_exist({error, enoent}) -> false;
does_file_exist(_Else) -> true.

couchdb_version() ->
  couch_server:get_version(short).

% installing a plugin:
%  - POST /_plugins -d {plugin-def}
%  - get plugin definition
%  - get download URL (matching erlang version)
%  - download archive
%  - match checksum
%  - untar-gz archive into a plugins dir
%  - code:add_path(“geocouch-{geocouch_version}-{erlang_version}/ebin”)
%  - [cp geocouch-{geocouch_version}-{erlang_version}/etc/ ]
%  - application:start(geocouch)
%  - register plugin in plugin registry

% Plugin registry impl:
%  - _plugins database
%   - pro: known db ops
%   - con: no need for replication, needs to be system db etc.
%  - _config/plugins namespace in config
%   - pro: lightweight, fits rarely-changing nature better
%   - con: potentially not flexible enough



% /geocouch
% /geocouch/dist/
% /geocouch/dist/geocouch-{geocouch_version}-{erlang_version}.tar.gz

% tar.gz includes:
% geocouch-{geocouch_version}-{erlang_version}/
% geocouch-{geocouch_version}-{erlang_version}/ebin
% [geocouch-{geocouch_version}-{erlang_version}/config/config.erlt]
% [geocouch-{geocouch_version}-{erlang_version}/share/]


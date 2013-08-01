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
-export([install/1]).

% couch_plugins:install({"geocouch", "http://127.0.0.1:8000", "1.0.0", [{"R15B03", "+XOJP6GSzmuO2qKdnjO+mWckXVs="}]}).
% couch_plugins:install({"geocouch", "http://people.apache.org/~jan/", "couchdb1.2.x_v0.3.0-11-gd83ba22", [{"R15B03", "ZetgdHj2bY2w37buulWVf3USOZs="}]}).

-define(PLUGIN_DIR, "/tmp/couchdb_plugins").

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

  ok = load_plugin(Name),
  log("loaded plugin"),

  load_config(Name, Version),
  log("loaded config"),

  ok.

-spec register_plugin(string(), string()) -> ok.
register_plugin(Name, Version) ->
  couch_config:set("plugins", Name, Version).

-spec load_config(string(), string()) -> ok.
load_config(Name, Version) ->
    lists:foreach(
      fun load_config_file/1,
      filelib:wildcard(
        filename:join(
          [?PLUGIN_DIR, get_file_slug(Name, Version),
           "priv", "default.d", "*.ini"]))).

-spec load_config_file(string()) -> ok.
load_config_file(File) ->
    {ok, Config} = couch_config:parse_ini_file(File),
    lists:foreach(fun set_config/1, Config).

-spec set_config({{string(), string()}, string()}) -> ok.
set_config({{Section, Key}, Value}) ->
    ok = couch_config:set(Section, Key, Value, false).

-spec add_code_path(string(), string()) -> ok | {error, bad_directory}.
add_code_path(Name, Version) ->
  PluginPath = ?PLUGIN_DIR ++ "/" ++ get_file_slug(Name, Version) ++ "/ebin",
  case code:add_path(PluginPath) of
    true -> ok;
    Else ->
      ?LOG_ERROR("Failed to add PluginPath: '~s'", [PluginPath]),
      Else
  end.

-spec load_plugin(string()) -> ok | {error, atom()}.
load_plugin(NameList) ->
  Name = list_to_atom(NameList),
  application:load(Name).


-spec untargz(string()) -> {ok, string()} | {error, string()}.
untargz(Filename) ->
  % read .gz file
  {ok, GzData} = file:read_file(Filename),
  % gunzip
  log("unzipped"),
  TarData = zlib:gunzip(GzData),
  ok = filelib:ensure_dir(?PLUGIN_DIR),
  % untar
  erl_tar:extract({binary, TarData}, [{cwd, ?PLUGIN_DIR}, keep_old_files]).


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

  OTPRelease = erlang:system_info(otp_release),
  case proplists:get_value(OTPRelease, Checksums) of
  undefined ->
    ?LOG_ERROR("[couch_plugins] Can't find checksum for OTP Release '~s'", [OTPRelease]),
    {error, no_checksum};
  Checksum ->
    do_verify_checksum(Filename, Checksum)
  end.

-spec do_verify_checksum(string(), string()) -> ok | {error, string()}.
do_verify_checksum(Filename, Checksum) ->
  ?LOG_DEBUG("Filename: ~s", [Filename]),
  case file:read_file(Filename) of
  {ok, Data} ->
    ComputedChecksum = binary_to_list(base64:encode(crypto:sha(Data))),
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
  Name ++ "-" ++ Version ++ "-" ++ OTPRelease.

-spec file_exists(string()) -> boolean().
file_exists(Filename) ->
  does_file_exist(file:read_file_info(Filename)).
-spec does_file_exist(term()) -> boolean().
does_file_exist({error, enoent}) -> false;
does_file_exist(_Else) -> true.

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



% config.erlt:
% // {{Section, Key}, Value}
% {{"httpd_db_handlers", "_spatial_cleanup"}, "{couch_spatial_http, handle_cleanup_req}"}
% {{"httpd_design_handlers", "_spatial"}, "{couch_spatial_http, handle_spatial_req}"}
% {{"httpd_design_handlers", "_list"}, "{couch_spatial_list, handle_view_list_req}"}
% {{"httpd_design_handlers", "_info"}, "{couch_spatial_http, handle_info_req}"}
% {{"httpd_design_handlers", "_compact"}, "{couch_spatial_http, handle_compact_req}"}

% milestones
% 1. MVP
%  - erlang plugins only
%  - no c deps
%  - install via futon (admin only)
%  - uninstall via futon (admin only)
%  - load plugin.tgz from the web
%  - no security checking
%  - no identity checking
%  - hardcoded list of plugins in futon
%  - must publish on *.apache.org/*

% 2. Creator friendly
%  - couchdb plugin template
%  - easy to publish

% 3. Public registry
%  - plugin authors can publish stuff independently, shows up in futon
%

% XXX Later
%  - signing of plugin releases
%  - signing verification of plugin releases


% Questions:
% - where should the downloaded .beam files put?
%  - in couch 1.x.x context
%  - in bigcouch context
%  - what is a server-user owned data/ dir we can use for this, that isn’t db_dir or index_dir or log or var/run or /tmp

% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_auth_lockout).
-include_lib("couch/include/couch_db.hrl").

-define(LRU, couch_lockout_lru).

-export([is_locked_out/3, lockout/3]).

is_locked_out(#httpd{} = Req, UserName, UserSalt) ->
    case lockout_mode() of
        off ->
            false;
        Mode ->
            is_locked_out_int(Req, Mode, UserName, UserSalt)
    end.

is_locked_out_int(#httpd{} = Req, Mode, UserName, UserSalt) ->
    LockoutThreshold = lockout_threshold(),
    case peer(Req) of
        nopeer ->
            false;
        Peer ->
            case ets_lru:lookup_d(?LRU, {Peer, UserName, UserSalt}) of
                {ok, FailCount} when FailCount >= LockoutThreshold, Mode == enforce ->
                    true;
                {ok, FailCount} when FailCount >= LockoutThreshold, Mode == warn ->
                    couch_log:warning(
                        "~p: authentication failure threshold reached for ~s from ~s",
                        [?MODULE, UserName, Peer]
                    ),
                    false;
                {ok, _} ->
                    false;
                not_found ->
                    false
            end
    end.

lockout(#httpd{} = Req, UserName, UserSalt) ->
    case peer(Req) of
        nopeer ->
            ok;
        Peer ->
            case lockout_mode() of
                off ->
                    ok;
                _ ->
                    ets_lru:update_counter(?LRU, {Peer, UserName, UserSalt}, 1)
            end
    end.

lockout_mode() ->
    case config:get("couch_auth_lockout", "mode", "off") of
        "off" ->
            off;
        "warn" ->
            warn;
        "enforce" ->
            enforce;
        _ ->
            off
    end.

lockout_threshold() ->
    config:get_integer("couch_auth_lockout", "threshold", 5).

peer(#httpd{mochi_req = MochiReq}) ->
    Socket = mochiweb_request:get(socket, MochiReq),
    case Socket of
        {remote, _Pid, _Ref} ->
            nopeer;
        _ ->
            mochiweb_request:get(peer, MochiReq)
    end.

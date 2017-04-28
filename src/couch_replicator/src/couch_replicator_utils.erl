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

-module(couch_replicator_utils).

-export([
   parse_rep_doc/2,
   open_db/1,
   close_db/1,
   start_db_compaction_notifier/2,
   stop_db_compaction_notifier/1,
   replication_id/2,
   sum_stats/2,
   is_deleted/1,
   rep_error_to_binary/1,
   get_json_value/2,
   get_json_value/3,
   pp_rep_id/1,
   iso8601/1,
   filter_state/3
]).

-export([
    handle_db_event/3
]).

-include_lib("couch/include/couch_db.hrl").
-include("couch_replicator.hrl").

-import(couch_util, [
    get_value/2,
    get_value/3
]).


open_db(#db{name = Name, user_ctx = UserCtx}) ->
    {ok, Db} = couch_db:open(Name, [{user_ctx, UserCtx} | []]),
    Db;
open_db(HttpDb) ->
    HttpDb.


close_db(#db{} = Db) ->
    couch_db:close(Db);
close_db(_HttpDb) ->
    ok.


start_db_compaction_notifier(#db{name = DbName}, Server) ->
    {ok, Pid} = couch_event:link_listener(
            ?MODULE, handle_db_event, Server, [{dbname, DbName}]
        ),
    Pid;
start_db_compaction_notifier(_, _) ->
    nil.


stop_db_compaction_notifier(nil) ->
    ok;
stop_db_compaction_notifier(Listener) ->
    couch_event:stop_listener(Listener).


handle_db_event(DbName, compacted, Server) ->
    gen_server:cast(Server, {db_compacted, DbName}),
    {ok, Server};
handle_db_event(_DbName, _Event, Server) ->
    {ok, Server}.


rep_error_to_binary(Error) ->
    couch_util:to_binary(error_reason(Error)).


error_reason({shutdown, Error}) ->
    error_reason(Error);
error_reason({error, {Error, Reason}})
  when is_atom(Error), is_binary(Reason) ->
    io_lib:format("~s: ~s", [Error, Reason]);
error_reason({error, Reason}) ->
    Reason;
error_reason(Reason) ->
    Reason.


get_json_value(Key, Props) ->
    get_json_value(Key, Props, undefined).

get_json_value(Key, Props, Default) when is_atom(Key) ->
    Ref = make_ref(),
    case get_value(Key, Props, Ref) of
        Ref ->
            get_value(?l2b(atom_to_list(Key)), Props, Default);
        Else ->
            Else
    end;
get_json_value(Key, Props, Default) when is_binary(Key) ->
    Ref = make_ref(),
    case get_value(Key, Props, Ref) of
        Ref ->
            get_value(list_to_atom(?b2l(Key)), Props, Default);
        Else ->
            Else
    end.


% pretty-print replication id
-spec pp_rep_id(#rep{} | rep_id()) -> string().
pp_rep_id(#rep{id = RepId}) ->
    pp_rep_id(RepId);
pp_rep_id({Base, Extension}) ->
    Base ++ Extension.


% NV: TODO: this function is not used outside api wrap module
% consider moving it there during final cleanup
is_deleted(Change) ->
    get_json_value(<<"deleted">>, Change, false).


% NV: TODO: proxy some functions which used to be here, later remove
% these and replace calls to their respective modules
replication_id(Rep, Version) ->
    couch_replicator_ids:replication_id(Rep, Version).


sum_stats(S1, S2) ->
    couch_replicator_stats:sum_stats(S1, S2).


parse_rep_doc(Props, UserCtx) ->
    couch_replicator_docs:parse_rep_doc(Props, UserCtx).


-spec iso8601(erlang:timestamp()) -> binary().
iso8601({_Mega, _Sec, _Micro} = Timestamp) ->
    {{Y, Mon, D}, {H, Min, S}} = calendar:now_to_universal_time(Timestamp),
    Format = "~B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
    iolist_to_binary(io_lib:format(Format, [Y, Mon, D, H, Min, S])).


%% Filter replication info ejson by state provided. If it matches return
%% the input value, if it doesn't return 'skip'. This is used from replicator
%% fabric coordinator and worker.
-spec filter_state(atom(), [atom()], {[_ | _]}) -> {[_ | _]} | skip.
filter_state(null = _State, _States, _Info) ->
    skip;
filter_state(_ = _State, [] = _States, Info) ->
    Info;
filter_state(State, States, Info) ->
    case lists:member(State, States) of
        true ->
            Info;
        false ->
            skip
    end.

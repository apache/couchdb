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

% Scanner plugin to find document contents matching a regular experssion.
%

-module(couch_scanner_plugin_find).

-behaviour(couch_scanner_plugin).

-export([
    start/2,
    resume/2,
    complete/1,
    checkpoint/1,
    db/2,
    shards/2,
    db_opened/2,
    doc_id/3,
    doc/3,
    db_closing/2
]).

-include_lib("couch_scanner/include/couch_scanner_plugin.hrl").

-record(st, {
    sid,
    regexes = #{},
    compiled_regexes = #{}
}).

% Behavior callbacks

start(SId, #{}) ->
    ?INFO("Starting.", [], #{sid => SId}),
    St = #st{sid = SId, regexes = regexes()},
    {ok, compile_regexes(St)}.

resume(SId, #{<<"regexes">> := OldRegexes}) ->
    Regexes = regexes(),
    case OldRegexes == Regexes of
        true ->
            ?INFO("Resuming.", [], #{sid => SId}),
            St = #st{sid = SId, regexes = Regexes},
            {ok, compile_regexes(St)};
        false ->
            ?INFO("Resetting. Config changed.", [], #{sid => SId}),
            reset
    end.

complete(#st{sid = SId}) ->
    ?INFO("Completed", [], #{sid => SId}),
    {ok, #{}}.

checkpoint(#st{sid = SId, regexes = CurRegexes}) ->
    case CurRegexes == regexes() of
        true ->
            {ok, #{<<"regexes">> => CurRegexes}};
        false ->
            ?INFO("Resetting. Config changed.", [], #{sid => SId}),
            reset
    end.

db(#st{} = St, DbName) ->
    #st{sid = SId, compiled_regexes = Pats} = St,
    Meta = #{sid => SId, db => DbName},
    report_match(DbName, Pats, Meta),
    {ok, St}.

shards(#st{sid = SId} = St, Shards) ->
    case debug() of
        true -> ?DEBUG(" ~p shards", [length(Shards)], #{sid => SId});
        false -> ok
    end,
    {Shards, St}.

db_opened(#st{sid = SId} = St, Db) ->
    case debug() of
        true -> ?DEBUG("", [], #{sid => SId, db => Db});
        false -> ok
    end,
    % Search backwards with the idea that we may be looking for some recent
    % changes we just made to the database.
    {couch_db:get_update_seq(Db), [{dir, rev}], St}.

doc_id(#st{} = St, DocId, Db) ->
    #st{sid = SId, compiled_regexes = Pats} = St,
    Meta = #{sid => SId, doc => DocId, db => Db},
    report_match(DocId, Pats, Meta),
    {ok, St}.

doc(#st{} = St, Db, #doc{id = DocId, body = Body}) ->
    #st{sid = SId, compiled_regexes = Pats} = St,
    Meta = #{sid => SId, doc => DocId, db => Db},
    report_match(Body, Pats, Meta),
    {ok, St}.

db_closing(#st{sid = SId} = St, Db) ->
    case debug() of
        true -> ?DEBUG("", [], #{sid => SId, db => Db});
        false -> ok
    end,
    {ok, St}.

% Private

regexes() ->
    Section = atom_to_list(?MODULE) ++ ".regexes",
    couch_scanner_util:load_regexes(config:get(Section)).

compile_regexes(#st{regexes = Regexes} = St) ->
    Compiled = couch_scanner_util:compile_regexes(Regexes),
    St#st{compiled_regexes = Compiled}.

report_match(Obj, Pats, Meta) ->
    case couch_scanner_util:match_regexes(Obj, Pats) of
        {match, PatId} -> ?WARN("found regex ~s", [PatId], Meta);
        nomatch -> ok
    end.

debug() ->
    config:get_boolean(atom_to_list(?MODULE), "debug", false).

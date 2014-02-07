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
%
-module(couch_mrview_changes).

-export([handle_changes/6]).

-include_lib("couch/include/couch_db.hrl").

-record(vst, {dbname,
              ddoc,
              view,
              view_options,
              since,
              callback,
              acc,
              user_timeout,
              timeout,
              heartbeat,
              timeout_acc=0,
              notifier,
              stream}).

-type changes_stream() :: true | false | once.
-type changes_options() :: [{stream, changes_stream()} |
                            {since, integer()} |
                            {view_options, list()} |
                            {timeout, integer()} |
                            {heartbeat, true | integer()}].

-export_type([changes_stream/0]).
-export_type([changes_options/0]).

%% @doc function returning changes in a streaming fashion if needed.
-spec handle_changes(binary(), binary(), binary(), function(), term(),
                     changes_options()) -> ok | {error, term()}.
handle_changes(DbName, DDocId, View, Fun, Acc, Options) ->
    Since = proplists:get_value(since, Options, 0),
    Stream = proplists:get_value(stream, Options, false),
    ViewOptions = proplists:get_value(view_options, Options, []),

    State0 = #vst{dbname=DbName,
                  ddoc=DDocId,
                  view=View,
                  view_options=ViewOptions,
                  since=Since,
                  callback=Fun,
                  acc=Acc},

    case view_changes_since(State0) of
        {ok, #vst{since=LastSeq, acc=Acc2}=State} ->
            case Stream of
                true ->
                    start_loop(State#vst{stream=true}, Options);
                once when LastSeq =:= Since ->
                    start_loop(State#vst{stream=once}, Options);
                _ ->
                    Fun(stop, {LastSeq, Acc2})
            end;
        {stop, #vst{since=LastSeq, acc=Acc2}} ->
            Fun(stop, {LastSeq, Acc2});
        Error ->
            Error
    end.

start_loop(#vst{dbname=DbName, ddoc=DDocId}=State, Options) ->
    {UserTimeout, Timeout, Heartbeat} = changes_timeout(Options),
    Notifier = index_update_notifier(DbName, DDocId),
    try
        loop(State#vst{notifier=Notifier,
                       user_timeout=UserTimeout,
                       timeout=Timeout,
                       heartbeat=Heartbeat})
    after
        couch_index_event:stop(Notifier)
    end.

loop(#vst{since=Since, callback=Callback, acc=Acc,
          user_timeout=UserTimeout, timeout=Timeout,
          heartbeat=Heartbeat, timeout_acc=TimeoutAcc,
          stream=Stream}=State) ->
    receive
        index_update ->
            case view_changes_since(State) of
                {ok, State2} when Stream =:= true ->
                    loop(State2#vst{timeout_acc=0});
                {ok, #vst{since=LastSeq, acc=Acc2}} ->
                    Callback(stop, {LastSeq, Acc2});
                {stop, #vst{since=LastSeq, acc=Acc2}} ->
                    Callback(stop, {LastSeq, Acc2})
            end;
        index_delete ->
            Callback(stop, {Since, Acc})
    after Timeout ->
            TimeoutAcc2 = TimeoutAcc + Timeout,
            case UserTimeout =< TimeoutAcc2 of
                true ->
                    Callback(stop, {Since, Acc});
                false when Heartbeat =:= true ->
                    case Callback(heartbeat, Acc) of
                        {ok, Acc2} ->
                            loop(State#vst{acc=Acc2, timeout_acc=TimeoutAcc2});
                        {stop, Acc2} ->
                            Callback(stop, {Since, Acc2})
                    end;
                _ ->
                    Callback(stop, {Since, Acc})
            end
    end.

changes_timeout(Options) ->
    DefaultTimeout = list_to_integer(
            couch_config:get("httpd", "changes_timeout", "60000")
    ),
    UserTimeout = proplists:get_value(timeout, Options, DefaultTimeout),
    {Timeout, Heartbeat} = case proplists:get_value(heartbeat, Options) of
        undefined -> {UserTimeout, false};
        true ->
            T = erlang:min(DefaultTimeout, UserTimeout),
            {T, true};
        H ->
            T = erlang:min(H, UserTimeout),
            {T, true}
    end,
    {UserTimeout, Timeout, Heartbeat}.

view_changes_since(#vst{dbname=DbName, ddoc=DDocId, view=View,
                        view_options=Options, since=Since,
                        callback=Callback, acc=UserAcc}=State) ->
    Wrapper = fun ({{Seq, _Key, _DocId}, _Val}=KV, {Go, Acc2, OldSeq}) ->
            LastSeq = if OldSeq < Seq -> Seq;
                true -> OldSeq
            end,

            {Go, Acc3} = Callback(KV, Acc2),
            {Go, {Go, Acc3, LastSeq}}
    end,

    Acc0 = {ok, UserAcc, Since},
    case couch_mrview:view_changes_since(DbName, DDocId, View, Since,
                                         Wrapper, Options, Acc0) of
        {ok, {Go, UserAcc2, Since2}}->
            {Go, State#vst{since=Since2, acc=UserAcc2}};
        Error ->
            Error
    end.

index_update_notifier(#db{name=DbName}, DDocId) ->
    index_update_notifier(DbName, DDocId);
index_update_notifier(DbName, DDocId) ->
    Self = self(),
    {ok, NotifierPid} = couch_index_event:start_link(fun
                ({index_update, {Name, Id, couch_mrview_index}})
                        when Name =:= DbName, Id =:= DDocId ->
                    Self ! index_update;
                ({index_delete, {Name, Id, couch_mrview_index}})
                        when Name =:= DbName, Id =:= DDocId ->
                    Self ! index_delete;
                (_) ->
                    ok
            end),
    NotifierPid.

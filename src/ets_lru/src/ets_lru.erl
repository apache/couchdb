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

-module(ets_lru).
-behaviour(gen_server).

-export([
    start_link/2,
    stop/1,

    insert/3,
    lookup/2,
    match/3,
    match_object/3,
    remove/2,
    clear/1,

    % Dirty functions read straight from
    % the ETS tables which means there are
    % race conditions with concurrent access.
    lookup_d/2
]).

-export([
    init/1,
    terminate/2,

    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-define(DEFAULT_TIME_UNIT, millisecond).

-type time_value() :: integer().
-type strict_monotonic_time() :: {time_value(), integer()}.

-record(entry, {
    key :: term(),
    val :: term(),
    atime :: strict_monotonic_time(),
    ctime :: strict_monotonic_time()
}).

-record(st, {
    objects,
    atimes,
    ctimes,

    max_objs :: non_neg_integer() | undefined,
    max_size :: non_neg_integer() | undefined,
    max_lifetime :: non_neg_integer() | undefined,
    max_idle :: non_neg_integer() | undefined,
    time_unit = ?DEFAULT_TIME_UNIT :: atom()
}).

start_link(Name, Options) when is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, {Name, Options}, []).

stop(LRU) ->
    gen_server:cast(LRU, stop).

lookup(LRU, Key) ->
    gen_server:call(LRU, {lookup, Key}).

insert(LRU, Key, Val) ->
    gen_server:call(LRU, {insert, Key, Val}).

remove(LRU, Key) ->
    gen_server:call(LRU, {remove, Key}).

%% @doc match/3 provides an efficient way to retrieve parts of the
%% keys and values without copying or requiring circumvention of the
%% ets_lru API. The KeySpec and ValueSpec parameters are used as part
%% of one larger match spec so keep in mind that all capturing
%% placeholders will be aliased between the key and value parts.
-spec match(atom() | pid(), term(), term()) -> [[any()]].
match(LRU, KeySpec, ValueSpec) ->
    gen_server:call(LRU, {match, KeySpec, ValueSpec}).

%% @doc match_object/3 provides an efficient way to retrieve multiple
%% values using match conditions. The KeySpec and ValueSpec parameters
%% are used as part of one larger match spec so keep in mind that all
%% capturing placeholders will be aliased between the key and value
%% parts.
-spec match_object(atom() | pid(), term(), term()) -> [any()].
match_object(Name, KeySpec, ValueSpec) when is_atom(Name) ->
    Pattern = #entry{key = KeySpec, val = ValueSpec, _ = '_'},
    Entries = ets:match_object(obj_table(Name), Pattern),
    lists:map(
        fun(#entry{key = Key, val = Val}) ->
            gen_server:cast(Name, {accessed, Key}),
            Val
        end,
        Entries
    );
match_object(LRU, KeySpec, ValueSpec) ->
    gen_server:call(LRU, {match_object, KeySpec, ValueSpec}).

clear(LRU) ->
    gen_server:call(LRU, clear).

lookup_d(Name, Key) when is_atom(Name) ->
    case ets:lookup(obj_table(Name), Key) of
        [#entry{val = Val}] ->
            gen_server:cast(Name, {accessed, Key}),
            {ok, Val};
        [] ->
            not_found
    end.

init({Name, Options}) ->
    St = set_options(#st{}, Options),
    ObjOpts = [set, named_table, protected, {keypos, #entry.key}],
    TimeOpts = [ordered_set, named_table, protected],

    {ok, St#st{
        objects = ets:new(obj_table(Name), ObjOpts),
        atimes = ets:new(at_table(Name), TimeOpts),
        ctimes = ets:new(ct_table(Name), TimeOpts)
    }}.

terminate(_Reason, St) ->
    true = ets:delete(St#st.objects),
    true = ets:delete(St#st.atimes),
    true = ets:delete(St#st.ctimes),
    ok.

handle_call({lookup, Key}, _From, St) ->
    Reply =
        case ets:lookup(St#st.objects, Key) of
            [#entry{val = Val} | _] ->
                accessed(Key, St),
                {ok, Val};
            [] ->
                not_found
        end,
    {reply, Reply, St, 0};
handle_call({match_object, KeySpec, ValueSpec}, _From, St) ->
    Pattern = #entry{key = KeySpec, val = ValueSpec, _ = '_'},
    Entries = ets:match_object(St#st.objects, Pattern),
    Values = lists:map(
        fun(#entry{key = Key, val = Val}) ->
            accessed(Key, St),
            Val
        end,
        Entries
    ),
    {reply, Values, St, 0};
handle_call({match, KeySpec, ValueSpec}, _From, St) ->
    Pattern = #entry{key = KeySpec, val = ValueSpec, _ = '_'},
    Values = ets:match(St#st.objects, Pattern),
    {reply, Values, St, 0};
handle_call({insert, Key, Val}, _From, St) ->
    NewATime = strict_monotonic_time(St#st.time_unit),
    Pattern = #entry{key = Key, atime = '$1', _ = '_'},
    case ets:match(St#st.objects, Pattern) of
        [[ATime]] ->
            true = ets:update_element(St#st.objects, Key, [
                {#entry.val, Val}, {#entry.atime, NewATime}
            ]),
            true = ets:delete(St#st.atimes, ATime),
            true = ets:insert(St#st.atimes, {NewATime, Key});
        [] ->
            Entry = #entry{key = Key, val = Val, atime = NewATime, ctime = NewATime},
            true = ets:insert(St#st.objects, Entry),
            true = ets:insert(St#st.atimes, {NewATime, Key}),
            true = ets:insert(St#st.ctimes, {NewATime, Key})
    end,
    {reply, ok, St, 0};
handle_call({remove, Key}, _From, St) ->
    Pattern = #entry{key = Key, atime = '$1', ctime = '$2', _ = '_'},
    Reply =
        case ets:match(St#st.objects, Pattern) of
            [[ATime, CTime]] ->
                true = ets:delete(St#st.objects, Key),
                true = ets:delete(St#st.atimes, ATime),
                true = ets:delete(St#st.ctimes, CTime),
                ok;
            [] ->
                not_found
        end,
    {reply, Reply, St, 0};
handle_call(clear, _From, St) ->
    true = ets:delete_all_objects(St#st.objects),
    true = ets:delete_all_objects(St#st.atimes),
    true = ets:delete_all_objects(St#st.ctimes),
    % No need to timeout here and evict cache
    % entries because its now empty.
    {reply, ok, St};
handle_call(Msg, _From, St) ->
    {stop, {invalid_call, Msg}, {invalid_call, Msg}, St}.

handle_cast({accessed, Key}, St) ->
    accessed(Key, St),
    {noreply, St, 0};
handle_cast(stop, St) ->
    {stop, normal, St};
handle_cast(Msg, St) ->
    {stop, {invalid_cast, Msg}, St}.

handle_info(timeout, St) ->
    trim(St),
    {noreply, St, next_timeout(St)};
handle_info(Msg, St) ->
    {stop, {invalid_info, Msg}, St}.

accessed(Key, St) ->
    Pattern = #entry{key = Key, atime = '$1', _ = '_'},
    case ets:match(St#st.objects, Pattern) of
        [[ATime]] ->
            NewATime = strict_monotonic_time(St#st.time_unit),
            Update = {#entry.atime, NewATime},
            true = ets:update_element(St#st.objects, Key, Update),
            true = ets:delete(St#st.atimes, ATime),
            true = ets:insert(St#st.atimes, {NewATime, Key}),
            ok;
        [] ->
            ok
    end.

trim(St) ->
    trim_count(St),
    trim_size(St),
    trim_lifetime(St),
    trim_idle(St).

trim_count(#st{max_objs = undefined}) ->
    ok;
trim_count(#st{max_objs = Max} = St) ->
    case ets:info(St#st.objects, size) > Max of
        true ->
            drop_lru(St, fun trim_count/1);
        false ->
            ok
    end.

trim_size(#st{max_size = undefined}) ->
    ok;
trim_size(#st{max_size = Max} = St) ->
    case ets:info(St#st.objects, memory) > Max of
        true ->
            drop_lru(St, fun trim_size/1);
        false ->
            ok
    end.

trim_lifetime(#st{max_lifetime = undefined}) ->
    ok;
trim_lifetime(#st{max_lifetime = Max} = St) ->
    Now = erlang:monotonic_time(St#st.time_unit),
    case ets:first(St#st.ctimes) of
        '$end_of_table' ->
            ok;
        CTime = {Time, _} ->
            case Now - Time > Max of
                true ->
                    [{CTime, Key}] = ets:lookup(St#st.ctimes, CTime),
                    Pattern = #entry{key = Key, atime = '$1', _ = '_'},
                    [[ATime]] = ets:match(St#st.objects, Pattern),
                    true = ets:delete(St#st.objects, Key),
                    true = ets:delete(St#st.atimes, ATime),
                    true = ets:delete(St#st.ctimes, CTime),
                    trim_lifetime(St);
                false ->
                    ok
            end
    end.

trim_idle(#st{max_idle = undefined}) ->
    ok;
trim_idle(#st{max_idle = Max} = St) ->
    Now = erlang:monotonic_time(St#st.time_unit),
    case ets:first(St#st.atimes) of
        '$end_of_table' ->
            ok;
        ATime = {Time, _} ->
            case Now - Time > Max of
                true ->
                    [{ATime, Key}] = ets:lookup(St#st.atimes, ATime),
                    Pattern = #entry{key = Key, ctime = '$1', _ = '_'},
                    [[CTime]] = ets:match(St#st.objects, Pattern),
                    true = ets:delete(St#st.objects, Key),
                    true = ets:delete(St#st.atimes, ATime),
                    true = ets:delete(St#st.ctimes, CTime),
                    trim_idle(St);
                false ->
                    ok
            end
    end.

drop_lru(St, Continue) ->
    case ets:first(St#st.atimes) of
        '$end_of_table' ->
            empty;
        ATime ->
            [{ATime, Key}] = ets:lookup(St#st.atimes, ATime),
            Pattern = #entry{key = Key, ctime = '$1', _ = '_'},
            [[CTime]] = ets:match(St#st.objects, Pattern),
            true = ets:delete(St#st.objects, Key),
            true = ets:delete(St#st.atimes, ATime),
            true = ets:delete(St#st.ctimes, CTime),
            Continue(St)
    end.

next_timeout(#st{max_lifetime = undefined, max_idle = undefined}) ->
    infinity;
next_timeout(St) ->
    Now = erlang:monotonic_time(St#st.time_unit),
    CTimeout = next_timeout(St#st.ctimes, Now, St#st.max_lifetime),
    ATimeout = next_timeout(St#st.atimes, Now, St#st.max_idle),
    min(CTimeout, ATimeout).

next_timeout(_Tab, _Now, undefined) ->
    infinity;
next_timeout(Tab, Now, Max) ->
    case ets:first(Tab) of
        '$end_of_table' ->
            infinity;
        {Time, _} ->
            TimeDiff = Now - Time,
            erlang:max(Max - TimeDiff, 0)
    end.

set_options(St, []) ->
    St;
set_options(St, [{max_objects, N} | Rest]) when is_integer(N), N >= 0 ->
    set_options(St#st{max_objs = N}, Rest);
set_options(St, [{max_size, N} | Rest]) when is_integer(N), N >= 0 ->
    set_options(St#st{max_size = N}, Rest);
set_options(St, [{max_lifetime, N} | Rest]) when is_integer(N), N >= 0 ->
    set_options(St#st{max_lifetime = N}, Rest);
set_options(St, [{max_idle, N} | Rest]) when is_integer(N), N >= 0 ->
    set_options(St#st{max_idle = N}, Rest);
set_options(St, [{time_unit, T} | Rest]) when is_atom(T) ->
    set_options(St#st{time_unit = T}, Rest);
set_options(_, [Opt | _]) ->
    throw({invalid_option, Opt}).

obj_table(Name) ->
    table_name(Name, "_objects").

at_table(Name) ->
    table_name(Name, "_atimes").

ct_table(Name) ->
    table_name(Name, "_ctimes").

table_name(Name, Ext) ->
    list_to_atom(atom_to_list(Name) ++ Ext).

-spec strict_monotonic_time(atom()) -> strict_monotonic_time().
strict_monotonic_time(TimeUnit) ->
    {erlang:monotonic_time(TimeUnit), make_ref()}.

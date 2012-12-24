% Copyright 2012 Cloudant. All rights reserved.

-module(ets_lru).


-export([
    create/2,
    destroy/1,

    insert/3,
    lookup/2,
    member/2,
    remove/2,
    hit/2,
    expire/1,
    clear/1
]).


-record(entry, {
    key,
    val,
    atime
}).

-record(ets_lru, {
    objects,
    atimes,
    named=false,

    max_objs,
    max_size,

    lifetime
}).


create(Name, Options) ->
    LRU = set_options(#ets_lru{}, Options),
    Opts = case LRU#ets_lru.named of
        true -> [named_table];
        false -> []
    end,
    {OName, ATName} = table_names(Name),
    {ok, LRU#ets_lru{
        objects = ets:new(OName,
                    [set, protected, {keypos, #entry.key}] ++ Opts),
        atimes = ets:new(ATName,
                    [ordered_set, protected] ++ Opts)
    }}.


destroy(#ets_lru{objects=Objs, atimes=ATimes}) ->
    true = ets:delete(Objs),
    true = ets:delete(ATimes),
    ok.


insert(#ets_lru{objects=Objs, atimes=ATs}=LRU, Key, Val) ->
    NewATime = erlang:now(),
    Pattern = #entry{key=Key, atime='$1', _='_'},
    case ets:match(Objs, Pattern) of
        [[ATime]] ->
            true = ets:delete(ATs, ATime),
            true = ets:insert(ATs, {NewATime, Key}),
            true = ets:update_element(Objs, Key, {#entry.val, Val});
        [] ->
            true = ets:insert(ATs, {NewATime, Key}),
            true = ets:insert(Objs, #entry{key=Key, val=Val, atime=NewATime})
    end,
    trim(LRU).


lookup(#ets_lru{objects=Objs}=LRU, Key) ->
    case ets:lookup(Objs, Key) of
        [#entry{val=Val}] ->
            hit(LRU, Key),
            {ok, Val};
        [] ->
            not_found
    end.


member(#ets_lru{objects=Objs}, Key) ->
    ets:member(Objs, Key).


remove(#ets_lru{objects=Objs, atimes=ATs}=LRU, Key) ->
    case ets:match(Objs, #entry{key=Key, atime='$1', _='_'}) of
        [[ATime]] ->
            true = ets:delete(ATs, ATime),
            true = ets:delete(Objs, Key),
            ok;
        [] ->
            ok
    end,
    false = member(LRU, Key),
    ok.


hit(#ets_lru{objects=Objs, atimes=ATs}, Key) ->
    case ets:match(Objs, #entry{key=Key, atime='$1', _='_'}) of
        [[ATime]] ->
            NewATime = erlang:now(),
            true = ets:delete(ATs, ATime),
            true = ets:insert(ATs, {NewATime, Key}),
            true = ets:update_element(Objs, Key, {#entry.atime, NewATime}),
            ok;
        [] ->
            ok
    end.


expire(#ets_lru{lifetime=undefined}) ->
    ok;
expire(#ets_lru{objects=Objs, atimes=ATs, lifetime=LT}=LRU) ->
    Now = os:timestamp(),
    LTMicro = LT * 1000,
    case ets:first(ATs) of
        '$end_of_table' ->
            ok;
        ATime ->
            case timer:now_diff(Now, ATime) > LTMicro of
                true ->
                    [{ATime, Key}] = ets:lookup(ATs, ATime),
                    true = ets:delete(ATs, ATime),
                    true = ets:delete(Objs, Key),
                    expire(LRU);
                false ->
                    ok
            end
    end.


clear(#ets_lru{objects=Objs, atimes=ATs}) ->
    true = ets:delete_all_objects(Objs),
    true = ets:delete_all_objects(ATs),
    ok.


trim(#ets_lru{}=LRU) ->
    case trim_count(LRU) of
        trimmed -> trim(LRU);
        _ -> ok
    end,
    case trim_size(LRU) of
        trimmed -> trim(LRU);
        _ -> ok
    end.


trim_count(#ets_lru{max_objs=undefined}) ->
    ok;
trim_count(#ets_lru{objects=Objs, max_objs=MO}=LRU) ->
    case ets:info(Objs, size) > MO of
        true -> drop_entry(LRU);
        false -> ok
    end.


trim_size(#ets_lru{max_size=undefined}) ->
    ok;
trim_size(#ets_lru{objects=Objs, max_size=MS}=LRU) ->
    case ets:info(Objs, memory) > MS of
        true -> drop_entry(LRU);
        false -> ok
    end.


drop_entry(#ets_lru{objects=Objs, atimes=ATs}) ->
    case ets:first(ATs) of
        '$end_of_table' ->
            empty;
        ATime ->
            [{ATime, Key}] = ets:lookup(ATs, ATime),
            true = ets:delete(ATs, ATime),
            true = ets:delete(Objs, Key),
            trimmed
    end.


set_options(LRU, []) ->
    LRU;
set_options(LRU, [named_tables | Rest]) ->
    set_options(LRU#ets_lru{named=true}, Rest);
set_options(LRU, [{max_objects, N} | Rest]) when is_integer(N), N > 0 ->
    set_options(LRU#ets_lru{max_objs=N}, Rest);
set_options(LRU, [{max_size, N} | Rest]) when is_integer(N), N > 0 ->
    set_options(LRU#ets_lru{max_size=N}, Rest);
set_options(LRU, [{lifetime, N} | Rest]) when is_integer(N), N > 0 ->
    set_options(LRU#ets_lru{lifetime=N}, Rest);
set_options(_, [Opt | _]) ->
    throw({invalid_option, Opt}).


table_names(Base) when is_atom(Base) ->
    BList = atom_to_list(Base),
    OName = list_to_atom(BList ++ "_objects"),
    ATName = list_to_atom(BList ++ "_atimes"),
    {OName, ATName}.


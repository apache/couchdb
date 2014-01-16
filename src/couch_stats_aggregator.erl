-module(couch_stats_aggregator).

-behaviour(gen_server).

-export([
    fetch/0
]).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-record(st, {
    descriptions,
    stats,
    collect_timer,
    reload_timer
}).

fetch() ->
    {ok, Stats} = gen_server:call(?MODULE, fetch),
    Stats.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Descs} = reload_metrics(),
    Interval = case application:get_env(couch_stats, collection_interval) of
        {ok, I} -> I * 1000
    end,
    {ok, CT} = timer:send_interval(Interval, self(), collect),
    {ok, RT} = timer:send_interval(600000, self(), reload),
    {ok, #st{descriptions=Descs, stats=[], collect_timer=CT, reload_timer=RT}}.

handle_call(fetch, _from, #st{stats = Stats}=State) ->
    {reply, {ok, Stats}, State};
handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, error, State}.

handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info(collect, #st{descriptions=Descriptions}=State) ->
    Stats = lists:map(
        fun({Name, Props}) ->
            {Name, [{value, couch_stats:sample(Name)}|Props]}
        end,
        Descriptions
    ),
    {noreply, State#st{stats=Stats}};
handle_info(reload, State) ->
    {ok, Descriptions} = reload_metrics(),
    {noreply, State#st{descriptions=Descriptions}};
handle_info(Msg, State) ->
    {stop, {unknown_info, Msg}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

reload_metrics() ->
    Existing = couch_stats:list(),
    Current = load_metrics_for_applications(),
    ToDelete = lists:foldl(
        fun({_, {Name, [{type, Type}, _]}}, Acc) ->
            E = {Name, [{type, Type}]},
            case sets:is_element(E, Acc) of
                true ->
                    sets:del_element(E, Acc);
                false ->
                    couch_stats:new(Type, Name),
                    Acc
            end
        end,
        sets:from_list(Existing),
        Current
    ),
    lists:foreach(
        fun({Name, _}) -> couch_stats:delete(Name) end,
        sets:to_list(ToDelete)
    ),
    {ok, Current}.

load_metrics_for_applications() ->
    Apps = [element(1, A) || A <- application:which_applications()],
    lists:foldl(
        fun(AppName, Acc) ->
            case load_metrics_for_application(AppName) of
                error -> Acc;
                Descriptions -> [{AppName, Descriptions}|Acc]
            end
        end,
        [],
        Apps
    ).

load_metrics_for_application(AppName) ->
    case code:priv_dir(AppName) of
        {error, _Error} ->
            error;
        Dir ->
            case file:consult(Dir ++ "/stat_descriptions.cfg") of
                {ok, Descriptions} ->
                    Descriptions;
                {error, _Error} ->
                    error
            end
    end.

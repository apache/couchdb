
-module(mango_protocol).

-behavior(gen_server).
-behavior(ranch_protocol).

-export([
    start_link/4
]).

-export([
    init/1,
    init/4,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-include("mango.hrl").


-record(st, {
    socket,
    transport,
    context,
    buffer = <<>>
}).


start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).


% This function only exists to satiate the
% gen_server behavior checks.
init(_) ->
    {ok, ignored}.


init(Ref, Socket, Transport, _Opts) ->
    twig:log(error, "Client connected: ~p", [Socket]),
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    St = #st{
        socket = Socket,
        transport = Transport,
        context = mango_ctx:new()
    },
    set_active(St),
    gen_server:enter_loop(?MODULE, [], St).


terminate(_Reason, St) ->
    twig:log(error, "Client disconnected: ~p", [St#st.socket]),
    ok.


handle_call(Msg, _From, St) ->
    {error, {invalid_call, Msg}, {invalid_call, Msg}, St}.


handle_cast(Msg, St) ->
    {error, {invalid_cast, Msg}, St}.


handle_info({tcp, _Socket, Data}, St) ->
    set_active(St),
    NewBuffer = <<(St#st.buffer)/binary, Data/binary>>,
    NewSt = St#st{buffer=NewBuffer},
    try
        FinalSt = maybe_handle_message(NewSt),
        {noreply, FinalSt}
    catch
        throw:{stop, Reason} ->
            {stop, Reason, NewSt}
    end;
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};
handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(Msg, St) ->
    {error, {invalid_info, Msg}, St}.


code_change(_OldVSn, St, _Extra) ->
    {ok, St}.


maybe_handle_message(St) ->
    try
        case mango_msg:new(St#st.buffer) of
            {ok, Msg, Rest} ->
                twig:log(err, "IN: ~p", [Msg]),
                NewSt = dispatch(Msg, St#st{buffer=Rest}),
                % Recurse incase the client sent us multiple
                % messages.
                maybe_handle_message(NewSt);
            undefined ->
                St;
            {error, Error1} ->
                NewCtx1 = mango_ctx:add_error(St#st.context, Error1),
                St#st{context=NewCtx1}
        end
    catch
        throw:Error2 ->
            Stack = erlang:get_stacktrace(),
            NewCtx2 = mango_ctx:add_error(St#st.context, Error2, Stack),
            St#st{context=NewCtx2};
        error:Error2 ->
            Stack = erlang:get_stacktrace(),
            NewCtx2 = mango_ctx:add_error(St#st.context, Error2, Stack),
            St#st{context=NewCtx2}
    end.


dispatch(Msg, St) ->
    case mango_handler:dispatch(Msg, St#st.context) of
        {ok, NewMsg, NewCtx} ->
            maybe_send_resp(NewMsg, St#st{context=NewCtx});
        {error, Reason} ->
            NewCtx = mango_ctx:add_error(St#st.context, Reason),
            maybe_send_resp(Msg, St#st{context=NewCtx})
    end.


set_active(#st{socket=S, transport=T}) ->
    ok = T:setopts(S, [{active, once}]).


maybe_send_resp(Msg, #st{context=Ctx}=St) ->
    twig:log(error, "OUT: ~p", [Msg]),
    case mango_msg:reply(Msg, Ctx) of
        Reply when is_binary(Reply) ->
            send_resp(Reply, St);
        undefined ->
            St
    end.


send_resp(Reply, #st{socket=S, transport=T}=St) ->
    case T:send(S, Reply) of
        ok ->
            St;
        {error, Reason} ->
            throw({stop, Reason})
    end.

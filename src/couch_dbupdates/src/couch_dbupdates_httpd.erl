-module(couch_dbupdates_httpd).

-export([handle_req/1]).

-include_lib("couch_db.hrl").

-record(state, {resp, feed}).

handle_req(#httpd{method='GET'}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    Qs = couch_httpd:qs(Req),
    Feed = proplists:get_value("feed", Qs, "longpoll"),

    Timeout = list_to_integer(
                proplists:get_value("timeout", Qs, "60000")
    ),

    Heartbeat0 = proplists:get_value("heartbeat", Qs),
    Heartbeat = case {Feed, Heartbeat0} of
        {"longpoll", _} -> false;
        {_, "false"} -> false;
        _ -> true
    end,

    Options = [{timeout, Timeout}, {heartbeat, Heartbeat}],

    {ok, Resp} = case Feed of
        "eventsource" ->
            Headers = [
                {"Content-Type", "text/event-stream"},
                {"Cache-Control", "no-cache"}
            ],
            couch_httpd:start_json_response(Req, 200, Headers);
        _ ->
            couch_httpd:start_json_response(Req, 200)
    end,

    State = #state{resp=Resp, feed=Feed},
    couch_dbupdates:handle_dbupdates(fun handle_update/2,
                                     State, Options);

handle_req(Req) ->
    couch_httpd:send_method_not_allowed(Req, "GET").

handle_update(stop, #state{resp=Resp}) ->
    couch_httpd:end_json_response(Resp);
handle_update(heartbeat, #state{resp=Resp}=State) ->
    {ok, Resp1} = couch_httpd:send_chunk(Resp, "\n"),
    {ok, State#state{resp=Resp1}};
handle_update(Event, #state{resp=Resp, feed="eventsource"}=State) ->
    EventObj = event_obj(Event),
    {ok, Resp1} = couch_httpd:send_chunk(Resp, ["data: ",
                                                ?JSON_ENCODE(EventObj),
                                                "\n\n"]),
    {ok, State#state{resp=Resp1}};
handle_update(Event, #state{resp=Resp, feed="continuous"}=State) ->
    EventObj = event_obj(Event),
    {ok, Resp1} = couch_httpd:send_chunk(Resp, [?JSON_ENCODE(EventObj) |
                            "\n"]),
    {ok, State#state{resp=Resp1}};
handle_update(Event, #state{resp=Resp, feed="longpoll"}) ->
    {Props} = event_obj(Event),
    JsonObj = {[{<<"ok">>, true} | Props]},
    couch_httpd:send_chunk(Resp, ?JSON_ENCODE(JsonObj)),
    stop.

event_obj({Type, DbName}) ->
    {[{<<"type">>, couch_util:to_binary(Type)},
      {<<"db_name">>, couch_util:to_binary(DbName)}]}.

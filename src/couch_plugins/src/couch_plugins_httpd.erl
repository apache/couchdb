-module(couch_plugins_httpd).

-export([handle_req/1]).

-include_lib("couch_db.hrl").

handle_req(#httpd{method='POST'}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    couch_httpd:validate_ctype(Req, "application/json"),

    {PluginSpec} = couch_httpd:json_body_obj(Req),
  ?LOG_DEBUG("Plugin Spec: ~p", [PluginSpec]),
    Url = binary_to_list(couch_util:get_value(<<"url">>, PluginSpec)),
    Name = binary_to_list(couch_util:get_value(<<"name">>, PluginSpec)),
    Version = binary_to_list(couch_util:get_value(<<"version">>, PluginSpec)),
    {Checksums0} = couch_util:get_value(<<"checksums">>, PluginSpec),
    Checksums = lists:map(fun({K, V}) ->
      {binary_to_list(K), binary_to_list(V)}
    end, Checksums0),

    case couch_plugins:install({Name, Url, Version, Checksums}}) of
    ok ->
        couch_httpd:send_json(Req, 202, {[{ok, true}]});
    Error ->
        ?LOG_DEBUG("Plugin Spec: ~p", [PluginSpec]),
        couch_httpd:send_error(Req, {bad_request, Error})
    end;
handle_req(Req) ->
    couch_httpd:send_method_not_allowed(Req, "POST").

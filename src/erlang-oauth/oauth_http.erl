-module(oauth_http).

-export([get/1, post/2, response_params/1, response_body/1, response_code/1]).


get(URL) ->
  request(get, {URL, []}).

post(URL, Data) ->
  request(post, {URL, [], "application/x-www-form-urlencoded", Data}).

request(Method, Request) ->
  httpc:request(Method, Request, [{autoredirect, false}], []).

response_params(Response) ->
  oauth_uri:params_from_string(response_body(Response)).

response_body({{_, _, _}, _, Body}) ->
  Body.

response_code({{_, Code, _}, _, _}) ->
  Code.

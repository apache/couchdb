-module(oauth_unix).

-export([timestamp/0]).


timestamp() ->
  timestamp(calendar:universal_time()).

timestamp(DateTime) ->
  seconds(DateTime) - epoch().

epoch() ->
  seconds({{1970,1,1},{00,00,00}}).

seconds(DateTime) ->
  calendar:datetime_to_gregorian_seconds(DateTime).

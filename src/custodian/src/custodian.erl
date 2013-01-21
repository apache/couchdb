% Copyright 2013 Cloudant. All rights reserved.

-module(custodian).

-export([report/0, summary/0]).

report() ->
    custodian_util:report().

summary() ->
     custodian_util:summary().

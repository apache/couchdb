-define(FABRIC, true).

-ifndef(COUCH).
-include("../../couch/src/couch_db.hrl").
-endif.

-ifndef(MEMBERSHIP).
-include("../../membership/include/membership.hrl").
-endif.

-include_lib("eunit/include/eunit.hrl").

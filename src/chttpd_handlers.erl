% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(chttpd_handlers).

-export([
    provider/2,
    url_handler/2,
    db_handler/2,
    design_handler/2
]).

-include_lib("couch/include/couch_db.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

provider(App, Module) ->
    couch_epi_functions:childspec(chttpd_handlers_subscription,
       App, chttpd_handlers, Module).

url_handler(HandlerKey, DefaultFun) ->
    select(collect(url_handler, [HandlerKey]), DefaultFun).

db_handler(HandlerKey, DefaultFun) ->
    select(collect(db_handler, [HandlerKey]), DefaultFun).

design_handler(HandlerKey, DefaultFun) ->
    select(collect(design_handler, [HandlerKey]), DefaultFun).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

collect(Func, Args) ->
    Results = do_apply(Func, Args, []),
    [HandlerFun || HandlerFun <- Results, HandlerFun /= no_match].

do_apply(Func, Args, Opts) ->
    Handle = couch_epi:get_handle(?MODULE),
    couch_epi:apply(Handle, chttpd, Func, Args, Opts).

select([], Default) ->
    Default;
select([{default, OverrideDefault}], _Default) ->
    OverrideDefault;
select(Handlers, _Default) ->
    [Handler] = do_select(Handlers, []),
    Handler.

do_select([], Acc) ->
    Acc;
do_select([{override, Handler}|_], _Acc) ->
    [Handler];
do_select([{default, _}|Rest], Acc) ->
    do_select(Rest, Acc);
do_select([Handler], Acc) ->
    [Handler | Acc];
do_select([Handler | Rest], Acc) ->
    do_select(Rest, [Handler | Acc]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

select_override_test() ->
    ?assertEqual(selected, select([{override, selected}, foo], default)),
    ?assertEqual(selected, select([foo, {override, selected}], default)),
    ?assertEqual(selected, select([{override, selected}, {override, bar}], default)),
    ?assertError({badmatch,[bar, foo]}, select([foo, bar], default)).

select_default_override_test() ->
    ?assertEqual(selected, select([{default, new_default}, selected], old_default)),
    ?assertEqual(selected, select([selected, {default, new_default}], old_default)),
    ?assertEqual(selected, select([{default, selected}], old_default)),
    ?assertEqual(selected, select([], selected)),
    ?assertEqual(selected,
        select([{default, new_default}, {override, selected}, bar], old_default)).

-endif.

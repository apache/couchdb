% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_lru).
-export([new/1, insert/2, update/2, close/1]).

-include_lib("couch/include/couch_db.hrl").

new(CloseFun) ->
    Updates = ets:new(couch_lru_updates, [ordered_set]),
    Counts = ets:new(couch_lru_counts, [set]),
    #couch_lru{updates=Updates, counts=Counts, close_fun=CloseFun}.

insert(Name, Lru) ->
    update(Name, Lru).

update(Name, Lru) ->
    #couch_lru{counts=Counts, updates=Updates, count=Count} = Lru,
    case ets:lookup(Counts, Name) of
        [] ->
            true = ets:insert(Counts, {Name, Count});
        [{Name, OldCount}] ->
            true = ets:update_element(Counts, Name, {2, Count}),
            true = ets:delete(Updates, {OldCount, Name})
    end,
    true = ets:insert(Updates, {{Count, Name}}),
    Lru#couch_lru{count=Count+1}.


close(Lru) ->
    #couch_lru{updates=Updates} = Lru,
    case close_int(ets:next(Updates, {-1, <<>>}), Lru) of
        true ->
            {true, Lru};
        false ->
            false
    end.


%% internals

close_int('$end_of_table', _Lru) ->
    false;
close_int({_Count, Name} = Key, Lru) ->
    #couch_lru{updates=Updates, counts=Counts, close_fun=CloseFun} = Lru,
    {Stop, Remove} = CloseFun(Name),
    case Remove of
        true ->
            true = ets:delete(Updates, Key),
            true = ets:delete(Counts, Name);
        false ->
            ok
    end,
    case Stop of
        false -> close_int(ets:next(Updates, Key), Lru);
        true -> true
    end.

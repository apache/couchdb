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

-module(fabric_dict).
-compile(export_all).

% Instead of ets, let's use an ordered keylist. We'll need to revisit if we
% have >> 100 shards, so a private interface is a good idea. - APK June 2010

init(Keys, InitialValue) ->
    orddict:from_list([{Key, InitialValue} || Key <- Keys]).

is_key(Key, Dict) ->
    orddict:is_key(Key, Dict).

decrement_all(Dict) ->
    [{K,V-1} || {K,V} <- Dict].

store(Key, Value, Dict) ->
    orddict:store(Key, Value, Dict).

erase(Key, Dict) ->
    orddict:erase(Key, Dict).

update_counter(Key, Incr, Dict0) ->
    orddict:update_counter(Key, Incr, Dict0).


lookup_element(Key, Dict) ->
    couch_util:get_value(Key, Dict).

size(Dict) ->
    orddict:size(Dict).

any(Value, Dict) ->
    lists:keymember(Value, 2, Dict).

filter(Fun, Dict) ->
    orddict:filter(Fun, Dict).

fold(Fun, Acc0, Dict) ->
    orddict:fold(Fun, Acc0, Dict).

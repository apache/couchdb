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

-module(couch_lists).

-export([uniq/1]).

%% uniq/1: return a new list with the unique elements of the given list

-spec uniq(List1) -> List2 when
    List1 :: [T],
    List2 :: [T],
    T :: term().

-if((?OTP_RELEASE) >= 25).
uniq(L) ->
    lists:uniq(L).
-else.
uniq(L) ->
    uniq_1(L, #{}).

uniq_1([X | Xs], M) ->
    case is_map_key(X, M) of
        true ->
            uniq_1(Xs, M);
        false ->
            [X | uniq_1(Xs, M#{X => true})]
    end;
uniq_1([], _) ->
    [].
-endif.

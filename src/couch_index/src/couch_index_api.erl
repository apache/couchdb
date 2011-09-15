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



get(Field, State) ->
    ok.


open(Db, State) ->
    ok.

close(State) ->
    ok.

delete(State) ->
    ok.

reset(State) ->
    ok.


start_update(State) ->
    {ok, State}.

purge(PurgedIdRevs, State) ->
    ok.

process_doc(Doc, State) ->
    ok.

finish_update(State) ->
    {ok, State}.

commit(State) ->
    ok.


compact(Parent, State, Opts) ->
    ok.

swap_compacted(OldState, NewState) ->
    ok.

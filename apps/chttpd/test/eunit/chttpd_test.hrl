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


% Borrowed from fabric2_test.hrl

% Some test modules do not use with, so squash the unused fun compiler warning
-compile([{nowarn_unused_function, [{with, 1}]}]).


-define(TDEF(Name), {atom_to_list(Name), fun Name/1}).
-define(TDEF(Name, Timeout), {atom_to_list(Name), Timeout, fun Name/1}).

-define(TDEF_FE(Name), fun(Arg) -> {atom_to_list(Name), ?_test(Name(Arg))} end).
-define(TDEF_FE(Name, Timeout), fun(Arg) -> {atom_to_list(Name), {timeout, Timeout, ?_test(Name(Arg))}} end).


with(Tests) ->
    fun(ArgsTuple) ->
        lists:map(fun
            ({Name, Fun}) ->
                {Name, ?_test(Fun(ArgsTuple))};
            ({Name, Timeout, Fun}) ->
                {Name, {timeout, Timeout, ?_test(Fun(ArgsTuple))}}
        end, Tests)
    end.

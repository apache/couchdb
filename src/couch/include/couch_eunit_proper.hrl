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

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(EUNIT_QUICKCHECK(QuickcheckTimeout),
    [
        {
            atom_to_list(F),
            {timeout, QuickcheckTimeout,
                ?_assert(proper:quickcheck(?MODULE:F(), [
                    {to_file, user},
                    {start_size, 2},
                    long_result
                 ]))}
            }
        || {F, 0} <- ?MODULE:module_info(exports), F > 'prop_', F < 'prop`'
    ]).

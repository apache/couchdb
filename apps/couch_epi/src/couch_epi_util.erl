% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_epi_util).

-export([module_version/1, hash/1, module_exists/1]).

-compile([nowarn_deprecated_function]).

module_version(Module) ->
    Attributes = Module:module_info(attributes),
    {vsn, VSNs} = lists:keyfind(vsn, 1, Attributes),
    VSNs.

hash(Term) ->
    <<SigInt:128/integer>> = couch_hash:md5_hash(term_to_binary(Term)),
    lists:flatten(io_lib:format("\"~.36B\"", [SigInt])).

module_exists(Module) ->
    erlang:function_exported(Module, module_info, 0).

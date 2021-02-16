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

-module(couch_term).

-include("couch_db.hrl").
-include("couch_db_int.hrl").

-export([
    format_record/1,
    format_term/1,
    format_fun/1,
    format_pid/1,
    format_port/1,
    format_ref/1,
    record_to_map/1
]).

format_record(#db{} = Db) ->
    couch_db:format_record(Db);

format_record(#user_ctx{} = UserCtx) ->
    ?record_to_map(user_ctx, UserCtx);

format_record(_) ->
    nil.

format_term(Fun) when is_function(Fun) ->
    format_fun(Fun);

format_term(Atom) when is_atom(Atom) ->
    Atom;

format_term(Pid) when is_pid(Pid) ->
    format_pid(Pid);

format_term(Number) when is_number(Number) ->
    Number;

format_term(Reference) when is_reference(Reference) ->
    format_ref(Reference);

format_term(Port) when is_port(Port) ->
    format_ref(Port);

% we strip any term which can leak user data
format_term(_) ->
    nil.


format_fun(Fun) when is_function(Fun) ->
    {module, M} = erlang:fun_info(Fun, module),
    {name, F} = erlang:fun_info(Fun, name),
    {arity, A} = erlang:fun_info(Fun, arity),
    format_fun({M, F, A});
    
format_fun({M, F, A}) when is_atom(M) andalso is_atom(F) andalso is_integer(A) ->
    Str = io_lib:format("~s:~s/~b", [M, F, A]),
    lists:flatten(Str);

format_fun(Term) -> 
    format_term(Term).

format_pid(Pid) when is_pid(Pid) ->
    pid_to_list(Pid);

format_pid(Term) ->
    format_term(Term).

format_port(Port) when is_port(Port) ->
    erlang:port_to_list(Port);

format_port(Term) -> 
    format_term(Term).

format_ref(Reference) when is_reference(Reference) ->
    erlang:ref_to_list(Reference);

format_ref(Term) -> 
    format_term(Term).

record_to_map(#db{} = Db) -> 
    ?record_to_map(db, Db);

record_to_map(_) ->
        #{}.

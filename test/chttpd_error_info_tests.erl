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

-module(chttpd_error_info_tests).

-include_lib("eunit/include/eunit.hrl").


error_info_test() ->
    Error = <<"error">>,
    Reason = <<"reason">>,
    ArgResult = [
        {
            bad_request,
            {400, <<"bad_request">>, <<>>}
        },
        {
            {bad_request, Reason},
            {400, <<"bad_request">>, Reason}
        },
        {
            {bad_request, "error", "reason"},
            {400, Error, Reason}
        },
        {
            {query_parse_error, Reason},
            {400, <<"query_parse_error">>, Reason}
        },
        {
            database_does_not_exist,
            {404, <<"not_found">>, <<"Database does not exist.">>}
        },
        {
            not_found,
            {404, <<"not_found">>, <<"missing">>}
        },
        {
            {not_found, Reason},
            {404, <<"not_found">>, Reason}
        },
        {
            {not_acceptable, Reason},
            {406, <<"not_acceptable">>, Reason}
        },
        {
            conflict,
            {409, <<"conflict">>, <<"Document update conflict.">>}
        },
        {
            {conflict, Reason},
            %% yes, the reason is ignored
            {409, <<"conflict">>, <<"Document update conflict.">>}
        },
        {
            {forbidden, Reason},
            {403, <<"forbidden">>, Reason}
        },
        {
            {forbidden, Error, Reason},
            {403, Error, Reason}
        },
        {
            {unauthorized, Reason},
            {401, <<"unauthorized">>, Reason}
        },
        {
            file_exists,
            {412, <<"file_exists">>,
             <<"The database could not be created, the file already exists.">>}
        },
        {
            {r_quorum_not_met, Reason},
            {412, <<"read_quorum_not_met">>, Reason}
        },
        {
            {error, {nodedown, Reason}}, {412, <<"nodedown">>, Reason}
        },
        {
            {maintenance_mode, Reason},
            {412, <<"nodedown">>, Reason}
        },
        {
            {maintenance_mode, nil, Reason},
            {412, <<"nodedown">>, Reason}
        },
        {
            {w_quorum_not_met, Reason},
            {500, <<"write_quorum_not_met">>, Reason}
        },
        {
            request_uri_too_long,
            {414, <<"too_long">>, <<"the request uri is too long">>}
        },
        {
            {bad_ctype, Reason},
            {415, <<"bad_content_type">>, Reason}
        },
        {
            requested_range_not_satisfiable,
            {416, <<"requested_range_not_satisfiable">>,
             <<"Requested range not satisfiable">>}
        },
        {
            {error, illegal_database_name},
            {400, <<"illegal_database_name">>,
             <<"Only lowercase letters (a-z), digits (0-9), and any of"
               " the characters _, $, (, ), +, -, and / are allowed."
               " Moreover, the database name must begin with a letter.">>}
        },
        {
            {missing_stub, Reason},
            {412, <<"missing_stub">>, Reason}
        },
        {
            request_entity_too_large,
            {413, <<"too_large">>, <<"the request entity is too large">>}
        },
        {
            not_implemented,
            {501, <<"not_implemented">>,
             <<"this feature is not yet implemented">>}
        },
        {
            timeout,
            {500, <<"timeout">>,
             <<"The request could not be processed in a reasonable"
               " amount of time.">>}
        },
        {
            {timeout, Error},
            {500, <<"timeout">>,
             <<"The request could not be processed in a reasonable"
               " amount of time.">>}
        },
        {
            {Error, null},
            {500, <<"unknown_error">>, Error}
        },
        {
            {Error, Reason},
            {500, Error, Reason}
        },
        {
            {Error, nil, [{}]},
            {500, <<"unknown_error">>, Error}
        },
        {
            {Error, Reason, [{}]},
            {500, Error, Reason}
        },
        {
            Error,
            {500, <<"unknown_error">>, Error}
        }
    ],

    lists:foreach(fun({Arg, Result}) ->
        ?assertEqual(Result, apply(chttpd, error_info, [Arg]))
    end, ArgResult).

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

-include_lib("eunit/include/eunit.hrl").

-define(BUILDDIR,
    fun() ->
        case os:getenv("BUILDDIR") of
            false ->
                throw("BUILDDIR environment variable must be set");
            Dir ->
                Dir
        end
    end).
-define(CONFIG_DEFAULT,
    filename:join([?BUILDDIR(), "tmp", "etc", "default_eunit.ini"])).
-define(CONFIG_CHAIN, [
    ?CONFIG_DEFAULT,
    filename:join([?BUILDDIR(), "tmp", "etc", "local_eunit.ini"]),
    filename:join([?BUILDDIR(), "tmp", "etc", "eunit.ini"])]).
-define(FIXTURESDIR,
    filename:join([?BUILDDIR(), "src", "couch", "test", "eunit", "fixtures"])).
-define(TEMPDIR,
    filename:join([?BUILDDIR(), "tmp", "tmp_data"])).

-define(APPDIR, filename:dirname(element(2, file:get_cwd()))).
%% Account for the fact that source files are in src/<app>/.eunit/<module>.erl
%% when run from eunit
-define(ABS_PATH(File), %% src/<app>/.eunit/<module>.erl
    filename:join([?APPDIR, File])).

-define(tempfile,
    fun() ->
        Suffix = couch_uuids:random(),
        FileName = io_lib:format("~p-~s", [node(), Suffix]),
        filename:join([?TEMPDIR, FileName])
    end).
-define(tempdb,
    fun() ->
        Suffix = couch_uuids:random(),
        iolist_to_binary(["eunit-test-db-", Suffix])
    end).
-define(tempshard,
    fun() ->
        Suffix = couch_uuids:random(),
        iolist_to_binary(["shards/80000000-ffffffff/eunit-test-db-", Suffix])
    end).
-define(docid,
    fun() ->
        integer_to_list(couch_util:unique_monotonic_integer())
    end).

%% Like assertEqual, but using == instead of =:=
-ifndef(assertEquiv).
-define(assertEquiv(Expect, Expr),
	((fun (__X) ->
        case (Expr) of
        __V when __V == __X -> ok;
        __Y -> erlang:error({assertEquiv_failed,
				      [{module, ?MODULE},
				       {line, ?LINE},
				       {expression, (??Expr)},
				       {expected, __X},
				       {value, __Y}]})
	    end
	  end)(Expect))).
-endif.
-define(_assertEquiv(Expect, Expr), ?_test(?assertEquiv(Expect, Expr))).

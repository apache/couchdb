%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%
%% -------------------------------------------------------------------

-module(rebar_require_vsn).

-include("rebar.hrl").

-export([compile/2,
         eunit/2]).

%% for internal use only
-export([info/2]).

%% ===================================================================
%% Public API
%% ===================================================================

compile(Config, _) ->
    check_versions(Config).

eunit(Config, _) ->
    check_versions(Config).

%% ====================================================================
%% Internal functions
%% ====================================================================

info(help, compile) ->
    info_help();
info(help, eunit) ->
    info_help().

info_help() ->
    ?CONSOLE(
       "Check required ERTS or OTP release version.~n"
       "~n"
       "Valid rebar.config options:~n"
       "  ~p~n"
       "  ~p~n"
       "  ~p~n",
       [
        {require_erts_vsn, ".*"},
        {require_otp_vsn, ".*"},
        {require_min_otp_vsn, ".*"}
       ]).

check_versions(Config) ->
    ErtsRegex = rebar_config:get(Config, require_erts_vsn, ".*"),
    ReOpts = [{capture, none}],
    case re:run(erlang:system_info(version), ErtsRegex, ReOpts) of
        match ->
            ?DEBUG("Matched required ERTS version: ~s -> ~s\n",
                   [erlang:system_info(version), ErtsRegex]);
        nomatch ->
            ?ABORT("ERTS version ~s does not match required regex ~s\n",
                   [erlang:system_info(version), ErtsRegex])
    end,

    OtpRegex = rebar_config:get(Config, require_otp_vsn, ".*"),
    case re:run(erlang:system_info(otp_release), OtpRegex, ReOpts) of
        match ->
            ?DEBUG("Matched required OTP release: ~s -> ~s\n",
                   [erlang:system_info(otp_release), OtpRegex]);
        nomatch ->
            ?ABORT("OTP release ~s does not match required regex ~s\n",
                   [erlang:system_info(otp_release), OtpRegex])
    end,

    case rebar_config:get(Config, require_min_otp_vsn, undefined) of
        undefined -> ?DEBUG("Min OTP version unconfigured~n", []);
        MinOtpVsn ->
            {MinMaj, MinMin} = version_tuple(MinOtpVsn, "configured"),
            {OtpMaj, OtpMin} = version_tuple(erlang:system_info(otp_release),
                                             "OTP Release"),
            case {OtpMaj, OtpMin} >= {MinMaj, MinMin} of
                true ->
                    ?DEBUG("~s satisfies the requirement for vsn ~s~n",
                           [erlang:system_info(otp_release),
                            MinOtpVsn]);
                false ->
                    ?ABORT("OTP release ~s or later is required, you have: ~s~n",
                           [MinOtpVsn,
                            erlang:system_info(otp_release)])
            end
    end.

version_tuple(OtpRelease, Type) ->
    case re:run(OtpRelease, "R(\\d+)B?-?(\\d+)?", [{capture, all, list}]) of
        {match, [_Full, Maj, Min]} ->
            {list_to_integer(Maj), list_to_integer(Min)};
        {match, [_Full, Maj]} ->
            {list_to_integer(Maj), 0};
        nomatch ->
            ?ABORT("Cannot parse ~s version string: ~s~n",
                   [Type, OtpRelease])
    end.

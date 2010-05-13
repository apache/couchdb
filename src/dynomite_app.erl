-module(dynomite_app).
-author('cliff@powerset.com').
-author('brad@cloudant.com').

-behaviour(application).

-include("../include/config.hrl").
-include("../../couch/src/couch_db.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start(Type, StartArgs) -> {ok, Pid} |
%%                                 {ok, Pid, State} |
%%                                 {error, Reason}
%% @doc This function is called whenever an application
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% @end
%%--------------------------------------------------------------------


%% @doc start required apps, join cluster, start dynomite supervisor
start(_Type, _StartArgs) ->
    % start dynomite supervisor
    dynomite_sup:start_link().


%%--------------------------------------------------------------------
%% @spec stop(State) -> void()
%% @doc This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
stop({_, Sup}) ->
    showroom_log:message(alert, "dynomite application stopped", []),
    exit(Sup, normal),
    ok.


%%====================================================================
%% Internal functions
%%====================================================================

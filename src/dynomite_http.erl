%%%-------------------------------------------------------------------
%%% File    : dynomite_http.erl
%%% Author  : Brad Anderson <brad@cloudant.com>
%%% Description :
%%%
%%% Created : 10 Jan 2010 by Brad Anderson <brad@cloudant.com>
%%%-------------------------------------------------------------------
-module(dynomite_http).
-author('Brad Anderson <brad@cloudant.com>').

-include("../couch/src/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([handle_cluster_info/1]).


%% GET /_cluster
handle_cluster_info(#httpd{method='GET', path_parts=[_]}=Req) ->
    ClusterInfo = [{<<"ping_node">>, ?l2b(atom_to_list(node()))}],
    showroom_log:message(info, "Cluster Info: ~p", [ClusterInfo]),
    couch_httpd:send_json(Req, {ClusterInfo}).

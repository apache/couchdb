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

-module(couch_replicator_ids).

-export([
    replication_id/1,
    replication_id/2,
    convert/1
]).

-include_lib("couch/include/couch_db.hrl").
-include("couch_replicator_api_wrap.hrl").
-include("couch_replicator.hrl").

% replication_id/1 and replication_id/2 will attempt to fetch
% filter code for filtered replications. If fetching or parsing
% of the remotely fetched filter code fails they throw:
%  {filter_fetch_error, Error} exception.
%

replication_id(#rep{options = Options} = Rep) ->
    BaseId = replication_id(Rep, ?REP_ID_VERSION),
    {BaseId, maybe_append_options([continuous, create_target], Options)}.


% Versioned clauses for generating replication IDs.
% If a change is made to how replications are identified,
% please add a new clause and increase ?REP_ID_VERSION.

replication_id(#rep{user_ctx = UserCtx} = Rep, 3) ->
    UUID = couch_server:get_uuid(),
    Src = get_rep_endpoint(UserCtx, Rep#rep.source),
    Tgt = get_rep_endpoint(UserCtx, Rep#rep.target),
    maybe_append_filters([UUID, Src, Tgt], Rep);

replication_id(#rep{user_ctx = UserCtx} = Rep, 2) ->
    {ok, HostName} = inet:gethostname(),
    Port = case (catch mochiweb_socket_server:get(couch_httpd, port)) of
    P when is_number(P) ->
        P;
    _ ->
        % On restart we might be called before the couch_httpd process is
        % started.
        % TODO: we might be under an SSL socket server only, or both under
        % SSL and a non-SSL socket.
        % ... mochiweb_socket_server:get(https, port)
        list_to_integer(config:get("httpd", "port", "5984"))
    end,
    Src = get_rep_endpoint(UserCtx, Rep#rep.source),
    Tgt = get_rep_endpoint(UserCtx, Rep#rep.target),
    maybe_append_filters([HostName, Port, Src, Tgt], Rep);

replication_id(#rep{user_ctx = UserCtx} = Rep, 1) ->
    {ok, HostName} = inet:gethostname(),
    Src = get_rep_endpoint(UserCtx, Rep#rep.source),
    Tgt = get_rep_endpoint(UserCtx, Rep#rep.target),
    maybe_append_filters([HostName, Src, Tgt], Rep).


-spec convert([_] | binary() | {string(), string()}) -> {string(), string()}.
convert(Id) when is_list(Id) ->
    convert(?l2b(Id));
convert(Id) when is_binary(Id) ->
    lists:splitwith(fun(Char) -> Char =/= $+ end, ?b2l(Id));
convert({BaseId, Ext} = Id) when is_list(BaseId), is_list(Ext) ->
    Id.


% Private functions

maybe_append_filters(Base,
        #rep{source = Source, user_ctx = UserCtx, options = Options}) ->
    Base2 = Base ++
        case couch_replicator_filters:parse(Options) of
        {ok, nil} ->
            [];
        {ok, {view, Filter, QueryParams}} ->
            [Filter, QueryParams];
        {ok, {user, {Doc, Filter}, QueryParams}} ->
            case couch_replicator_filters:fetch(Doc, Filter, Source, UserCtx) of
                {ok, Code} ->
                    [Code, QueryParams];
                {error, Error} ->
                    throw({filter_fetch_error, Error})
            end;
        {ok, {docids, DocIds}} ->
            [DocIds];
        {ok, {mango, Selector}} ->
            [Selector];
        {error, FilterParseError} ->
            throw({error, FilterParseError})
        end,
    couch_util:to_hex(couch_crypto:hash(md5, term_to_binary(Base2))).


maybe_append_options(Options, RepOptions) ->
    lists:foldl(fun(Option, Acc) ->
        Acc ++
        case couch_util:get_value(Option, RepOptions, false) of
        true ->
            "+" ++ atom_to_list(Option);
        false ->
            ""
        end
    end, [], Options).


get_rep_endpoint(_UserCtx, #httpdb{url=Url, headers=Headers, oauth=OAuth}) ->
    DefaultHeaders = (#httpdb{})#httpdb.headers,
    case OAuth of
    nil ->
        {remote, Url, Headers -- DefaultHeaders};
    #oauth{} ->
        {remote, Url, Headers -- DefaultHeaders, OAuth}
    end;
get_rep_endpoint(UserCtx, <<DbName/binary>>) ->
    {local, DbName, UserCtx}.

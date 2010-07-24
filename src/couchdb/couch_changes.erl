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

-module(couch_changes).
-include("couch_db.hrl").

-export([handle_changes/3]).

%% @type Req -> #httpd{} | {json_req, JsonObj()}
handle_changes(#changes_args{style=Style}=Args1, Req, Db) ->
    Args = Args1#changes_args{filter=
            make_filter_fun(Args1#changes_args.filter, Style, Req, Db)},
    StartSeq = case Args#changes_args.dir of
    rev ->
        couch_db:get_update_seq(Db);
    fwd ->
        Args#changes_args.since
    end,
    if Args#changes_args.feed == "continuous" orelse
        Args#changes_args.feed == "longpoll" ->
        fun(Callback) ->
            Self = self(),
            {ok, Notify} = couch_db_update_notifier:start_link(
                fun({_, DbName}) when DbName == Db#db.name ->
                    Self ! db_updated;
                (_) ->
                    ok
                end
            ),
            start_sending_changes(Callback, Args#changes_args.feed),
            {Timeout, TimeoutFun} = get_changes_timeout(Args, Callback),
            try
                keep_sending_changes(
                    Args,
                    Callback,
                    Db,
                    StartSeq,
                    <<"">>,
                    Timeout,
                    TimeoutFun
                )
            after
                couch_db_update_notifier:stop(Notify),
                get_rest_db_updated() % clean out any remaining update messages
            end
        end;
    true ->
        fun(Callback) ->
            start_sending_changes(Callback, Args#changes_args.feed),
            {ok, {_, LastSeq, _Prepend, _, _, _, _, _}} =
                send_changes(
                    Args#changes_args{feed="normal"},
                    Callback,
                    Db,
                    StartSeq,
                    <<"">>
                ),
            end_sending_changes(Callback, LastSeq, Args#changes_args.feed)
        end
    end.

%% @type Req -> #httpd{} | {json_req, JsonObj()}
make_filter_fun(FilterName, Style, Req, Db) ->
    case [list_to_binary(couch_httpd:unquote(Part))
            || Part <- string:tokens(FilterName, "/")] of
    [] ->
        fun(#doc_info{revs=[#rev_info{rev=Rev}|_]=Revs}) ->
            case Style of
            main_only ->
                [{[{<<"rev">>, couch_doc:rev_to_str(Rev)}]}];
            all_docs ->
                [{[{<<"rev">>, couch_doc:rev_to_str(R)}]}
                        || #rev_info{rev=R} <- Revs]
            end
        end;
    [DName, FName] ->
        DesignId = <<"_design/", DName/binary>>,
        DDoc = couch_httpd_db:couch_doc_open(Db, DesignId, nil, []),
        % validate that the ddoc has the filter fun
        #doc{body={Props}} = DDoc,
        couch_util:get_nested_json_value({Props}, [<<"filters">>, FName]),
        fun(DocInfo) ->
            DocInfos =
            case Style of
            main_only ->
                [DocInfo];
            all_docs ->
                [DocInfo#doc_info{revs=[Rev]}|| Rev <- DocInfo#doc_info.revs]
            end,
            Docs = [Doc || {ok, Doc} <- [
                    couch_db:open_doc(Db, DocInfo2, [deleted, conflicts])
                        || DocInfo2 <- DocInfos]],
            {ok, Passes} = couch_query_servers:filter_docs(
                Req, Db, DDoc, FName, Docs
            ),
            [{[{<<"rev">>, couch_doc:rev_to_str({RevPos,RevId})}]}
                || {Pass, #doc{revs={RevPos,[RevId|_]}}}
                <- lists:zip(Passes, Docs), Pass == true]
        end;
    _Else ->
        throw({bad_request,
            "filter parameter must be of the form `designname/filtername`"})
    end.

get_changes_timeout(Args, Callback) ->
    #changes_args{
        heartbeat = Heartbeat,
        timeout = Timeout,
        feed = ResponseType
    } = Args,
    DefaultTimeout = list_to_integer(
        couch_config:get("httpd", "changes_timeout", "60000")
    ),
    case Heartbeat of
    undefined ->
        case Timeout of
        undefined ->
            {DefaultTimeout, fun() -> stop end};
        infinity ->
            {infinity, fun() -> stop end};
        _ ->
            {lists:min([DefaultTimeout, Timeout]), fun() -> stop end}
        end;
    true ->
        {DefaultTimeout, fun() -> Callback(timeout, ResponseType), ok end};
    _ ->
        {lists:min([DefaultTimeout, Heartbeat]),
            fun() -> Callback(timeout, ResponseType), ok end}
    end.

start_sending_changes(_Callback, "continuous") ->
    ok;
start_sending_changes(Callback, ResponseType) ->
    Callback(start, ResponseType).

send_changes(Args, Callback, Db, StartSeq, Prepend) ->
    #changes_args{
        style = Style,
        include_docs = IncludeDocs,
        limit = Limit,
        feed = ResponseType,
        dir = Dir,
        filter = FilterFun
    } = Args,
    couch_db:changes_since(
        Db,
        Style,
        StartSeq,
        fun changes_enumerator/2,
        [{dir, Dir}],
        {Db, StartSeq, Prepend, FilterFun, Callback, ResponseType, Limit,
            IncludeDocs}
    ).

keep_sending_changes(Args, Callback, Db, StartSeq, Prepend, Timeout,
    TimeoutFun) ->
    #changes_args{
        feed = ResponseType,
        limit = Limit
    } = Args,
    % ?LOG_INFO("send_changes start ~p",[StartSeq]),
    {ok, {_, EndSeq, Prepend2, _, _, _, NewLimit, _}} = send_changes(
        Args#changes_args{dir=fwd}, Callback, Db, StartSeq, Prepend
    ),
    % ?LOG_INFO("send_changes last ~p",[EndSeq]),
    couch_db:close(Db),
    if Limit > NewLimit, ResponseType == "longpoll" ->
        end_sending_changes(Callback, EndSeq, ResponseType);
    true ->
        case wait_db_updated(Timeout, TimeoutFun) of
        updated ->
            % ?LOG_INFO("wait_db_updated updated ~p",[{Db#db.name, EndSeq}]),
            case couch_db:open(Db#db.name, [{user_ctx, Db#db.user_ctx}]) of
            {ok, Db2} ->
                keep_sending_changes(
                    Args#changes_args{limit=NewLimit},
                    Callback,
                    Db2,
                    EndSeq,
                    Prepend2,
                    Timeout,
                    TimeoutFun
                );
            _Else ->
                end_sending_changes(Callback, EndSeq, ResponseType)
            end;
        stop ->
            % ?LOG_INFO("wait_db_updated stop ~p",[{Db#db.name, EndSeq}]),
            end_sending_changes(Callback, EndSeq, ResponseType)
        end
    end.

end_sending_changes(Callback, EndSeq, ResponseType) ->
    Callback({stop, EndSeq}, ResponseType).

changes_enumerator(DocInfo, {Db, _, _, FilterFun, Callback, "continuous",
    Limit, IncludeDocs}) ->

    #doc_info{id=Id, high_seq=Seq,
            revs=[#rev_info{deleted=Del,rev=Rev}|_]} = DocInfo,
    Results0 = FilterFun(DocInfo),
    Results = [Result || Result <- Results0, Result /= null],
    Go = if Limit =< 1 -> stop; true -> ok end,
    case Results of
    [] ->
        {Go, {Db, Seq, nil, FilterFun, Callback, "continuous", Limit,
                IncludeDocs}
        };
    _ ->
        ChangesRow = changes_row(Db, Seq, Id, Del, Results, Rev, IncludeDocs),
        Callback({change, ChangesRow, <<"">>}, "continuous"),
        {Go, {Db, Seq, nil, FilterFun, Callback, "continuous",  Limit - 1,
                IncludeDocs}
        }
    end;
changes_enumerator(DocInfo, {Db, _, Prepend, FilterFun, Callback, ResponseType,
    Limit, IncludeDocs}) ->

    #doc_info{id=Id, high_seq=Seq, revs=[#rev_info{deleted=Del,rev=Rev}|_]}
        = DocInfo,
    Results0 = FilterFun(DocInfo),
    Results = [Result || Result <- Results0, Result /= null],
    Go = if Limit =< 1 -> stop; true -> ok end,
    case Results of
    [] ->
        {Go, {Db, Seq, Prepend, FilterFun, Callback, ResponseType, Limit,
                IncludeDocs}
        };
    _ ->
        ChangesRow = changes_row(Db, Seq, Id, Del, Results, Rev, IncludeDocs),
        Callback({change, ChangesRow, Prepend}, ResponseType),
        {Go, {Db, Seq, <<",\n">>, FilterFun, Callback, ResponseType, Limit - 1,
                IncludeDocs}
        }
    end.


changes_row(Db, Seq, Id, Del, Results, Rev, true) ->
    {[{<<"seq">>, Seq}, {<<"id">>, Id}, {<<"changes">>, Results}] ++
        deleted_item(Del) ++ couch_httpd_view:doc_member(Db, {Id, Rev})};
changes_row(_, Seq, Id, Del, Results, _, false) ->
    {[{<<"seq">>, Seq}, {<<"id">>, Id}, {<<"changes">>, Results}] ++
        deleted_item(Del)}.

deleted_item(true) -> [{deleted, true}];
deleted_item(_) -> [].

% waits for a db_updated msg, if there are multiple msgs, collects them.
wait_db_updated(Timeout, TimeoutFun) ->
    receive db_updated -> get_rest_db_updated()
    after Timeout ->
        case TimeoutFun() of
        ok -> wait_db_updated(Timeout, TimeoutFun);
        stop -> stop
        end
    end.

get_rest_db_updated() ->
    receive db_updated -> get_rest_db_updated()
    after 0 -> updated
    end.

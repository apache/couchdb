-module(mango_cursor_view).

-export([
    execute/3
]).

-export([
    handle_message/2
]).


-include_lib("couch/include/couch_db.hrl").
-include("mango_cursor.hrl").


execute(#cursor{db = Db, index = Idx} = Cursor0, UserFun, UserAcc) ->
    Cursor = Cursor0#cursor{
        user_fun = UserFun,
        user_acc = UserAcc
    },
    BaseArgs = #view_query_args{
        view_type = red_map,
        start_key = mango_idx:start_key(Idx, Cursor#cursor.ranges),
        end_key = mango_idx:end_key(Idx, Cursor#cursor.ranges),
        include_docs = true
    },
    Args = apply_opts(Cursor#cursor.opts, BaseArgs),
    CB = fun ?MODULE:handle_message/2,
    {ok, LastCursor} = case mango_idx:def(Idx) of
        all_docs ->
            twig:log(err, "Query: ~s all_docs~n  ~p", [Db#db.name, Args]),
            fabric:all_docs(Db, CB, Cursor, Args);
        _ ->
            % Normal view
            DDoc = ddocid(Idx),
            Name = mango_idx:name(Idx),
            twig:log(err, "Query: ~s ~s ~s~n  ~p", [Db#db.name, DDoc, Name, Args]),
            fabric:query_view(Db, DDoc, Name, CB, Cursor, Args)
    end,
    {ok, LastCursor#cursor.user_acc}.


handle_message({total_and_offset, _, _} = _TO, Cursor) ->
    %twig:log(err, "TOTAL AND OFFSET: ~p", [_TO]),
    {ok, Cursor};
handle_message({row, {Props}}, Cursor) ->
    %twig:log(err, "ROW: ~p", [Props]),
    case doc_member(Cursor#cursor.db, Props, Cursor#cursor.opts) of
        {ok, Doc} ->
            case mango_selector:match(Cursor#cursor.selector, Doc) of
                true ->
                    handle_doc(Cursor, Doc);
                false ->
                    {ok, Cursor}
            end;
        Error ->
            twig:log(err, "~s :: Error loading doc: ~p", [?MODULE, Error]),
            {ok, Cursor}
    end;
handle_message(complete, Cursor) ->
    %twig:log(err, "COMPLETE", []),
    {ok, Cursor};
handle_message({error, Reason}, _Cursor) ->
    %twig:log(err, "ERROR: ~p", [Reason]),
    {error, Reason}.


handle_doc(#cursor{skip = N} = C, _) when N > 0 ->
    {ok, C#cursor{skip = N - 1}};
handle_doc(Cursor, Doc) ->
    UserFun = Cursor#cursor.user_fun,
    UserAcc = Cursor#cursor.user_acc,
    {Go, NewAcc} = UserFun({row, Doc}, UserAcc),
    NewCursor = Cursor#cursor{user_acc = NewAcc},
    case Cursor#cursor.limit of
        1 ->
            {stop, NewCursor};
        L when L > 1 ->
            {Go, NewCursor}
    end.


ddocid(Idx) ->
    case mango_idx:ddoc(Idx) of
        <<"_design/", Rest/binary>> ->
            Rest;
        Else ->
            Else
    end.


apply_opts([], Args) ->
    Args;
apply_opts([{r, RStr} | Rest], Args) ->
    IncludeDocs = case list_to_integer(RStr) of
        1 ->
            true;
        R when R > 1 ->
            % We don't load the doc in the view query because
            % we have to do a quorum read in the coordinator
            % so there's no point.
            false
    end,
    NewArgs = Args#view_query_args{include_docs = IncludeDocs},
    apply_opts(Rest, NewArgs);
apply_opts([{conflicts, true} | Rest], Args) ->
    % I need to patch things so that views can specify
    % parameters when loading the docs from disk
    apply_opts(Rest, Args);
apply_opts([{conflicts, false} | Rest], Args) ->
    % Ignored cause default
    apply_opts(Rest, Args);
apply_opts([{sort, Sort} | Rest], Args) ->
    % We only support single direction sorts
    % so nothing fancy here.
    case mango_sort:directions(Sort) of
        [] ->
            apply_opts(Rest, Args);
        [<<"asc">> | _] ->
            apply_opts(Rest, Args);
        [<<"desc">> | _] ->
            SK = Args#view_query_args.start_key,
            EK = Args#view_query_args.end_key,
            NewArgs = Args#view_query_args{
                direction = rev,
                start_key = EK,
                end_key = SK
            },
            apply_opts(Rest, NewArgs)
    end;
apply_opts([{_, _} | Rest], Args) ->
    % Ignore unknown options
    apply_opts(Rest, Args).


doc_member(Db, RowProps, Opts) ->
    Fields = couch_util:get_value(fields, Opts, all_fields),
    case load_doc(Db, RowProps, Opts) of
        {ok, Doc} when Fields /= all_fields ->
            {ok, mango_fields:extract(Doc, Fields)};
        Else ->
            Else
    end.


load_doc(Db, RowProps, Opts) ->
    case couch_util:get_value(doc, RowProps) of
        {DocProps} ->
            {ok, {DocProps}};
        undefined ->
            Id = couch_util:get_value(id, RowProps),
            case mango_util:defer(fabric, open_doc, [Db, Id, Opts]) of
                {ok, #doc{}=Doc} ->
                    {ok, couch_doc:to_json_obj(Doc, [])};
                Else ->
                    Else
            end
    end.


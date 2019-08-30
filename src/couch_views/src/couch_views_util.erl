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

-module(couch_views_util).


-export([
    ddoc_to_mrst/2,
    validate_args/1
]).


-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include("couch_views.hrl").


ddoc_to_mrst(DbName, #doc{id=Id, body={Fields}}) ->
    MakeDict = fun({Name, {MRFuns}}, DictBySrcAcc) ->
        case couch_util:get_value(<<"map">>, MRFuns) of
            MapSrc when MapSrc /= undefined ->
                RedSrc = couch_util:get_value(<<"reduce">>, MRFuns, null),
                {ViewOpts} = couch_util:get_value(<<"options">>, MRFuns, {[]}),
                View = case dict:find({MapSrc, ViewOpts}, DictBySrcAcc) of
                    {ok, View0} -> View0;
                    error -> #mrview{def=MapSrc, options=ViewOpts}
                end,
                {MapNames, RedSrcs} = case RedSrc of
                    null ->
                        MNames = [Name | View#mrview.map_names],
                        {MNames, View#mrview.reduce_funs};
                    _ ->
                        RedFuns = [{Name, RedSrc} | View#mrview.reduce_funs],
                        {View#mrview.map_names, RedFuns}
                end,
                View2 = View#mrview{map_names=MapNames, reduce_funs=RedSrcs},
                dict:store({MapSrc, ViewOpts}, View2, DictBySrcAcc);
            undefined ->
                DictBySrcAcc
        end;
        ({Name, Else}, DictBySrcAcc) ->
            couch_log:error("design_doc_to_view_group ~s views ~p",
                [Name, Else]),
            DictBySrcAcc
    end,
    {DesignOpts} = proplists:get_value(<<"options">>, Fields, {[]}),
    Partitioned = proplists:get_value(<<"partitioned">>, DesignOpts, false),

    {RawViews} = couch_util:get_value(<<"views">>, Fields, {[]}),
    BySrc = lists:foldl(MakeDict, dict:new(), RawViews),

    NumViews = fun({_, View}, N) ->
            {View#mrview{id_num = N}, N+1}
    end,
    {Views, _} = lists:mapfoldl(NumViews, 0, lists:sort(dict:to_list(BySrc))),

    Language = couch_util:get_value(<<"language">>, Fields, <<"javascript">>),
    Lib = couch_util:get_value(<<"lib">>, RawViews, {[]}),

    IdxState = #mrst{
        db_name=DbName,
        idx_name=Id,
        lib=Lib,
        views=Views,
        language=Language,
        design_opts=DesignOpts,
        partitioned=Partitioned
    },
    SigInfo = {Views, Language, DesignOpts, couch_index_util:sort_lib(Lib)},
    {ok, IdxState#mrst{sig=couch_hash:md5_hash(term_to_binary(SigInfo))}}.


% This is mostly a copy of couch_mrview_util:validate_args/1 but it doesn't
% update start / end keys and also throws a not_implemented error for reduce
%
validate_args(#mrargs{} = Args) ->
    GroupLevel = determine_group_level(Args),
    Reduce = Args#mrargs.reduce,

    case Reduce =/= undefined orelse Args#mrargs.view_type == red of
        true -> throw(not_implemented);
        false -> ok
    end,

    case Reduce == undefined orelse is_boolean(Reduce) of
        true -> ok;
        _ -> mrverror(<<"Invalid `reduce` value.">>)
    end,

    case {Args#mrargs.view_type, Reduce} of
        {map, true} -> mrverror(<<"Reduce is invalid for map-only views.">>);
        _ -> ok
    end,

    case {Args#mrargs.view_type, GroupLevel, Args#mrargs.keys} of
        {red, exact, _} -> ok;
        {red, _, KeyList} when is_list(KeyList) ->
            Msg = <<"Multi-key fetchs for reduce views must use `group=true`">>,
            mrverror(Msg);
        _ -> ok
    end,

    case Args#mrargs.keys of
        Keys when is_list(Keys) -> ok;
        undefined -> ok;
        _ -> mrverror(<<"`keys` must be an array of strings.">>)
    end,

    case {Args#mrargs.keys, Args#mrargs.start_key,
          Args#mrargs.end_key} of
        {undefined, _, _} -> ok;
        {[], _, _} -> ok;
        {[_|_], undefined, undefined} -> ok;
        _ -> mrverror(<<"`keys` is incompatible with `key`"
                        ", `start_key` and `end_key`">>)
    end,

    case Args#mrargs.start_key_docid of
        undefined -> ok;
        SKDocId0 when is_binary(SKDocId0) -> ok;
        _ -> mrverror(<<"`start_key_docid` must be a string.">>)
    end,

    case Args#mrargs.end_key_docid of
        undefined -> ok;
        EKDocId0 when is_binary(EKDocId0) -> ok;
        _ -> mrverror(<<"`end_key_docid` must be a string.">>)
    end,

    case Args#mrargs.direction of
        fwd -> ok;
        rev -> ok;
        _ -> mrverror(<<"Invalid direction.">>)
    end,

    case {Args#mrargs.limit >= 0, Args#mrargs.limit == undefined} of
        {true, _} -> ok;
        {_, true} -> ok;
        _ -> mrverror(<<"`limit` must be a positive integer.">>)
    end,

    case Args#mrargs.skip < 0 of
        true -> mrverror(<<"`skip` must be >= 0">>);
        _ -> ok
    end,

    case {Args#mrargs.view_type, GroupLevel} of
        {red, exact} -> ok;
        {_, 0} -> ok;
        {red, Int} when is_integer(Int), Int >= 0 -> ok;
        {red, _} -> mrverror(<<"`group_level` must be >= 0">>);
        {map, _} -> mrverror(<<"Invalid use of grouping on a map view.">>)
    end,

    case Args#mrargs.stable of
        true -> ok;
        false -> ok;
        _ -> mrverror(<<"Invalid value for `stable`.">>)
    end,

    case Args#mrargs.update of
        true -> ok;
        false -> ok;
        lazy -> ok;
        _ -> mrverror(<<"Invalid value for `update`.">>)
    end,

    case is_boolean(Args#mrargs.inclusive_end) of
        true -> ok;
        _ -> mrverror(<<"Invalid value for `inclusive_end`.">>)
    end,

    case {Args#mrargs.view_type, Args#mrargs.include_docs} of
        {red, true} -> mrverror(<<"`include_docs` is invalid for reduce">>);
        {_, ID} when is_boolean(ID) -> ok;
        _ -> mrverror(<<"Invalid value for `include_docs`">>)
    end,

    case {Args#mrargs.view_type, Args#mrargs.conflicts} of
        {_, undefined} -> ok;
        {map, V} when is_boolean(V) -> ok;
        {red, undefined} -> ok;
        {map, _} -> mrverror(<<"Invalid value for `conflicts`.">>);
        {red, _} -> mrverror(<<"`conflicts` is invalid for reduce views.">>)
    end,

    case is_boolean(Args#mrargs.sorted) of
        true -> ok;
        _ -> mrverror(<<"Invalid value for `sorted`.">>)
    end,

    Args#mrargs{group_level=GroupLevel}.


determine_group_level(#mrargs{group=undefined, group_level=undefined}) ->
    0;

determine_group_level(#mrargs{group=false, group_level=undefined}) ->
    0;

determine_group_level(#mrargs{group=false, group_level=Level}) when Level > 0 ->
    mrverror(<<"Can't specify group=false and group_level>0 at the same time">>);

determine_group_level(#mrargs{group=true, group_level=undefined}) ->
    exact;

determine_group_level(#mrargs{group_level=GroupLevel}) ->
    GroupLevel.


mrverror(Mesg) ->
    throw({query_parse_error, Mesg}).

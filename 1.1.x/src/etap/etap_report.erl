%% Copyright (c) 2008-2009 Nick Gerakines <nick@gerakines.net>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%
%% @doc A module for creating nice looking code coverage reports.
-module(etap_report).
-export([create/0]).

%% @spec create() -> ok
%% @doc Create html code coverage reports for each module that code coverage
%% data exists for.
create() ->
    [cover:import(File) || File <- filelib:wildcard("cover/*.coverdata")],
    Modules = lists:foldl(
        fun(Module, Acc) ->
            [{Module, file_report(Module)} | Acc]
        end,
        [],
        cover:imported_modules()
    ),
    index(Modules).

%% @private
index(Modules) ->
    {ok, IndexFD} = file:open("cover/index.html", [write]),
    io:format(IndexFD, "<html><head><style>
    table.percent_graph { height: 12px; border:1px solid #E2E6EF; empty-cells: show; }
    table.percent_graph td.covered { height: 10px; background: #00f000; }
    table.percent_graph td.uncovered { height: 10px; background: #e00000; }
    .odd { background-color: #ddd; }
    .even { background-color: #fff; }
    </style></head>", []),
    io:format(IndexFD, "<body>", []),
    lists:foldl(
        fun({Module, {Good, Bad, Source}}, LastRow) ->
            case {Good + Bad, Source} of
                {0, _} -> LastRow;
                {_, none} -> LastRow;
                _ ->
                    CovPer = round((Good / (Good + Bad)) * 100),
                    UnCovPer = round((Bad / (Good + Bad)) * 100),
                    RowClass = case LastRow of 1 -> "odd"; _ -> "even" end,
                    io:format(IndexFD, "<div class=\"~s\">", [RowClass]),
                    io:format(IndexFD, "<a href=\"~s\">~s</a>", [atom_to_list(Module) ++ "_report.html", atom_to_list(Module)]),
                    io:format(IndexFD, "
                    <table cellspacing='0' cellpadding='0' align='right'>
                      <tr>
                        <td><tt>~p%</tt>&nbsp;</td><td>
                          <table cellspacing='0' class='percent_graph' cellpadding='0' width='100'>
                          <tr><td class='covered' width='~p' /><td class='uncovered' width='~p' /></tr>
                          </table>
                        </td>
                      </tr>
                    </table>
                    ", [CovPer, CovPer, UnCovPer]),
                    io:format(IndexFD, "</div>", []),
                    case LastRow of
                        1 -> 0;
                        0 -> 1
                    end
            end
        end,
        0,
        lists:sort(Modules)
    ),
    {TotalGood, TotalBad} = lists:foldl(
        fun({_, {Good, Bad, Source}}, {TGood, TBad}) ->
            case Source of none -> {TGood, TBad}; _ -> {TGood + Good, TBad + Bad} end
        end,
        {0, 0},
        Modules
    ),
    io:format(IndexFD, "<p>Generated on ~s.</p>~n", [etap:datetime({date(), time()})]),
    case TotalGood + TotalBad of
        0 -> ok;
        _ ->
            TotalCovPer = round((TotalGood / (TotalGood + TotalBad)) * 100),
            TotalUnCovPer = round((TotalBad / (TotalGood + TotalBad)) * 100),
            io:format(IndexFD, "<div>", []),
            io:format(IndexFD, "Total 
            <table cellspacing='0' cellpadding='0' align='right'>
              <tr>
                <td><tt>~p%</tt>&nbsp;</td><td>
                  <table cellspacing='0' class='percent_graph' cellpadding='0' width='100'>
                  <tr><td class='covered' width='~p' /><td class='uncovered' width='~p' /></tr>
                  </table>
                </td>
              </tr>
            </table>
            ", [TotalCovPer, TotalCovPer, TotalUnCovPer]),
            io:format(IndexFD, "</div>", [])
    end,
    io:format(IndexFD, "</body></html>", []),
    file:close(IndexFD),
    ok.

%% @private
file_report(Module) ->
    {ok, Data} = cover:analyse(Module, calls, line),
    Source = find_source(Module),
    {Good, Bad} = collect_coverage(Data, {0, 0}),
    case {Source, Good + Bad} of
        {none, _} -> ok;
        {_, 0} -> ok;
        _ ->
            {ok, SourceFD} = file:open(Source, [read]),
            {ok, WriteFD} = file:open("cover/" ++ atom_to_list(Module) ++ "_report.html", [write]),
            io:format(WriteFD, "~s", [header(Module, Good, Bad)]),
            output_lines(Data, WriteFD, SourceFD, 1),
            io:format(WriteFD, "~s", [footer()]),
            file:close(WriteFD),
            file:close(SourceFD),
            ok
    end,
    {Good, Bad, Source}.

%% @private
collect_coverage([], Acc) -> Acc;
collect_coverage([{{_, _}, 0} | Data], {Good, Bad}) ->
    collect_coverage(Data, {Good, Bad + 1});
collect_coverage([_ | Data], {Good, Bad}) ->
    collect_coverage(Data, {Good + 1, Bad}).

%% @private
output_lines(Data, WriteFD, SourceFD, LineNumber) ->
    {Match, NextData} = datas_match(Data, LineNumber),
    case io:get_line(SourceFD, '') of
        eof -> ok;
        Line = "%% @todo" ++ _ ->
            io:format(WriteFD, "~s", [out_line(LineNumber, highlight, Line)]),
            output_lines(NextData, WriteFD, SourceFD, LineNumber + 1);
        Line = "% " ++ _ ->
            io:format(WriteFD, "~s", [out_line(LineNumber, none, Line)]),
            output_lines(NextData, WriteFD, SourceFD, LineNumber + 1);
        Line ->
            case Match of
                {true, CC} ->
                    io:format(WriteFD, "~s", [out_line(LineNumber, CC, Line)]),
                    output_lines(NextData, WriteFD, SourceFD, LineNumber + 1);
                false ->
                    io:format(WriteFD, "~s", [out_line(LineNumber, none, Line)]),
                    output_lines(NextData, WriteFD, SourceFD, LineNumber + 1)
            end
    end.

%% @private
out_line(Number, none, Line) ->
    PadNu = string:right(integer_to_list(Number), 5, $.),
    io_lib:format("<span class=\"marked\"><a name=\"line~p\"></a>~s ~s</span>", [Number, PadNu, Line]);
out_line(Number, highlight, Line) ->
    PadNu = string:right(integer_to_list(Number), 5, $.),
    io_lib:format("<span class=\"highlight\"><a name=\"line~p\"></a>~s ~s</span>", [Number, PadNu, Line]);
out_line(Number, 0, Line) ->
    PadNu = string:right(integer_to_list(Number), 5, $.),
    io_lib:format("<span class=\"uncovered\"><a name=\"line~p\"></a>~s ~s</span>", [Number, PadNu, Line]);
out_line(Number, _, Line) ->
    PadNu = string:right(integer_to_list(Number), 5, $.),
    io_lib:format("<span class=\"covered\"><a name=\"line~p\"></a>~s ~s</span>", [Number, PadNu, Line]).

%% @private
datas_match([], _) -> {false, []};
datas_match([{{_, Line}, CC} | Datas], LineNumber) when Line == LineNumber -> {{true, CC}, Datas};
datas_match(Data, _) -> {false, Data}.

%% @private
find_source(Module) when is_atom(Module) ->
    Root = filename:rootname(Module),
    Dir = filename:dirname(Root),
    XDir = case os:getenv("SRC") of false -> "src"; X -> X end,
    find_source([
        filename:join([Dir, Root ++ ".erl"]),
        filename:join([Dir, "..", "src", Root ++ ".erl"]),
        filename:join([Dir, "src", Root ++ ".erl"]),
        filename:join([Dir, "elibs", Root ++ ".erl"]),
        filename:join([Dir, "..", "elibs", Root ++ ".erl"]),
        filename:join([Dir, XDir, Root ++ ".erl"])
    ]);
find_source([]) -> none;
find_source([Test | Tests]) ->
    case filelib:is_file(Test) of
        true -> Test;
        false -> find_source(Tests)
    end.

%% @private
header(Module, Good, Bad) ->
    io:format("Good ~p~n", [Good]),
    io:format("Bad ~p~n", [Bad]),
    CovPer = round((Good / (Good + Bad)) * 100),
    UnCovPer = round((Bad / (Good + Bad)) * 100),
    io:format("CovPer ~p~n", [CovPer]),
    io_lib:format("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
        <html lang='en' xml:lang='en' xmlns='http://www.w3.org/1999/xhtml'>
          <head>
            <title>~s - C0 code coverage information</title>
            <style type='text/css'>body { background-color: rgb(240, 240, 245); }</style>
            <style type='text/css'>span.marked0 {
     background-color: rgb(185, 210, 200);
     display: block;
    }
    span.marked { display: block; background-color: #ffffff; }
    span.highlight { display: block; background-color: #fff9d7; }
    span.covered { display: block; background-color: #f7f7f7 ; }
    span.uncovered { display: block; background-color: #ffebe8 ; }
    span.overview {
     border-bottom: 1px solid #E2E6EF; 
    }
    div.overview {
     border-bottom: 1px solid #E2E6EF; 
    }
    body {
     font-family: verdana, arial, helvetica;
    }
    div.footer {
     font-size: 68%;
     margin-top: 1.5em;
    }
    h1, h2, h3, h4, h5, h6 {
     margin-bottom: 0.5em;
    }
    h5 {
     margin-top: 0.5em;
    }
    .hidden {
     display: none;
    }
    div.separator {
     height: 10px;
    }
    table.percent_graph {
     height: 12px;
     border: 1px solid #E2E6EF; 
     empty-cells: show;
    }
    table.percent_graph td.covered {
     height: 10px;
     background: #00f000;
    }
    table.percent_graph td.uncovered {
     height: 10px;
     background: #e00000;
    }
    table.percent_graph td.NA {
     height: 10px;
     background: #eaeaea;
    }
    table.report {
     border-collapse: collapse;
     width: 100%;
    }
    table.report td.heading {
     background: #dcecff;
     border: 1px solid #E2E6EF; 
     font-weight: bold;
     text-align: center;
    }
    table.report td.heading:hover {
     background: #c0ffc0;
    }
    table.report td.text {
     border: 1px solid #E2E6EF; 
    }
    table.report td.value {
     text-align: right;
     border: 1px solid #E2E6EF; 
    }
    table.report tr.light {
     background-color: rgb(240, 240, 245);
    }
    table.report tr.dark {
     background-color: rgb(230, 230, 235);
    }
    </style>
          </head>
          <body>
            <h3>C0 code coverage information</h3>
            <p>Generated on ~s with <a href='http://github.com/ngerakines/etap'>etap 0.3.4</a>.
            </p>            
        <table class='report'>
          <thead>
            <tr>
              <td class='heading'>Name</td>
              <td class='heading'>Total lines</td>
              <td class='heading'>Lines of code</td>
              <td class='heading'>Total coverage</td>
              <td class='heading'>Code coverage</td>
            </tr>
          </thead>
          <tbody>
            <tr class='light'>

              <td>
                <a href='~s'>~s</a>
              </td>
              <td class='value'>
                <tt>??</tt>
              </td>
              <td class='value'>
                <tt>??</tt>
              </td>
              <td class='value'>
                <tt>??</tt>
              </td>
              <td>
                <table cellspacing='0' cellpadding='0' align='right'>
                  <tr>
                    <td><tt>~p%</tt>&nbsp;</td><td>
                      <table cellspacing='0' class='percent_graph' cellpadding='0' width='100'>
                      <tr><td class='covered' width='~p' /><td class='uncovered' width='~p' /></tr>
                      </table>
                    </td>
                  </tr>
                </table>
              </td>
            </tr>
          </tbody>
        </table><pre>", [Module, etap:datetime({date(), time()}), atom_to_list(Module) ++ "_report.html", Module, CovPer, CovPer, UnCovPer]).

%% @private
footer() ->
    "</pre><hr /><p>Generated using <a href='http://github.com/ngerakines/etap'>etap 0.3.4</a>.</p>
          </body>
        </html>
    ".

%% The MIT License
%%
%% Copyright (c) 2009 Tom Preston-Werner <tom@mojombo.com>
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

%% See the README at http://github.com/mojombo/mustache.erl for additional
%% documentation and usage examples.

-module(mustache).  %% v0.1.0
-author("Tom Preston-Werner").
-export([compile/1, compile/2, render/1, render/2, render/3, get/2, get/3, escape/1, start/1]).

-record(mstate, {mod = undefined,
                 section_re = undefined,
                 tag_re = undefined}).

compile(Body) when is_list(Body) ->
  State = #mstate{},
  CompiledTemplate = pre_compile(Body, State),
  % io:format("~p~n~n", [CompiledTemplate]),
  % io:format(CompiledTemplate ++ "~n", []),
  {ok, Tokens, _} = erl_scan:string(CompiledTemplate),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  Bindings = erl_eval:new_bindings(),
  {value, Fun, _} = erl_eval:expr(Form, Bindings),
  Fun;
compile(Mod) ->
  TemplatePath = template_path(Mod),
  compile(Mod, TemplatePath).

compile(Mod, File) ->
  code:purge(Mod),
  {module, _} = code:load_file(Mod),
  {ok, TemplateBin} = file:read_file(File),
  Template = re:replace(TemplateBin, "\"", "\\\\\"", [global, {return,list}]),
  State = #mstate{mod = Mod},
  CompiledTemplate = pre_compile(Template, State),
  % io:format("~p~n~n", [CompiledTemplate]),
  % io:format(CompiledTemplate ++ "~n", []),
  {ok, Tokens, _} = erl_scan:string(CompiledTemplate),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  Bindings = erl_eval:new_bindings(),
  {value, Fun, _} = erl_eval:expr(Form, Bindings),
  Fun.

render(Mod) ->
  TemplatePath = template_path(Mod),
  render(Mod, TemplatePath).

render(Body, Ctx) when is_list(Body) ->
  TFun = compile(Body),
  render(undefined, TFun, Ctx);
render(Mod, File) when is_list(File) ->
  render(Mod, File, dict:new());
render(Mod, CompiledTemplate) ->
  render(Mod, CompiledTemplate, dict:new()).

render(Mod, File, Ctx) when is_list(File) ->
  CompiledTemplate = compile(Mod, File),
  render(Mod, CompiledTemplate, Ctx);
render(Mod, CompiledTemplate, Ctx) ->
  Ctx2 = dict:store('__mod__', Mod, Ctx),
  lists:flatten(CompiledTemplate(Ctx2)).

pre_compile(T, State) ->
  SectionRE = "\{\{\#([^\}]*)}}\s*(.+?){{\/\\1\}\}\s*",
  {ok, CompiledSectionRE} = re:compile(SectionRE, [dotall]),
  TagRE = "\{\{(#|=|!|<|>|\{)?(.+?)\\1?\}\}+",
  {ok, CompiledTagRE} = re:compile(TagRE, [dotall]),
  State2 = State#mstate{section_re = CompiledSectionRE, tag_re = CompiledTagRE},
  "fun(Ctx) -> " ++
    "CFun = fun(A, B) -> A end, " ++
    compiler(T, State2) ++ " end.".

compiler(T, State) ->
  Res = re:run(T, State#mstate.section_re),
  case Res of
    {match, [{M0, M1}, {N0, N1}, {C0, C1}]} ->
      Front = string:substr(T, 1, M0),
      Back = string:substr(T, M0 + M1 + 1),
      Name = string:substr(T, N0 + 1, N1),
      Content = string:substr(T, C0 + 1, C1),
      "[" ++ compile_tags(Front, State) ++
        " | [" ++ compile_section(Name, Content, State) ++
        " | [" ++ compiler(Back, State) ++ "]]]";
    nomatch ->
      compile_tags(T, State)
  end.

compile_section(Name, Content, State) ->
  Mod = State#mstate.mod,
  Result = compiler(Content, State),
  "fun() -> " ++
    "case mustache:get(" ++ Name ++ ", Ctx, " ++ atom_to_list(Mod) ++ ") of " ++
      "\"true\" -> " ++
        Result ++ "; " ++
      "\"false\" -> " ++
        "[]; " ++
      "List when is_list(List) -> " ++
        "[fun(Ctx) -> " ++ Result ++ " end(dict:merge(CFun, SubCtx, Ctx)) || SubCtx <- List]; " ++
      "Else -> " ++
        "throw({template, io_lib:format(\"Bad context for ~p: ~p\", [" ++ Name ++ ", Else])}) " ++
    "end " ++
  "end()".

compile_tags(T, State) ->
  Res = re:run(T, State#mstate.tag_re),
  case Res of
    {match, [{M0, M1}, K, {C0, C1}]} ->
      Front = string:substr(T, 1, M0),
      Back = string:substr(T, M0 + M1 + 1),
      Content = string:substr(T, C0 + 1, C1),
      Kind = tag_kind(T, K),
      Result = compile_tag(Kind, Content, State),
      "[\"" ++ Front ++
        "\" | [" ++ Result ++
        " | " ++ compile_tags(Back, State) ++ "]]";
    nomatch ->
      "[\"" ++ T ++ "\"]"
  end.

tag_kind(_T, {-1, 0}) ->
  none;
tag_kind(T, {K0, K1}) ->
  string:substr(T, K0 + 1, K1).

compile_tag(none, Content, State) ->
  Mod = State#mstate.mod,
  "mustache:escape(mustache:get(" ++ Content ++ ", Ctx, " ++ atom_to_list(Mod) ++ "))";
compile_tag("{", Content, State) ->
  Mod = State#mstate.mod,
  "mustache:get(" ++ Content ++ ", Ctx, " ++ atom_to_list(Mod) ++ ")";
compile_tag("!", _Content, _State) ->
  "[]".

template_dir(Mod) ->
  DefaultDirPath = filename:dirname(code:which(Mod)),
  case application:get_env(mustache, templates_dir) of
    {ok, DirPath} when is_list(DirPath) ->
      case filelib:ensure_dir(DirPath) of
        ok -> DirPath;
        _  -> DefaultDirPath
      end;
    _ ->
      DefaultDirPath
  end.
template_path(Mod) ->
  DirPath = template_dir(Mod),
  Basename = atom_to_list(Mod),
  filename:join(DirPath, Basename ++ ".mustache").

get(Key, Ctx) when is_list(Key) ->
  {ok, Mod} = dict:find('__mod__', Ctx),
  get(list_to_atom(Key), Ctx, Mod);
get(Key, Ctx) ->
  {ok, Mod} = dict:find('__mod__', Ctx),
  get(Key, Ctx, Mod).

get(Key, Ctx, Mod) when is_list(Key) ->
  get(list_to_atom(Key), Ctx, Mod);
get(Key, Ctx, Mod) ->
  case dict:find(Key, Ctx) of
    {ok, Val} ->
      % io:format("From Ctx {~p, ~p}~n", [Key, Val]),
      to_s(Val);
    error ->
      case erlang:function_exported(Mod, Key, 1) of
        true ->
          Val = to_s(Mod:Key(Ctx)),
          % io:format("From Mod/1 {~p, ~p}~n", [Key, Val]),
          Val;
        false ->
          case erlang:function_exported(Mod, Key, 0) of
            true ->
              Val = to_s(Mod:Key()),
              % io:format("From Mod/0 {~p, ~p}~n", [Key, Val]),
              Val;
            false ->
              []
          end
      end
  end.

to_s(Val) when is_integer(Val) ->
  integer_to_list(Val);
to_s(Val) when is_float(Val) ->
  io_lib:format("~.2f", [Val]);
to_s(Val) when is_atom(Val) ->
  atom_to_list(Val);
to_s(Val) ->
  Val.

escape(HTML) ->
  escape(HTML, []).

escape([], Acc) ->
  lists:reverse(Acc);
escape(["<" | Rest], Acc) ->
  escape(Rest, lists:reverse("&lt;", Acc));
escape([">" | Rest], Acc) ->
  escape(Rest, lists:reverse("&gt;", Acc));
escape(["&" | Rest], Acc) ->
  escape(Rest, lists:reverse("&amp;", Acc));
escape([X | Rest], Acc) ->
  escape(Rest, [X | Acc]).

%%---------------------------------------------------------------------------

start([T]) ->
  Out = render(list_to_atom(T)),
  io:format(Out ++ "~n", []).

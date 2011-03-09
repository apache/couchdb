%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with your Erlang distribution. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Corelatus AB.
%% Portions created by Corelatus are Copyright 2003, Corelatus
%% AB. All Rights Reserved.''
%%
%% Module to print out terms for logging. Limits by length rather than depth.
%%
%% The resulting string may be slightly larger than the limit; the intention
%% is to provide predictable CPU and memory consumption for formatting
%% terms, not produce precise string lengths.
%%
%% Typical use:
%%
%%   trunc_io:print(Term, 500).
%%
-module(trunc_io).
-author('matthias@corelatus.se').
%% And thanks to Chris Newcombe for a bug fix 
-export([print/2, fprint/2, safe/2]).               % interface functions
-export([perf/0, perf/3, perf1/0, test/0, test/2]). % testing functions
-version("$Id: trunc_io.erl,v 1.11 2009-02-23 12:01:06 matthias Exp $").


%% Returns an flattened list containing the ASCII representation of the given
%% term.
fprint(T, Max) -> 
    {L, _} = print(T, Max),
    lists:flatten(L).

%% Same as print, but never crashes. 
%%
%% This is a tradeoff. Print might conceivably crash if it's asked to
%% print something it doesn't understand, for example some new data
%% type in a future version of Erlang. If print crashes, we fall back
%% to io_lib to format the term, but then the formatting is
%% depth-limited instead of length limited, so you might run out
%% memory printing it. Out of the frying pan and into the fire.
%% 
safe(What, Len) ->
    case catch print(What, Len) of
	{L, Used} when is_list(L) -> {L, Used};
	_ -> {"unable to print" ++ io_lib:write(What, 99)}
    end.	     

%% Returns {List, Length}
print(_, Max) when Max < 0 -> {"...", 3};
print(Tuple, Max) when is_tuple(Tuple) ->
    {TC, Len} = tuple_contents(Tuple, Max-2),
    {[${, TC, $}], Len + 2};

%% We assume atoms, floats, funs, integers, PIDs, ports and refs never need 
%% to be truncated. This isn't strictly true, someone could make an 
%% arbitrarily long bignum. Let's assume that won't happen unless someone
%% is being malicious.
%%
print(Atom, _Max) when is_atom(Atom) ->
    L = atom_to_list(Atom),
    {L, length(L)};

print(<<>>, _Max) ->
    {"<<>>", 4};

print(Binary, Max) when is_binary(Binary) ->
    B = binary_to_list(Binary, 1, lists:min([Max, size(Binary)])),
    {L, Len} = alist_start(B, Max-4),
    {["<<", L, ">>"], Len};

print(Float, _Max) when is_float(Float) ->
    L = float_to_list(Float),
    {L, length(L)};

print(Fun, _Max) when is_function(Fun) ->
    L = erlang:fun_to_list(Fun),
    {L, length(L)};

print(Integer, _Max) when is_integer(Integer) ->
    L = integer_to_list(Integer),
    {L, length(L)};

print(Pid, _Max) when is_pid(Pid) ->
    L = pid_to_list(Pid),
    {L, length(L)};

print(Ref, _Max) when is_reference(Ref) ->
    L = erlang:ref_to_list(Ref),
    {L, length(L)};

print(Port, _Max) when is_port(Port) ->
    L = erlang:port_to_list(Port),
    {L, length(L)};

print(List, Max) when is_list(List) ->
    alist_start(List, Max).

%% Returns {List, Length}
tuple_contents(Tuple, Max) ->
    L = tuple_to_list(Tuple),
    list_body(L, Max).

%% Format the inside of a list, i.e. do not add a leading [ or trailing ].
%% Returns {List, Length}
list_body([], _) -> {[], 0};
list_body(_, Max) when Max < 4 -> {"...", 3};
list_body([H|T], Max) -> 
    {List, Len} = print(H, Max),
    {Final, FLen} = list_bodyc(T, Max - Len),
    {[List|Final], FLen + Len};
list_body(X, Max) ->  %% improper list
    {List, Len} = print(X, Max - 1),
    {[$|,List], Len + 1}.

list_bodyc([], _) -> {[], 0};
list_bodyc(_, Max) when Max < 4 -> {"...", 3};
list_bodyc([H|T], Max) -> 
    {List, Len} = print(H, Max),
    {Final, FLen} = list_bodyc(T, Max - Len - 1),
    {[$,, List|Final], FLen + Len + 1};
list_bodyc(X,Max) ->  %% improper list
    {List, Len} = print(X, Max - 1),
    {[$|,List], Len + 1}.

%% The head of a list we hope is ascii. Examples:
%%
%% [65,66,67] -> "ABC"
%% [65,0,67] -> "A"[0,67]
%% [0,65,66] -> [0,65,66]
%% [65,b,66] -> "A"[b,66]
%%
alist_start([], _) -> {"[]", 2};
alist_start(_, Max) when Max < 4 -> {"...", 3};
alist_start([H|T], Max) when H >= 16#20, H =< 16#7e ->  % definitely printable
    {L, Len} = alist([H|T], Max-1),
    {[$\"|L], Len + 1};
alist_start([H|T], Max) when H == 9; H == 10; H == 13 ->   % show as space
    {L, Len} = alist(T, Max-1),
    {[$ |L], Len + 1};
alist_start(L, Max) ->
    {R, Len} = list_body(L, Max-2),
    {[$[, R, $]], Len + 2}.

alist([], _) -> {"\"", 1};
alist(_, Max) when Max < 5 -> {"...\"", 4};
alist([H|T], Max) when H >= 16#20, H =< 16#7e ->     % definitely printable
    {L, Len} = alist(T, Max-1),
    {[H|L], Len + 1};
alist([H|T], Max) when H == 9; H == 10; H == 13 ->   % show as space
    {L, Len} = alist(T, Max-1),
    {[$ |L], Len + 1};
alist(L, Max) ->
    {R, Len} = list_body(L, Max-3),
    {[$\", $[, R, $]], Len + 3}.


%%--------------------
%% The start of a test suite. So far, it only checks for not crashing.
test() ->
    test(trunc_io, print).

test(Mod, Func) ->
    Simple_items = [atom, 1234, 1234.0, {tuple}, [], [list], "string", self(),
		    <<1,2,3>>, make_ref(), fun() -> ok end],
    F = fun(A) ->
		Mod:Func(A, 100),
		Mod:Func(A, 2),
		Mod:Func(A, 20)
	end,

    G = fun(A) ->
		case catch F(A) of
		    {'EXIT', _} -> exit({failed, A});
		    _ -> ok
		end
	end,
    
    lists:foreach(G, Simple_items),
    
    Tuples = [ {1,2,3,a,b,c}, {"abc", def, 1234},
	       {{{{a},b,c,{d},e}},f}],
    
    Lists = [ [1,2,3,4,5,6,7], lists:seq(1,1000),
	      [{a}, {a,b}, {a, [b,c]}, "def"], [a|b], [$a|$b] ],
    
    
    lists:foreach(G, Tuples),
    lists:foreach(G, Lists).
    
perf() ->
    {New, _} = timer:tc(trunc_io, perf, [trunc_io, print, 1000]),
    {Old, _} = timer:tc(trunc_io, perf, [io_lib, write, 1000]),
    io:fwrite("New code took ~p us, old code ~p\n", [New, Old]).

perf(M, F, Reps) when Reps > 0 ->
    test(M,F),
    perf(M,F,Reps-1);
perf(_,_,_) ->
    done.    

%% Performance test. Needs a particularly large term I saved as a binary...
perf1() ->
    {ok, Bin} = file:read_file("bin"),
    A = binary_to_term(Bin),
    {N, _} = timer:tc(trunc_io, print, [A, 1500]),
    {M, _} = timer:tc(io_lib, write, [A]),
    {N, M}.


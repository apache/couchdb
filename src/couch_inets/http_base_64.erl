%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
%% Description: Implements base 64 encode and decode, see RFC2045.
-module(http_base_64).
 
-export([encode/1, decode/1]).

-deprecated({'_', '_', next_major_release}).

%%%=========================================================================
%%%  API
%%%=========================================================================

%%-------------------------------------------------------------------------
%% encode(ASCII) -> Base64
%%	ASCII - string()  
%%	Base64 - string()
%%                                   
%% Description: Encodes a plain ASCII string into base64.
%%-------------------------------------------------------------------------
encode(ASCII) when is_list(ASCII) ->
    encode_base64_list(ASCII).


%%-------------------------------------------------------------------------
%% decode(Base64) -> ASCII
%%	Base64 - string() 
%%	ASCII - string()
%%                                    
%% Description: Decodes an base64 encoded string to plain ASCII. 
%%-------------------------------------------------------------------------
decode(Base64) when is_list(Base64) ->
    decode_base64_list(sixtets(Base64), []).

%%%========================================================================
%%% Internal functions
%%%========================================================================

%% Base-64 encoding: take 6 bits at a time from the head of the binary
%% and emit it as 8 bit characters.
encode_base64_list([]) ->
    [];
encode_base64_list([A]) ->
    [int_to_b64(A bsr 2), int_to_b64((A band 3) bsl 4), $=, $=];
encode_base64_list([A,B]) ->
    [int_to_b64(A bsr 2), int_to_b64(((A band 3) bsl 4) bor (B bsr 4)), 
     int_to_b64((B band 15) bsl 2), $=];
encode_base64_list([A,B,C|Ls]) ->
    encode_base64_list_do(A,B,C, Ls).

encode_base64_list_do(A,B,C, Rest) ->
    BB = (A bsl 16) bor (B bsl 8) bor C,
    [int_to_b64(BB bsr 18), int_to_b64((BB bsr 12) band 63), 
     int_to_b64((BB bsr 6) band 63), int_to_b64(BB band 63) |
     encode_base64_list(Rest)].

int_to_b64(X) when X >= 0, X =< 25 -> X + $A;
int_to_b64(X) when X >= 26, X =< 51 -> X - 26 + $a;
int_to_b64(X) when X >= 52, X =< 61 -> X - 52 + $0;
int_to_b64(62) -> $+;
int_to_b64(63) -> $/.

%% This version works by consuming groups of 4 input characters to create
%% a group of 3 output characters, with the three special-cases for
%% end-of-input first:
		      
decode_base64_list({[],[]}, Acc) ->
    lists:reverse(Acc);
decode_base64_list({[Sixtet1,Sixtet2,pad,pad], []}, Acc) ->
    Bits2x6 = (Sixtet1 bsl 18) bor (Sixtet2 bsl 12),
    Octet1 = Bits2x6 bsr 16,
    lists:reverse([Octet1 | Acc]);
decode_base64_list({[Sixtet1,Sixtet2,Sixtet3,pad], []}, Acc) ->
    Bits3x6 = (Sixtet1 bsl 18) bor (Sixtet2 bsl 12) bor (Sixtet3 bsl 6),
    Octet1 = Bits3x6 bsr 16,
    Octet2 = (Bits3x6 bsr 8) band 16#ff,
    lists:reverse([Octet2, Octet1 | Acc]);
decode_base64_list({[Sixtet1,Sixtet2,Sixtet3,Sixtet4],Rest}, Acc) when 
  Sixtet1 =/= pad,
  Sixtet2 =/= pad,
  Sixtet3 =/= pad,
  Sixtet4 =/= pad ->
    Bits4x6 =
	(Sixtet1 bsl 18) bor (Sixtet2 bsl 12) bor (Sixtet3 bsl 6) bor Sixtet4,
    Octet1 = Bits4x6 bsr 16,
    Octet2 = (Bits4x6 bsr 8) band 16#ff,
    Octet3 = Bits4x6 band 16#ff,
    decode_base64_list(sixtets(Rest), [Octet3, Octet2, Octet1 | Acc]).

b64_to_int(X) when X >= $A, X =< $Z -> X - $A;
b64_to_int(X) when X >= $a, X =< $z -> X - $a + 26;
b64_to_int(X) when X >= $0, X =< $9 -> X - $0 + 52;
b64_to_int($+) -> 62;
b64_to_int($/) -> 63;
b64_to_int($=) -> pad; % Padding will be removed by decode_base64_list/2
b64_to_int(_) -> ignore. % Not in base 64 should be ignored

sixtets(Str) ->
    sixtets(Str, []).

sixtets([], Sixtets) ->
    {lists:reverse(Sixtets), []};
sixtets(Rest, Sixtets) when length(Sixtets) == 4 ->
    {lists:reverse(Sixtets), Rest};
sixtets([Base64 | Tail], Sixtets) when length(Sixtets) < 4 ->
    case b64_to_int(Base64) of
	ignore ->
	    sixtets(Tail, Sixtets);
	Int ->
	    sixtets(Tail, [Int | Sixtets])
    end.

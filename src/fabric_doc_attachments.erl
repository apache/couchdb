% Copyright 2010 Cloudant
%
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

-module(fabric_doc_attachments).

-include("fabric.hrl").
-include_lib("couch/include/couch_db.hrl").

%% couch api calls
-export([receiver/2]).

receiver(_Req, undefined) ->
    <<"">>;
receiver(_Req, {unknown_transfer_encoding, Unknown}) ->
    exit({unknown_transfer_encoding, Unknown});
receiver(Req, chunked) ->
    MiddleMan = spawn(fun() -> middleman(Req, chunked) end),
    fun(4096, ChunkFun, ok) ->
        write_chunks(MiddleMan, ChunkFun)
    end;
receiver(_Req, 0) ->
    <<"">>;
receiver(Req, Length) when is_integer(Length) ->
    maybe_send_continue(Req),
    Middleman = spawn(fun() -> middleman(Req, Length) end),
    fun() ->
        Middleman ! {self(), gimme_data},
        receive
            {Middleman, Data} ->
                rexi:reply(attachment_chunk_received),
                iolist_to_binary(Data)
        after 600000 ->
            exit(timeout)
        end
    end;
receiver(_Req, Length) ->
    exit({length_not_integer, Length}).

%%
%% internal
%%

maybe_send_continue(#httpd{mochi_req = MochiReq} = Req) ->
    case couch_httpd:header_value(Req, "expect") of
    undefined ->
        ok;
    Expect ->
        case string:to_lower(Expect) of
        "100-continue" ->
            MochiReq:start_raw_response({100, gb_trees:empty()});
        _ ->
            ok
        end
    end.

write_chunks(MiddleMan, ChunkFun) ->
    MiddleMan ! {self(), gimme_data},
    receive
    {MiddleMan, ChunkRecordList} ->
        rexi:reply(attachment_chunk_received),
        case flush_chunks(ChunkRecordList, ChunkFun) of
        continue -> write_chunks(MiddleMan, ChunkFun);
        done -> ok
        end
    after 600000 ->
        exit(timeout)
    end.

flush_chunks([], _ChunkFun) ->
    continue;
flush_chunks([{0, _}], _ChunkFun) ->
    done;
flush_chunks([Chunk | Rest], ChunkFun) ->
    ChunkFun(Chunk, ok),
    flush_chunks(Rest, ChunkFun).

receive_unchunked_attachment(_Req, 0) ->
    ok;
receive_unchunked_attachment(Req, Length) ->
    receive {MiddleMan, go} ->
        Data = couch_httpd:recv(Req, 0),
        MiddleMan ! {self(), Data}
    end,
    receive_unchunked_attachment(Req, Length - size(Data)).

middleman(Req, chunked) ->
    % spawn a process to actually receive the uploaded data
    RcvFun = fun(ChunkRecord, ok) ->
        receive {From, go} -> From ! {self(), ChunkRecord} end, ok
    end,
    Receiver = spawn(fun() -> couch_httpd:recv_chunked(Req,4096,RcvFun,ok) end),

    % take requests from the DB writers and get data from the receiver
    N = erlang:list_to_integer(couch_config:get("cluster","n")),
    middleman_loop(Receiver, N, [], []);

middleman(Req, Length) ->
    Receiver = spawn(fun() -> receive_unchunked_attachment(Req, Length) end),
    N = erlang:list_to_integer(couch_config:get("cluster","n")),
    middleman_loop(Receiver, N, [], []).

middleman_loop(Receiver, N, Counters0, ChunkList0) ->
    receive {From, gimme_data} ->
        % Figure out how far along this writer (From) is in the list
        ListIndex = case fabric_dict:lookup_element(From, Counters0) of
        undefined -> 0;
        I -> I
        end,

        % Talk to the receiver to get another chunk if necessary
        ChunkList1 = if ListIndex == length(ChunkList0) ->
            Receiver ! {self(), go},
            receive
                {Receiver, ChunkRecord} ->
                    ChunkList0 ++ [ChunkRecord]
            end;
        true -> ChunkList0 end,

        % reply to the writer
        Reply = lists:nthtail(ListIndex, ChunkList1),
        From ! {self(), Reply},

        % Update the counter for this writer
        Counters1 = fabric_dict:update_counter(From, length(Reply), Counters0),

        % Drop any chunks that have been sent to all writers
        Size = fabric_dict:size(Counters1),
        NumToDrop = lists:min([I || {_, I} <- Counters1]),

        {ChunkList3, Counters3} =
        if Size == N andalso NumToDrop > 0 ->
            ChunkList2 = lists:nthtail(NumToDrop, ChunkList1),
            Counters2 = [{F, I-NumToDrop} || {F, I} <- Counters1],
            {ChunkList2, Counters2};
        true ->
            {ChunkList1, Counters1}
        end,

        middleman_loop(Receiver, N, Counters3, ChunkList3)
    after 10000 ->
        ok
    end.

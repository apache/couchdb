.. Licensed under the Apache License, Version 2.0 (the "License"); you may not
.. use this file except in compliance with the License. You may obtain a copy of
.. the License at
..
..   http://www.apache.org/licenses/LICENSE-2.0
..
.. Unless required by applicable law or agreed to in writing, software
.. distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
.. WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
.. License for the specific language governing permissions and limitations under
.. the License.

.. default-domain:: js

.. _query-server/erlang:

======
Erlang
======

.. note::
    The Erlang query server is disabled by default.
    Read :ref:`configuration guide <config/native_query_servers>` about
    reasons why and how to enable it.

.. function:: Emit(Id, Value)

    Emits `key`-`value` pairs to view indexer process.

    .. code-block:: erlang

        fun({Doc}) ->
            <<K,_/binary>> = proplists:get_value(<<"_rev">>, Doc, null),
            V = proplists:get_value(<<"_id">>, Doc, null),
            Emit(<<K>>, V)
        end.

.. function:: FoldRows(Fun, Acc)

    Helper to iterate over all rows in a list function.

    :param Fun: Function object.
    :param Acc: The value previously returned by `Fun`.

    .. code-block:: erlang

        fun(Head, {Req}) ->
            Fun = fun({Row}, Acc) ->
                Id = couch_util:get_value(<<"id">>, Row),
                Send(list_to_binary(io_lib:format("Previous doc id: ~p~n", [Acc]))),
                Send(list_to_binary(io_lib:format("Current  doc id: ~p~n", [Id]))),
                {ok, Id}
            end,
            FoldRows(Fun, nil),
            ""
        end.

.. function:: GetRow()

    Retrieves the next row from a related view result.

    .. code-block:: erlang

        %% FoldRows background implementation.
        %% https://git-wip-us.apache.org/repos/asf?p=couchdb.git;a=blob;f=src/couchdb/couch_native_process.erl;hb=HEAD#l368
        %%
        foldrows(GetRow, ProcRow, Acc) ->
            case GetRow() of
                nil ->
                    {ok, Acc};
                Row ->
                    case (catch ProcRow(Row, Acc)) of
                        {ok, Acc2} ->
                            foldrows(GetRow, ProcRow, Acc2);
                        {stop, Acc2} ->
                            {ok, Acc2}
                    end
        end.

.. function:: Log(Msg)

    :param Msg: Log a message at the `INFO` level.

    .. code-block:: erlang

        fun({Doc}) ->
            <<K,_/binary>> = proplists:get_value(<<"_rev">>, Doc, null),
            V = proplists:get_value(<<"_id">>, Doc, null),
            Log(lists:flatten(io_lib:format("Hello from ~s doc!", [V]))),
            Emit(<<K>>, V)
        end.

    After the map function has run, the following line can be found in
    CouchDB logs (e.g. at `/var/log/couchdb/couch.log`):

    .. code-block:: text

        [Sun, 04 Nov 2012 11:33:58 GMT] [info] [<0.9144.2>] Hello from 8d300b86622d67953d102165dbe99467 doc!

.. function:: Send(Chunk)

    Sends a single string `Chunk` in response.

    .. code-block:: erlang

        fun(Head, {Req}) ->
            Send("Hello,"),
            Send(" "),
            Send("Couch"),
            "!"
        end.

    The function above produces the following response:

    .. code-block:: text

        Hello, Couch!

.. function:: Start(Headers)

    :param Headers: Proplist of :ref:`response object<response_object>`.

    Initialize :ref:`listfun` response. At this point, response code and headers
    may be defined. For example, this function redirects to the CouchDB
    web site:

    .. code-block:: erlang

        fun(Head, {Req}) ->
            Start({[{<<"code">>, 302},
                    {<<"headers">>, {[
                        {<<"Location">>, <<"http://couchdb.apache.org">>}]
                    }}
                ]}),
            "Relax!"
        end.

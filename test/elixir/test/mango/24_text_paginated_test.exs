# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

defmodule PaginatedResultsTest do
  alias Couch.Test.Utils
  use CouchTestCase

  @db_name "paginated-results"
  # Great enough to make faster systems busy while running the
  # query.
  @num_docs 9_999
  @updates 25

  setup do
    MangoDatabase.recreate(@db_name)
    fields = [
      %{"name" => "_id", "type" => "string"},
      %{"name" => "name", "type" => "string"}
    ]
    MangoDatabase.create_text_index(
      @db_name,
      analyzer: "keyword",
      default_field: %{},
      selector: %{},
      fields: fields,
      index_array_lengths: true
    )

    docs =
      0..@num_docs
      |> Enum.map(fn doc_id ->
        %{
          "_id" => hex8(doc_id),
          "name" => Utils.random_name("db")
        }
      end)

    MangoDatabase.save_docs(@db_name, docs)
    :ok
  end

  defp hex8(i) do
    :io_lib.format("~8.16.0B", [i]) |> IO.iodata_to_binary()
  end

  defp do_query(delay, selector, opts \\ []) do
    Task.async(fn ->
      Process.sleep(delay)
      MangoDatabase.find(@db_name, selector, opts)
    end)
  end

  defp do_updates(pause, doc_id) do
    Task.async(fn ->
      for i <- 0..@updates do
        doc = MangoDatabase.open_doc(@db_name, doc_id)
        updated_doc = %{
          "_id" => doc_id,
          "_rev" => doc["_rev"],
          "update" => i,
          "name" => "foobar"
        }

        MangoDatabase.save_doc(@db_name, updated_doc)
        Process.sleep(pause)
      end
    end)
  end

  test "query with lot of results" do
    selector = %{"_id" => %{"$lte": hex8(1000)}}
    # 200 is the maximum for `text` searches.
    {:ok, docs} = MangoDatabase.find(@db_name, selector, limit: 200)
    assert length(docs) == 200
  end

  test "no duplicates on interleaved updates" do
    # Give ~500 ms head start for the updates before running the
    # query.
    query = do_query(500, %{"name" => "foobar"})
    # Keep updating the target document in every 200 ms.
    do_updates(200, hex8(2))

    {:ok, docs} = Task.await(query, :infinity)
    assert length(docs) == 1
  end

  test "no duplicates on interleaved updates heavy" do
    query = do_query(500, %{"name" => "foobar"})
    do_updates(50, hex8(2))
    do_updates(200, hex8(3))
    do_updates(300, hex8(4))
    do_updates(150, hex8(5))
    do_updates(100, hex8(6))

    {:ok, docs} = Task.await(query, :infinity)
    ids = Enum.sort(Enum.map(docs, fn doc -> doc["_id"] end))
    assert ids == [
      hex8(2),
      hex8(3),
      hex8(4),
      hex8(5),
      hex8(6)
    ]
  end

  test "no duplicates on interleaved updates with limit skip" do
    query = do_query(500, %{"name" => "foobar"}, limit: 1, skip: 3)
    do_updates(50, hex8(2))
    do_updates(200, hex8(3))
    do_updates(300, hex8(4))
    do_updates(150, hex8(5))
    do_updates(100, hex8(6))

    {:ok, docs} = Task.await(query, :infinity)
    assert length(docs) == 1
  end
end

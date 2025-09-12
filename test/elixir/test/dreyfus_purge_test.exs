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

defmodule DreyfusPurgeTest do
  use CouchTestCase

  @moduletag :search

  @doc_ids ["apple", "tomato", "cherry", "strawberry", "haw"]

  @search_ddoc %{
    _id: "_design/search",
    language: "javascript",
    indexes: %{
      index: %{
        analyzer: "standard",
        index: ~S"""
          function (doc) {
            index("default", doc._id);
            if(doc.color) {
              index("color", doc.color);
            }
            if(doc.size) {
              index("size", doc.size);
            }
          }
          """
      }
    }
  }

  @tag :with_db
  test "purge single document", context do
    db_name = context[:db_name]

    create_db_docs(db_name)
    wait_search(db_name, "apple", 1)

    purge_docs(db_name, ["apple"])
    wait_search(db_name, "apple", 0)
  end

  @tag :with_db
  test "purge multiple documents", context do
    db_name = context[:db_name]

    create_db_docs(db_name)
    wait_search(db_name, "color:red", 5)

    purge_docs(db_name, @doc_ids)
    wait_search(db_name, "color:red", 0)
  end

  @tag :with_db
  test "purge multiple documents in batches", context do
    db_name = context[:db_name]

    create_db_docs(db_name)
    wait_search(db_name, "color:red", 5)

    purge_docs(db_name, ["apple", "tomato"])
    wait_search(db_name, "color:red", 3)

    purge_docs(db_name, ["cherry", "haw"])
    wait_search(db_name, "color:red", 1)
  end

  @tag :with_db
  test "purge conflict", context do
    db_name_target = context[:db_name]
    {red_count1, green_count1} = setup_conflict(db_name_target)

    purge_docs(db_name_target, @doc_ids)

    # After purging winning revs, losers become winners (colors swap)
    retry_until(
      fn ->
        red_count2 = dreyfus_search(db_name_target, "color:red")
        green_count2 = dreyfus_search(db_name_target, "color:green")
        assert red_count2 + green_count2 == 5
        assert red_count1 == green_count2
        assert green_count1 == red_count2
      end,
      200,
      60_000
    )
  end

  @tag :with_db
  test "purge conflict twice removes all", context do
    db_name_target = context[:db_name]
    setup_conflict(db_name_target)

    purge_docs(db_name_target, @doc_ids)
    purge_docs(db_name_target, @doc_ids)

    wait_search(db_name_target, "color:red", 0)
    wait_search(db_name_target, "color:green", 0)
  end

  @tag :with_db
  test "purge conflict then purge remaining", context do
    db_name_target = context[:db_name]
    {red_count1, green_count1} = setup_conflict(db_name_target)

    purge_docs(db_name_target, @doc_ids)

    # after purging, losers become winners (colors swap)
    retry_until(
      fn ->
        red_count2 = dreyfus_search(db_name_target, "color:red")
        green_count2 = dreyfus_search(db_name_target, "color:green")
        assert red_count2 + green_count2 == 5
        assert red_count1 == green_count2
        assert green_count1 == red_count2
      end,
      200,
      60_000
    )

    purge_docs(db_name_target, @doc_ids)

    wait_search(db_name_target, "color:red", 0)
    wait_search(db_name_target, "color:green", 0)
  end

  @tag :with_db
  test "purge conflict with all revs", context do
    db_name_target = context[:db_name]
    setup_conflict(db_name_target, "green", "red")

    purge_docs_with_all_revs(db_name_target, @doc_ids)

    wait_search(db_name_target, "color:red", 0)
    wait_search(db_name_target, "color:green", 0)
  end

  @tag :with_db
  test "purge after update", context do
    db_name = context[:db_name]
    create_db_docs(db_name)

    wait_search(db_name, "color:red", 5)

    # update apple from red to green
    rev = get_rev(db_name, "apple")

    resp =
      Couch.put("/#{db_name}/apple",
        body: %{"_rev" => rev, "color" => "green", "size" => 8}
      )

    assert resp.status_code in [201, 202]

    retry_until(
      fn ->
        red_count = dreyfus_search(db_name, "color:red")
        green_count = dreyfus_search(db_name, "color:green")
        assert red_count == 4
        assert green_count == 1
      end,
      200,
      60_000
    )

    # purge 1 red and 1 green
    purge_docs(db_name, ["apple", "tomato"])

    retry_until(
      fn ->
        red_count = dreyfus_search(db_name, "color:red")
        green_count = dreyfus_search(db_name, "color:green")
        assert red_count == 3
        assert green_count == 0
      end,
      200,
      60_000
    )
  end

  @tag :with_db
  test "purge after many updates", context do
    db_name = context[:db_name]
    create_db_docs(db_name)

    wait_search(db_name, "size:1", 5)

    # update apple doc 999 times, final size = 1 + 999 = 1000
    Enum.reduce(1..999, get_rev(db_name, "apple"), fn i, rev ->
      resp =
        Couch.put("/#{db_name}/apple",
          body: %{"_rev" => rev, "size" => i + 1}
        )

      assert resp.status_code in [201, 202]
      resp.body["rev"]
    end)

    retry_until(
      fn ->
        count_1 = dreyfus_search(db_name, "size:1")
        count_1000 = dreyfus_search(db_name, "size:1000")
        assert count_1 == 4
        assert count_1000 == 1
      end,
      200,
      60_000
    )

    purge_docs(db_name, ["apple"])

    retry_until(
      fn ->
        count_1 = dreyfus_search(db_name, "size:1")
        count_1000 = dreyfus_search(db_name, "size:1000")
        assert count_1 == 4
        assert count_1000 == 0
      end,
      200,
      60_000
    )
  end

  @tag :with_db
  test "delete document", context do
    db_name = context[:db_name]

    create_db_docs(db_name)
    wait_search(db_name, "apple", 1)

    delete_docs(db_name, ["apple"])
    wait_search(db_name, "apple", 0)
  end

  @tag :with_db
  test "delete purge conflict", context do
    db_name_target = context[:db_name]
    setup_conflict(db_name_target)

    purge_docs(db_name_target, @doc_ids)
    delete_docs(db_name_target, @doc_ids)

    wait_search(db_name_target, "color:red", 0)
    wait_search(db_name_target, "color:green", 0)
  end

  @tag :with_db
  test "delete conflict", context do
    db_name_target = context[:db_name]
    {red_count1, green_count1} = setup_conflict(db_name_target)

    delete_docs(db_name_target, @doc_ids)

    # after deleting winning revs, losers become winner (colors swap)
    retry_until(
      fn ->
        red_count2 = dreyfus_search(db_name_target, "color:red")
        green_count2 = dreyfus_search(db_name_target, "color:green")
        assert red_count2 + green_count2 == 5
        assert red_count1 == green_count2
        assert green_count1 == red_count2
      end,
      200,
      60_000
    )
  end

  @tag :with_db
  test "purge then search", context do
    db_name = context[:db_name]
    create_db_docs(db_name)
    purge_docs(db_name, ["apple", "tomato", "haw"])
    wait_search(db_name, "color:red", 2)
  end

  @tag :with_db
  test "purge updates local checkpoint doc", context do
    db_name = context[:db_name]
    create_db_docs(db_name)
    wait_search(db_name, "apple", 1)

    # purge checkpoints exist before, purge_seq == 0
    checkpoints_before = get_checkpoints(db_name)
    assert checkpoints_before != []

    Enum.each(checkpoints_before, fn doc ->
      assert doc["type"] == "dreyfus"
      assert doc["ddoc_id"] == "_design/search"
      assert doc["indexname"] == "index"
      assert doc["purge_seq"] == 0
    end)

    purge_docs(db_name, ["apple", "tomato", "cherry", "strawberry"])
    wait_search(db_name, "apple", 0)

    # purge checkpoints updated after, purge_seq > 0
    checkpoints_after = get_checkpoints(db_name)
    assert length(checkpoints_after) == length(checkpoints_before)

    total_purge_seq =
      Enum.reduce(checkpoints_after, 0, fn doc, acc ->
        assert doc["type"] == "dreyfus"
        assert doc["ddoc_id"] == "_design/search"
        assert doc["purge_seq"] > 0
        acc + doc["purge_seq"]
      end)

    # assert total purge seq count
    assert total_purge_seq == 4
  end

  @tag :with_db
  test "delete design doc cleans up local purge doc", context do
    db_name = context[:db_name]
    create_db_docs(db_name)
    wait_search(db_name, "apple", 1)
    purge_docs(db_name, ["apple"])
    wait_search(db_name, "apple", 0)

    # purge checkpoints exist before
    assert get_checkpoints(db_name) != []

    delete_docs(db_name, ["_design/search"])
    resp = Couch.post("/#{db_name}/_search_cleanup")
    assert resp.status_code in [201, 202]

    # after cleanup checkpoints are gone
    retry_until(
      fn ->
        assert get_checkpoints(db_name) == []
      end,
      200,
      60_000
    )
  end

  defp create_db_docs(db_name, color \\ "red", opts \\ []) do
    docs =
      Enum.map(@doc_ids, fn id ->
        %{"_id" => id, "color" => color, "size" => 1}
      end)
    resp = Couch.post("/#{db_name}/_bulk_docs", body: %{docs: docs})
    assert resp.status_code in [201, 202]
    unless opts[:skip_ddoc] do
      resp = Couch.post("/#{db_name}", body: @search_ddoc)
      assert resp.status_code in [201, 202]
    end
  end

  defp get_checkpoints(db_name) do
    resp =
      Couch.get("/#{db_name}/_local_docs",
        query: %{include_docs: true, include_system: true}
      )
    assert resp.status_code == 200
    resp.body["rows"]
    |> Enum.filter(fn row ->
      String.starts_with?(row["id"], "_local/purge-dreyfus-")
    end)
    |> Enum.map(fn row -> row["doc"] end)
  end

  defp dreyfus_search(db_name, query) do
    url = "/#{db_name}/_design/search/_search/index"
    resp = Couch.get(url, query: %{q: query}, timeout: 60_000)
    assert resp.status_code == 200
    resp.body["total_rows"]
  end

  defp wait_search(db_name, query, expected_count) do
    retry_fun = fn -> assert expected_count == dreyfus_search(db_name, query) end
    retry_until(retry_fun, 200, 60_000)
  end

  defp get_rev(db_name, doc_id) do
    resp = Couch.get("/#{db_name}/#{doc_id}")
    assert resp.status_code == 200
    resp.body["_rev"]
  end

  defp get_revs(db_name, doc_id) do
    resp = Couch.get("/#{db_name}/#{doc_id}", query: %{conflicts: true, revs_info: true})
    assert resp.status_code == 200
    # we include the winning rev + the rest of the conflicts in the response
    case resp.body["_conflicts"] do
      nil -> [resp.body["_rev"]]
      conflicts -> [resp.body["_rev"] | conflicts]
    end
  end

  defp purge_docs(db_name, doc_ids) do
    purge_body =
      Enum.into(doc_ids, %{}, fn doc_id ->
        rev = get_rev(db_name, doc_id)
        {doc_id, [rev]}
      end)
    resp = Couch.post("/#{db_name}/_purge", body: purge_body)
    assert resp.status_code in [201, 202]
  end

  defp purge_docs_with_all_revs(db_name, doc_ids) do
    purge_body =
      Enum.into(doc_ids, %{}, fn doc_id ->
        revs = get_revs(db_name, doc_id)
        {doc_id, revs}
      end)
    resp = Couch.post("/#{db_name}/_purge", body: purge_body)
    assert resp.status_code in [201, 202]
  end

  defp delete_docs(db_name, doc_ids) do
    Enum.each(doc_ids, fn doc_id ->
      rev = get_rev(db_name, doc_id)
      resp = Couch.delete("/#{db_name}/#{doc_id}", query: %{rev: rev})
      assert resp.status_code in [200, 202]
    end)
  end

  # This is kind of a silly way to induce conflicts, but we are just copying it
  # from the erlang dreyfus_purge_test module. In the end return the red and
  # green count tuple.
  #
  defp setup_conflict(db_name_target, source_color \\ "red", target_color \\ "green") do
    db_name_source = random_db_name()
    create_db(db_name_source)
    on_exit(fn -> delete_db(db_name_source) end)

    create_db_docs(db_name_source, source_color, skip_ddoc: true)
    create_db_docs(db_name_target, target_color)

    retry_until(
      fn ->
        red = dreyfus_search(db_name_target, "color:red")
        green = dreyfus_search(db_name_target, "color:green")
        assert red + green == 5
      end,
      200,
      60_000
    )

    replicate(db_name_source, db_name_target)

    # Assert conflicts were created on each doc
    Enum.each(@doc_ids, fn doc_id ->
      revs = get_revs(db_name_target, doc_id)
      assert length(revs) == 2, "expected 2 revs (conflict) for #{doc_id}, got #{length(revs)}"
    end)

    # After replicating, search should still see 5 docs total
    retry_until(
      fn ->
        red = dreyfus_search(db_name_target, "color:red")
        green = dreyfus_search(db_name_target, "color:green")
        assert red + green == 5
      end,
      200,
      60_000
    )

    red_count = dreyfus_search(db_name_target, "color:red")
    green_count = dreyfus_search(db_name_target, "color:green")
    {red_count, green_count}
  end

end

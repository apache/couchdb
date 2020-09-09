defmodule AllDocsTest do
  use CouchTestCase

  @moduletag :all_docs
  @moduletag kind: :single_node

  @moduledoc """
  Test CouchDB _all_docs
  This is a port of the all_docs.js suite
  """

  # TODO: do we need to bring this in?
  # var db = new CouchDB(db_name, {"X-Couch-Full-Commit":"false"}, {w: 3});

  @tag :with_db
  test "All Docs tests", context do
    db_name = context[:db_name]
    resp1 = Couch.post("/#{db_name}", body: %{:_id => "0", :a => 1, :b => 1}).body
    resp2 = Couch.post("/#{db_name}", body: %{:_id => "3", :a => 4, :b => 16}).body
    resp3 = Couch.post("/#{db_name}", body: %{:_id => "1", :a => 2, :b => 4}).body
    resp4 = Couch.post("/#{db_name}", body: %{:_id => "2", :a => 3, :b => 9}).body

    assert resp1["ok"]
    assert resp2["ok"]
    assert resp3["ok"]
    assert resp4["ok"]

    revs = [resp1["rev"], resp2["rev"], resp3["rev"], resp4["rev"]]

    # Check _all_docs
    resp = Couch.get("/#{db_name}/_all_docs").body
    rows = resp["rows"]
    assert resp["total_rows"] == length(rows)

    Enum.each(rows, fn row ->
      assert row["id"] >= "0" && row["id"] <= "4"
    end)

    # Check _all_docs with descending=true
    resp = Couch.get("/#{db_name}/_all_docs", query: %{:descending => true}).body
    rows = resp["rows"]
    assert resp["total_rows"] == length(rows)

    # Check _all_docs offset
    resp = Couch.get("/#{db_name}/_all_docs", query: %{:startkey => "\"2\""}).body
    assert resp["offset"] == :null
    assert Enum.at(resp["rows"], 0)["key"] == "2"

    # Confirm that queries may assume raw collation
    resp =
      Couch.get(
        "/#{db_name}/_all_docs",
        query: %{
          :startkey => "\"org.couchdb.user:\"",
          :endkey => "\"org.couchdb.user;\""
        }
      )

    assert Enum.empty?(resp.body["rows"])

    # Check that all docs show up in the changes feed; order can vary
    resp = Couch.get("/#{db_name}/_changes").body

    Enum.each(resp["results"], fn row ->
      assert Enum.member?(revs, hd(row["changes"])["rev"]),
             "doc #{row["id"]} should be in changes"
    end)

    # Check that deletions also show up right
    doc1 = Couch.get("/#{db_name}/1").body
    assert Couch.delete("/#{db_name}/1", query: %{:rev => doc1["_rev"]}).body["ok"]
    changes = Couch.get("/#{db_name}/_changes").body["results"]
    assert length(changes) == 4

    deleted = Enum.filter(changes, fn row -> row["deleted"] end)
    assert length(deleted) == 1
    assert hd(deleted)["id"] == "1"

    # (remember old seq)
    orig_doc = Enum.find(changes, fn row -> row["id"] == "3" end)
    # Perform an update
    doc3 = Couch.get("/#{db_name}/3").body
    doc3 = Map.put(doc3, :updated, "totally")
    assert Couch.put("/#{db_name}/3", body: doc3).body["ok"]

    # The update should make doc id 3 have another seq num
    # (not nec. higher or the last though)
    changes = Couch.get("/#{db_name}/_changes").body["results"]
    assert length(changes) == 4
    updated_doc = Enum.find(changes, fn row -> row["id"] == "3" end)
    assert orig_doc["seq"] != updated_doc["seq"], "seq num should be different"

    # Ok, now let's see what happens with include docs
    changes =
      Couch.get("/#{db_name}/_changes", query: %{:include_docs => true}).body["results"]

    assert length(changes) == 4
    updated_doc = Enum.find(changes, fn row -> row["id"] == doc3["_id"] end)
    assert updated_doc["doc"]["updated"] == "totally"

    deleted_doc = Enum.find(changes, fn row -> row["deleted"] end)
    assert deleted_doc["doc"]["_deleted"]

    # Test _all_docs with keys
    rows =
      Couch.post(
        "/#{db_name}/_all_docs",
        query: %{:include_docs => true},
        body: %{:keys => ["1"]}
      ).body["rows"]

    row = hd(rows)
    assert length(rows) == 1
    assert row["key"] == "1"
    assert row["id"] == "1"
    assert row["value"]["deleted"]
    assert row["doc"] == :null

    # Add conflicts
    conflicted_doc1 = %{
      :_id => "3",
      :_rev => "2-aa01552213fafa022e6167113ed01087",
      :value => "X"
    }

    conflicted_doc2 = %{
      :_id => "3",
      :_rev => "2-ff01552213fafa022e6167113ed01087",
      :value => "Z"
    }

    assert Couch.put(
             "/#{db_name}/3",
             query: %{:new_edits => false},
             body: conflicted_doc1
           ).body["ok"]

    assert Couch.put(
             "/#{db_name}/3",
             query: %{:new_edits => false},
             body: conflicted_doc2
           ).body["ok"]

    win_rev = Couch.get("/#{db_name}/3").body

    changes =
      Couch.get(
        "/#{db_name}/_changes",
        query: %{:include_docs => true, :conflicts => true, :style => "all_docs"}
      ).body["results"]

    doc3 = Enum.find(changes, fn row -> row["id"] == "3" end)
    assert doc3["id"] == "3"
    assert length(doc3["changes"]) == 3
    assert win_rev["_rev"] == hd(doc3["changes"])["rev"]
    assert is_list(doc3["doc"]["_conflicts"])
    assert length(doc3["doc"]["_conflicts"]) == 2

    rows =
      Couch.get(
        "/#{db_name}/_all_docs",
        query: %{:include_docs => true, :conflicts => true}
      ).body["rows"]

    assert length(rows) == 3
    change = hd(tl(tl(rows)))
    assert change["key"] == "3"
    assert change["id"] == "3"
    assert change["value"]["rev"] == win_rev["_rev"]
    assert change["doc"]["_rev"] == win_rev["_rev"]
    assert change["doc"]["_id"] == "3"
    assert is_list(change["doc"]["_conflicts"])
    assert length(change["doc"]["_conflicts"]) == 2

    # Test that _all_docs collates sanely
    assert Couch.post("/#{db_name}", body: %{:_id => "Z", :foo => "Z"}).body["ok"]
    assert Couch.post("/#{db_name}", body: %{:_id => "a", :foo => "a"}).body["ok"]

    rows =
      Couch.get(
        "/#{db_name}/_all_docs",
        query: %{:startkey => "\"Z\"", :endkey => "\"Z\""}
      ).body["rows"]

    assert length(rows) == 1
  end

  @tag :with_db
  test "GET with one key", context do
    db_name = context[:db_name]

    {:ok, _} =
      create_doc(
        db_name,
        %{
          _id: "foo",
          bar: "baz"
        }
      )

    {:ok, _} =
      create_doc(
        db_name,
        %{
          _id: "foo2",
          bar: "baz2"
        }
      )

    resp =
      Couch.get(
        "/#{db_name}/_all_docs",
        query: %{
          :key => "\"foo\""
        }
      )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 1
  end

  @tag :with_db
  test "POST with empty body", context do
    db_name = context[:db_name]

    resp = Couch.post("/#{db_name}/_bulk_docs", body: %{docs: create_docs(0..2)})
    assert resp.status_code in [201, 202]

    resp =
      Couch.post(
        "/#{db_name}/_all_docs",
        body: %{}
      )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 3
  end

  @tag :with_db
  test "POST with missing keys", context do
    db_name = context[:db_name]

    resp = Couch.post("/#{db_name}/_bulk_docs", body: %{docs: create_docs(0..3)})
    assert resp.status_code in [201, 202]

    resp =
      Couch.post(
        "/#{db_name}/_all_docs",
        body: %{
          :keys => [1]
        }
      )

    assert resp.status_code == 200
    rows = resp.body["rows"]
    assert length(rows) == 1
    assert hd(rows) == %{"error" => "not_found", "key" => 1}
  end

  @tag :with_db
  test "POST with keys and limit", context do
    db_name = context[:db_name]

    resp = Couch.post("/#{db_name}/_bulk_docs", body: %{docs: create_docs(0..3)})
    assert resp.status_code in [201, 202]

    resp =
      Couch.post(
        "/#{db_name}/_all_docs",
        body: %{
          :keys => ["1", "2"],
          :limit => 1,
          :include_docs => true
        }
      )

    assert resp.status_code == 200
    rows = resp.body["rows"]
    assert length(rows) == 1
    doc = hd(rows)["doc"]
    assert doc["string"] == "1"
  end

  @tag :with_db
  test "_local_docs POST with keys and limit", context do
    expected = [
      %{
        "doc" => %{"_id" => "_local/one", "_rev" => "0-1", "value" => "one"},
        "id" => "_local/one",
        "key" => "_local/one",
        "value" => %{"rev" => "0-1"}
      },
      %{
        "doc" => %{"_id" => "_local/two", "_rev" => "0-1", "value" => "two"},
        "id" => "_local/two",
        "key" => "_local/two",
        "value" => %{"rev" => "0-1"}
      },
      %{
        "doc" => %{
          "_id" => "three",
          "_rev" => "1-878d3724976748bc881841046a276ceb",
          "value" => "three"
        },
        "id" => "three",
        "key" => "three",
        "value" => %{"rev" => "1-878d3724976748bc881841046a276ceb"}
      },
      %{"error" => "not_found", "key" => "missing"},
      %{"error" => "not_found", "key" => "_local/missing"}
    ]

    db_name = context[:db_name]

    docs = [
      %{
        _id: "_local/one",
        value: "one"
      },
      %{
        _id: "_local/two",
        value: "two"
      },
      %{
        _id: "three",
        value: "three"
      }
    ]

    resp = Couch.post("/#{db_name}/_bulk_docs", body: %{docs: docs})
    assert resp.status_code in [201, 202]

    resp =
      Couch.post(
        "/#{db_name}/_all_docs",
        body: %{
          :keys => ["_local/one", "_local/two", "three", "missing", "_local/missing"],
          :include_docs => true
        }
      )

    assert resp.status_code == 200
    rows = resp.body["rows"]
    assert length(rows) == 5
    assert rows == expected
  end

  @tag :with_db
  test "POST with query parameter and JSON body", context do
    db_name = context[:db_name]

    resp = Couch.post("/#{db_name}/_bulk_docs", body: %{docs: create_docs(0..3)})
    assert resp.status_code in [201, 202]

    resp =
      Couch.post(
        "/#{db_name}/_all_docs",
        query: %{
          :limit => 1
        },
        body: %{
          :keys => [1, 2]
        }
      )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 1
  end

  @tag :with_db
  test "POST edge case with colliding parameters - query takes precedence", context do
    db_name = context[:db_name]

    resp = Couch.post("/#{db_name}/_bulk_docs", body: %{docs: create_docs(0..3)})
    assert resp.status_code in [201, 202]

    resp =
      Couch.post(
        "/#{db_name}/_all_docs",
        query: %{
          :limit => 1
        },
        body: %{
          :keys => [1, 2],
          :limit => 2
        }
      )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 1
  end

  @tag :with_db
  test "all_docs ordering", context do
    db_name = context[:db_name]

    docs = [
      %{:_id => "a"},
      %{:_id => "m"},
      %{:_id => "z"}
    ]

    resp = Couch.post("/#{db_name}/_bulk_docs", body: %{:docs => docs})
    Enum.each(resp.body, &assert(&1["ok"]))

    resp = Couch.get("/#{db_name}/_all_docs", query: %{:startkey => false}).body
    rows = resp["rows"]
    assert length(rows) === 3
    assert get_ids(resp) == ["a", "m", "z"]

    resp = Couch.get("/#{db_name}/_all_docs", query: %{:startkey => 0}).body
    rows = resp["rows"]
    assert length(rows) === 3
    assert get_ids(resp) == ["a", "m", "z"]

    resp = Couch.get("/#{db_name}/_all_docs", query: %{:startkey => "[1,2]"}).body
    rows = resp["rows"]
    assert length(rows) === 3
    assert get_ids(resp) == ["a", "m", "z"]

    resp = Couch.get("/#{db_name}/_all_docs", query: %{:end_key => 0}).body
    rows = resp["rows"]
    assert length(rows) === 0
  end

  defp get_ids(resp) do
    %{"rows" => rows} = resp
    Enum.map(rows, fn row -> row["id"] end)
  end
end

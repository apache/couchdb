defmodule ViewCompactionTest do
  use CouchTestCase

  @moduledoc """
  Test CouchDB View Compaction Behavior
  This is a port of the view_compaction.js suite
  """
  
  @moduletag kind: :single_node
  
  @num_docs 1000

  @ddoc %{
    _id: "_design/foo",
    language: "javascript",
    views: %{
      view1: %{
        map: "function(doc) { emit(doc._id, doc.value) }"
      },
      view2: %{
        map:
          "function(doc) { if (typeof(doc.integer) === 'number') {emit(doc._id, doc.integer);} }",
        reduce: "function(keys, values, rereduce) { return sum(values); }"
      }
    }
  }

  defp bulk_save_for_update(db_name, docs) do
    resp = bulk_save(db_name, docs)
    revs = resp.body

    Enum.map(docs, fn m ->
      rev = Enum.at(revs, String.to_integer(m["_id"]))["rev"]

      m
      |> Map.put("_rev", rev)
      |> Map.update!("integer", &(&1 + 1))
    end)
  end

  @tag :with_db
  test "view compaction", context do
    db_name = context[:db_name]
    create_doc(db_name, @ddoc)

    docs = make_docs(0..(@num_docs - 1))
    docs = bulk_save_for_update(db_name, docs)

    resp = view(db_name, "foo/view1")
    assert length(resp.body["rows"]) == @num_docs

    resp = view(db_name, "foo/view2")
    assert length(resp.body["rows"]) == 1

    resp = Couch.get("/#{db_name}/_design/foo/_info")
    assert resp.body["view_index"]["update_seq"] == @num_docs + 1

    docs = bulk_save_for_update(db_name, docs)

    resp = view(db_name, "foo/view1")
    assert length(resp.body["rows"]) == @num_docs

    resp = view(db_name, "foo/view2")
    assert length(resp.body["rows"]) == 1

    resp = Couch.get("/#{db_name}/_design/foo/_info")
    assert resp.body["view_index"]["update_seq"] == 2 * @num_docs + 1

    bulk_save(db_name, docs)
    resp = view(db_name, "foo/view1")
    assert length(resp.body["rows"]) == @num_docs

    resp = view(db_name, "foo/view2")
    assert length(resp.body["rows"]) == 1

    resp = Couch.get("/#{db_name}/_design/foo/_info")
    assert resp.body["view_index"]["update_seq"] == 3 * @num_docs + 1

    disk_size_before_compact = resp.body["view_index"]["sizes"]["file"]
    data_size_before_compact = resp.body["view_index"]["sizes"]["active"]

    assert is_integer(disk_size_before_compact)
    assert data_size_before_compact < disk_size_before_compact

    resp = Couch.post("/#{db_name}/_compact/foo")
    assert resp.body["ok"] == true

    retry_until(fn ->
      resp = Couch.get("/#{db_name}/_design/foo/_info")
      resp.body["view_index"]["compact_running"] == false
    end)

    resp = view(db_name, "foo/view1")
    assert length(resp.body["rows"]) == @num_docs

    resp = view(db_name, "foo/view2")
    assert length(resp.body["rows"]) == 1

    resp = Couch.get("/#{db_name}/_design/foo/_info")
    assert resp.body["view_index"]["update_seq"] == 3 * @num_docs + 1

    disk_size_after_compact = resp.body["view_index"]["sizes"]["file"]
    data_size_after_compact = resp.body["view_index"]["sizes"]["active"]
    assert disk_size_after_compact < disk_size_before_compact
    assert is_integer(data_size_after_compact)
    assert data_size_after_compact < disk_size_after_compact
  end
end

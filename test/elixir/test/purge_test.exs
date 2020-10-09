defmodule PurgeTest do
  use CouchTestCase

  @moduletag :purge
  @moduletag kind: :single_node

  @tag :with_db
  test "purge documents", context do
    db_name = context[:db_name]

    design_doc = %{
      _id: "_design/test",
      language: "javascript",
      views: %{
        all_docs_twice: %{
          map: "function(doc) { emit(doc.integer, null); emit(doc.integer, null) }"
        },
        single_doc: %{
          map: "function(doc) { if (doc._id == \"1\") { emit(1, null) }}"
        }
      }
    }

    {:ok, _} = create_doc(db_name, design_doc)

    num_docs = 10
    bulk_save(db_name, make_docs(1..(num_docs + 1)))

    test_all_docs_twice(db_name, num_docs, 1)

    info = info(db_name)

    doc1 = open_doc(db_name, 1)
    doc2 = open_doc(db_name, 2)

    resp =
      Couch.post("/#{db_name}/_purge",
        body: %{"1": [doc1["_rev"]], "2": [doc2["_rev"]]}
      )

    assert resp.status_code == 201
    result = resp.body

    assert Enum.at(result["purged"]["1"], 0) == doc1["_rev"]
    assert Enum.at(result["purged"]["2"], 0) == doc2["_rev"]

    open_doc(db_name, 1, 404)
    open_doc(db_name, 2, 404)

    purged_info = info(db_name)

    assert purged_info["purge_seq"] != info["purge_seq"]

    test_all_docs_twice(db_name, num_docs, 0, 2)

    # purge sequences are preserved after compaction (COUCHDB-1021)
    compact(db_name)

    compacted_info = info(db_name)
    assert compacted_info["purge_seq"] == purged_info["purge_seq"]

    # purge documents twice in a row without loading views
    # (causes full view rebuilds)

    doc3 = open_doc(db_name, 3)
    doc4 = open_doc(db_name, 4)

    resp =
      Couch.post("/#{db_name}/_purge",
        body: %{"3": [doc3["_rev"]]}
      )

    assert resp.status_code == 201

    resp =
      Couch.post("/#{db_name}/_purge",
        body: %{"4": [doc4["_rev"]]}
      )

    assert resp.status_code == 201

    test_all_docs_twice(db_name, num_docs, 0, 4)
  end

  @tag :with_db
  test "COUCHDB-1065", context do
    db_name_a = context[:db_name]
    db_name_b = random_db_name()
    {:ok, _} = create_db(db_name_b)

    {:ok, doc_a_resp} = create_doc(db_name_a, %{_id: "test", a: 1})
    {:ok, doc_b_resp} = create_doc(db_name_b, %{_id: "test", a: 2})
    replicate(db_name_a, db_name_b)

    open_rev(db_name_b, "test", doc_a_resp.body["rev"], 200)
    open_rev(db_name_b, "test", doc_b_resp.body["rev"], 200)

    resp =
      Couch.post("/#{db_name_b}/_purge",
        body: %{test: [doc_a_resp.body["rev"]]}
      )

    assert resp.status_code == 201

    open_rev(db_name_b, "test", doc_a_resp.body["rev"], 404)

    resp =
      Couch.post("/#{db_name_b}/_purge",
        body: %{test: [doc_b_resp.body["rev"]]}
      )

    assert resp.status_code == 201

    open_rev(db_name_b, "test", doc_b_resp.body["rev"], 404)

    resp =
      Couch.post("/#{db_name_b}/_purge",
        body: %{test: [doc_a_resp.body["rev"], doc_b_resp.body["rev"]]}
      )

    assert resp.status_code == 201

    delete_db(db_name_b)
  end

  defp open_doc(db_name, id, expect \\ 200) do
    resp = Couch.get("/#{db_name}/#{id}")
    assert resp.status_code == expect
    resp.body
  end

  defp open_rev(db_name, id, rev, expect) do
    resp = Couch.get("/#{db_name}/#{id}?rev=#{rev}")
    assert resp.status_code == expect
    resp.body
  end

  defp test_all_docs_twice(db_name, num_docs, sigle_doc_expect, offset \\ 0) do
    resp = Couch.get("/#{db_name}/_design/test/_view/all_docs_twice")
    assert resp.status_code == 200
    rows = resp.body["rows"]

    for x <- 0..(num_docs - offset) do
      assert Map.get(Enum.at(rows, 2 * x), "key") == x + offset + 1
      assert Map.get(Enum.at(rows, 2 * x + 1), "key") == x + offset + 1
    end

    resp = Couch.get("/#{db_name}/_design/test/_view/single_doc")
    assert resp.body["total_rows"] == sigle_doc_expect
  end
end

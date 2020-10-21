defmodule ViewIncludeDocsTest do
  use CouchTestCase

  @moduletag kind: :single_node

  @ddoc %{
    _id: "_design/test",
    language: "javascript",
    views: %{
      all_docs: %{
        map: "function(doc) { emit(doc.integer, doc.string) }"
      },
      with_prev: %{
        map:
          "function(doc){if(doc.prev) emit(doc._id,{'_rev':doc.prev}); else emit(doc._id,{'_rev':doc._rev});}"
      },
      with_id: %{
        map:
          "function(doc) {if(doc.link_id) { var value = {'_id':doc.link_id}; if (doc.link_rev) {value._rev = doc.link_rev}; emit(doc._id, value);}};"
      },
      summate: %{
        map:
          "function (doc) { if (typeof doc.integer === 'number') {emit(doc.integer, doc.integer)};}",
        reduce: "function (keys, values) { return sum(values); };"
      }
    }
  }

  setup_all do
    db_name = random_db_name()
    {:ok, _} = create_db(db_name)
    on_exit(fn -> delete_db(db_name) end)

    bulk_save(db_name, make_docs(0..99))

    create_doc(db_name, @ddoc)

    {:ok, [db_name: db_name]}
  end

  test "include docs in view", context do
    db_name = context[:db_name]
    resp = view(db_name, "test/all_docs", %{include_docs: true, limit: 2})
    assert length(resp.body["rows"]) == 2
    row0 = Enum.at(resp.body["rows"], 0)
    assert row0["id"] == "0"
    assert row0["doc"]["_id"] == "0"
    row1 = Enum.at(resp.body["rows"], 1)
    assert row1["id"] == "1"
    assert row1["doc"]["_id"] == "1"

    resp = view(db_name, "test/all_docs", %{include_docs: true}, [29, 74])
    assert length(resp.body["rows"]) == 2
    row0 = Enum.at(resp.body["rows"], 0)
    assert row0["doc"]["_id"] == "29"
    row1 = Enum.at(resp.body["rows"], 1)
    assert row1["doc"]["integer"] == 74
  end

  test "include docs in all_docs", context do
    db_name = context[:db_name]

    resp =
      Couch.get("/#{db_name}/_all_docs",
        query: [limit: 2, skip: 1, include_docs: true]
      )

    assert length(resp.body["rows"]) == 2
    row0 = Enum.at(resp.body["rows"], 0)
    row1 = Enum.at(resp.body["rows"], 1)
    assert row0["doc"]["integer"] == 1
    assert row1["doc"]["integer"] == 10

    resp =
      Couch.post("/#{db_name}/_all_docs",
        query: [include_docs: true],
        headers: ["Content-Type": "application/json"],
        body: %{"keys" => ["not_a_doc"]}
      )

    assert length(resp.body["rows"]) == 1
    row0 = Enum.at(resp.body["rows"], 0)
    assert not Map.has_key?(row0, "doc")

    resp =
      Couch.post("/#{db_name}/_all_docs",
        query: [include_docs: true],
        headers: ["Content-Type": "application/json"],
        body: %{"keys" => ["1", "foo"]}
      )

    assert length(resp.body["rows"]) == 2
    row0 = Enum.at(resp.body["rows"], 0)
    row1 = Enum.at(resp.body["rows"], 1)
    assert row0["doc"]["integer"] == 1
    assert not Map.has_key?(row1, "doc")

    resp =
      Couch.get("/#{db_name}/_all_docs",
        query: [limit: 0, include_docs: true]
      )

    assert Enum.empty?(resp.body["rows"])
  end

  test "no reduce support", context do
    db_name = context[:db_name]

    resp =
      Couch.get("/#{db_name}/_design/test/_view/summate", query: [include_docs: true])

    assert resp.status_code == 400
    assert resp.body["error"] == "query_parse_error"
  end

  test "Reduce support when reduce=false", context do
    db_name = context[:db_name]

    resp =
      Couch.get("/#{db_name}/_design/test/_view/summate",
        query: [reduce: false, include_docs: true]
      )

    assert length(resp.body["rows"]) == 100
  end

  test "Not an error with include_docs=false&reduce=true", context do
    db_name = context[:db_name]

    resp =
      Couch.get("/#{db_name}/_design/test/_view/summate",
        query: [reduce: true, include_docs: false]
      )

    assert length(resp.body["rows"]) == 1
    row0 = Enum.at(resp.body["rows"], 0)
    assert row0["value"] == 4950
  end

  @tag :with_db
  test "link to another doc from a value", context do
    db_name = context[:db_name]

    bulk_save(db_name, make_docs(0..99))
    create_doc(db_name, @ddoc)

    doc_link = %{
      _id: "link-to-10",
      link_id: "10"
    }

    {:ok, _} = create_doc(db_name, doc_link)
    resp = view(db_name, "test/with_id", %{key: ~s("link-to-10")})
    assert length(resp.body["rows"]) == 1
    row0 = Enum.at(resp.body["rows"], 0)
    assert row0["key"] == "link-to-10"
    assert row0["value"]["_id"] == "10"

    resp = view(db_name, "test/with_id", %{key: ~s("link-to-10"), include_docs: true})
    assert length(resp.body["rows"]) == 1
    row0 = Enum.at(resp.body["rows"], 0)
    assert row0["value"]["_id"] == "10"
    assert row0["doc"]["_id"] == "10"
  end

  @tag :with_db
  test "emitted _rev controls things", context do
    db_name = context[:db_name]

    bulk_save(db_name, make_docs(0..99))
    create_doc(db_name, @ddoc)

    resp =
      Couch.post("/#{db_name}/_all_docs",
        query: [include_docs: true],
        headers: ["Content-Type": "application/json"],
        body: %{"keys" => ["0"]}
      )

    doc_before = Enum.at(resp.body["rows"], 0)["doc"]

    resp = Couch.get("/#{db_name}/0")
    assert resp.status_code == 200
    prev = resp.body["_rev"]

    doc_after =
      resp.body
      |> Map.put("integer", 100)
      |> Map.put("prev", prev)

    saved_doc = save(db_name, doc_after)

    resp = Couch.get("/#{db_name}/0")
    assert resp.status_code == 200
    doc_after = resp.body
    assert doc_after["_rev"] == saved_doc["_rev"]
    assert doc_after["_rev"] != doc_after["prev"]
    assert doc_after["integer"] == 100

    resp = view(db_name, "test/with_prev", %{include_docs: true}, ["0"])
    row0 = Enum.at(resp.body["rows"], 0)["doc"]
    assert row0["_id"] == "0"
    assert row0["_rev"] == doc_before["_rev"]
    assert not Map.has_key?(row0, "prev")
    assert assert row0["integer"] == 0
  end

  test "COUCHDB-549 - include_docs=true with conflicts=true" do
    db_name_a = random_db_name()
    db_name_b = random_db_name()
    create_db(db_name_a)
    create_db(db_name_b)
    on_exit(fn -> delete_db(db_name_a) end)
    on_exit(fn -> delete_db(db_name_b) end)

    ddoc = %{
      _id: "_design/mydesign",
      language: "javascript",
      views: %{
        myview: %{
          map: """
           function(doc) {
            emit(doc.value, 1);
          }
          """
        }
      }
    }

    {:ok, _} = create_doc(db_name_a, ddoc)

    doc1a = %{_id: "foo", value: 1, str: "1"}
    {:ok, _} = create_doc(db_name_a, doc1a)

    doc1b = %{_id: "foo", value: 1, str: "666"}
    {:ok, _} = create_doc(db_name_b, doc1b)

    doc2 = %{_id: "bar", value: 2, str: "2"}
    {:ok, _} = create_doc(db_name_a, doc2)

    replicate(db_name_a, db_name_b)

    resp = Couch.get("/#{db_name_b}/foo", query: [conflicts: true])
    assert resp.status_code == 200
    doc1b = resp.body
    assert Map.has_key?(doc1b, "_conflicts")
    assert length(doc1b["_conflicts"]) == 1
    conflict_rev = Enum.at(doc1b["_conflicts"], 0)

    resp = Couch.get("/#{db_name_b}/bar", query: [conflicts: true])
    assert resp.status_code == 200
    doc2 = resp.body
    assert not Map.has_key?(doc2, "_conflicts")

    resp = view(db_name_b, "mydesign/myview", %{include_docs: true, conflicts: true})
    assert length(resp.body["rows"]) == 2
    row0 = Enum.at(resp.body["rows"], 0)["doc"]
    assert length(row0["_conflicts"]) == 1
    assert Enum.at(row0["_conflicts"], 0) == conflict_rev
    row1 = Enum.at(resp.body["rows"], 1)["doc"]
    assert not Map.has_key?(row1, "_conflicts")
  end
end

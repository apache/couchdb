defmodule ViewMapTest do
  use CouchTestCase

  @moduledoc """
  Test Map functionality for views
  """
  def get_ids(resp) do
    %{:body => %{"rows" => rows}} = resp
    Enum.map(rows, fn row -> row["id"] end)
  end

  def get_keys(resp) do
    %{:body => %{"rows" => rows}} = resp
    Enum.map(rows, fn row -> row["key"] end)
  end

  defp create_map_docs(db_name) do
    docs =
      for i <- 1..10 do
        group =
          if rem(i, 3) == 0 do
            "one"
          else
            "two"
          end

        %{
          :_id => "doc-id-#{i}",
          :value => i,
          :some => "field",
          :group => group
        }
      end

    resp = Couch.post("/#{db_name}/_bulk_docs", body: %{:docs => docs, :w => 3})
    assert resp.status_code == 201
  end

  setup do
    db_name = random_db_name()
    {:ok, _} = create_db(db_name)
    on_exit(fn -> delete_db(db_name) end)

    create_map_docs(db_name)

    map_fun1 = """
      function(doc) {
        if (doc.some) {
            emit(doc.value , doc.value);
        }

        if (doc._id.indexOf("_design") > -1) {
            emit(0, "ddoc")
        }
      }
    """

    map_fun2 = """
      function(doc) {
        if (doc.group) {
          emit([doc.some, doc.group], 1);
        }
      }
    """

    map_fun3 = """
      function(doc) {
        if (doc.group) {
            emit(doc.group, 1);
        }
      }
    """

    body = %{
      :w => 3,
      :docs => [
        %{
          _id: "_design/map",
          views: %{
            some: %{map: map_fun1},
            map_some: %{map: map_fun2},
            map_group: %{map: map_fun3}
          }
        },
        %{
          _id: "_design/include_ddocs",
          views: %{some: %{map: map_fun1}},
          options: %{include_design: true}
        }
      ]
    }

    resp = Couch.post("/#{db_name}/_bulk_docs", body: body)
    Enum.each(resp.body, &assert(&1["ok"]))

    {:ok, [db_name: db_name]}
  end

  def get_reduce_result(resp) do
    %{:body => %{"rows" => rows}} = resp
    rows
  end

  test "query returns docs", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_design/map/_view/some"
    resp = Couch.get(url)
    assert resp.status_code == 200

    ids = get_ids(resp)

    assert ids == [
             "doc-id-1",
             "doc-id-2",
             "doc-id-3",
             "doc-id-4",
             "doc-id-5",
             "doc-id-6",
             "doc-id-7",
             "doc-id-8",
             "doc-id-9",
             "doc-id-10"
           ]

    url = "/#{db_name}/_design/map/_view/map_some"
    resp = Couch.get(url)
    assert resp.status_code == 200

    ids = get_ids(resp)

    assert ids == [
             "doc-id-3",
             "doc-id-6",
             "doc-id-9",
             "doc-id-1",
             "doc-id-10",
             "doc-id-2",
             "doc-id-4",
             "doc-id-5",
             "doc-id-7",
             "doc-id-8"
           ]
  end

  test "updated docs rebuilds index", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_design/map/_view/some"
    resp = Couch.get(url)
    assert resp.status_code == 200
    ids = get_ids(resp)

    assert ids == [
             "doc-id-1",
             "doc-id-2",
             "doc-id-3",
             "doc-id-4",
             "doc-id-5",
             "doc-id-6",
             "doc-id-7",
             "doc-id-8",
             "doc-id-9",
             "doc-id-10"
           ]

    update_doc_value(db_name, "doc-id-5", 0)
    update_doc_value(db_name, "doc-id-6", 100)

    resp = Couch.get("/#{db_name}/doc-id-3")
    doc3 = convert(resp.body)
    resp = Couch.delete("/#{db_name}/#{doc3["_id"]}", query: %{rev: doc3["_rev"]})
    assert resp.status_code == 200
    #
    resp = Couch.get("/#{db_name}/doc-id-4")
    doc4 = convert(resp.body)
    doc4 = Map.delete(doc4, "some")
    resp = Couch.put("/#{db_name}/#{doc4["_id"]}", body: doc4)
    assert resp.status_code == 201
    #
    resp = Couch.get("/#{db_name}/doc-id-1")
    doc1 = convert(resp.body)
    doc1 = Map.put(doc1, "another", "value")
    resp = Couch.put("/#{db_name}/#{doc1["_id"]}", body: doc1)
    assert resp.status_code == 201

    url = "/#{db_name}/_design/map/_view/some"
    resp = Couch.get(url)
    assert resp.status_code == 200
    ids = get_ids(resp)

    assert ids == [
             "doc-id-5",
             "doc-id-1",
             "doc-id-2",
             "doc-id-7",
             "doc-id-8",
             "doc-id-9",
             "doc-id-10",
             "doc-id-6"
           ]
  end

  test "can index design docs", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_design/include_ddocs/_view/some"
    resp = Couch.get(url, query: %{limit: 3})
    assert resp.status_code == 200
    ids = get_ids(resp)

    assert ids == ["_design/include_ddocs", "_design/map", "doc-id-1"]
  end

  test "can use key in query string", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_design/map/_view/map_group"
    resp = Couch.get(url, query: %{limit: 3, key: "\"one\""})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert ids == ["doc-id-3", "doc-id-6", "doc-id-9"]

    resp =
      Couch.get(url,
        query: %{
          limit: 3,
          key: "\"one\"",
          descending: true
        }
      )

    assert resp.status_code == 200
    ids = get_ids(resp)
    assert ids == ["doc-id-9", "doc-id-6", "doc-id-3"]
  end

  test "can use keys in query string", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_design/map/_view/some"
    resp = Couch.post(url, body: %{keys: [6, 3, 9]})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert ids == ["doc-id-6", "doc-id-3", "doc-id-9"]

    # should ignore descending = true
    resp = Couch.post(url, body: %{keys: [6, 3, 9], descending: true})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert ids == ["doc-id-6", "doc-id-3", "doc-id-9"]
  end

  test "inclusive = false", context do
    db_name = context[:db_name]

    docs = [
      %{key: "key1"},
      %{key: "key2"},
      %{key: "key3"},
      %{key: "key4"},
      %{key: "key4"},
      %{key: "key5"},
      %{
        _id: "_design/inclusive",
        views: %{
          by_key: %{
            map: """
                function (doc) {
                    if (doc.key) {
                        emit(doc.key, doc);
                    }
                }
            """
          }
        }
      }
    ]

    resp = Couch.post("/#{db_name}/_bulk_docs", body: %{:docs => docs, :w => 3})
    assert resp.status_code == 201
    url = "/#{db_name}/_design/inclusive/_view/by_key"

    query = %{
      endkey: "\"key4\"",
      inclusive_end: false
    }

    resp = Couch.get(url, query: query)
    assert resp.status_code == 200
    keys = get_keys(resp)
    assert keys == ["key1", "key2", "key3"]

    query = %{
      startkey: "\"key3\"",
      endkey: "\"key4\"",
      inclusive_end: false
    }

    resp = Couch.get(url, query: query)
    assert resp.status_code == 200
    keys = get_keys(resp)
    assert keys == ["key3"]

    query = %{
      startkey: "\"key4\"",
      endkey: "\"key1\"",
      inclusive_end: false,
      descending: true
    }

    resp = Couch.get(url, query: query)
    assert resp.status_code == 200
    keys = get_keys(resp)
    assert keys == ["key4", "key4", "key3", "key2"]
  end

  test "supports linked documents", context do
    db_name = context[:db_name]

    docs = [
      %{_id: "mydoc", foo: "bar"},
      %{_id: "join-doc", doc_id: "mydoc"},
      %{
        _id: "_design/join",
        views: %{
          by_doc_id: %{
            map: """
                function (doc) {
                    if (doc.doc_id) {
                        emit(doc._id, {_id: doc.doc_id});
                    }
                }
            """
          }
        }
      }
    ]

    resp = Couch.post("/#{db_name}/_bulk_docs", body: %{:docs => docs, :w => 3})
    assert resp.status_code == 201

    url = "/#{db_name}/_design/join/_view/by_doc_id"
    resp = Couch.get(url)
    assert resp.status_code == 200
    %{:body => %{"rows" => [row]}} = resp

    assert row == %{
             "id" => "join-doc",
             "key" => "join-doc",
             "value" => %{"_id" => "mydoc"}
           }

    url = "/#{db_name}/_design/join/_view/by_doc_id"
    resp = Couch.get(url, query: %{include_docs: true})
    assert resp.status_code == 200
    %{:body => %{"rows" => [doc]}} = resp

    assert doc["id"] == "join-doc"
    assert doc["doc"]["_id"] == "mydoc"
  end

  test "bad range returns error", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_design/map/_view/some"
    resp = Couch.get(url, query: %{startkey: "5", endkey: "4"})
    assert resp.status_code == 400
    %{:body => %{"error" => error}} = resp
    assert error == "query_parse_error"
  end

  test "multiple emits in correct value order", context do
    db_name = context[:db_name]

    docs = [
      %{_id: "doc1", foo: "foo", bar: "bar"},
      %{_id: "doc2", foo: "foo", bar: "bar"},
      %{
        _id: "_design/emit",
        views: %{
          multiple_emit: %{
            map: """
                function (doc) {
                  if (!doc.foo) {
                    return;
                  }
                  emit(doc.foo);
                  emit(doc.bar);
                  emit(doc.foo);
                  emit(doc.bar, 'multiple values!');
                  emit(doc.bar, 'crayon!');
                }
            """
          }
        }
      }
    ]

    resp = Couch.post("/#{db_name}/_bulk_docs", body: %{:docs => docs, :w => 3})
    assert resp.status_code == 201

    url = "/#{db_name}/_design/emit/_view/multiple_emit"
    resp = Couch.post(url, body: %{keys: ["foo", "bar"]})
    assert resp.status_code == 200
    %{:body => %{"rows" => rows}} = resp

    assert Enum.at(rows, 0)["key"] == "foo"
    assert Enum.at(rows, 0)["id"] == "doc1"
    assert Enum.at(rows, 1)["key"] == "foo"
    assert Enum.at(rows, 1)["id"] == "doc1"

    assert Enum.at(rows, 2)["key"] == "foo"
    assert Enum.at(rows, 2)["id"] == "doc2"
    assert Enum.at(rows, 3)["key"] == "foo"
    assert Enum.at(rows, 3)["id"] == "doc2"

    assert Enum.at(rows, 4)["key"] == "bar"
    assert Enum.at(rows, 4)["id"] == "doc1"
    assert Enum.at(rows, 4)["value"] == :null
    assert Enum.at(rows, 5)["key"] == "bar"
    assert Enum.at(rows, 5)["id"] == "doc1"
    assert Enum.at(rows, 5)["value"] == "crayon!"
    assert Enum.at(rows, 6)["key"] == "bar"
    assert Enum.at(rows, 6)["id"] == "doc1"
    assert Enum.at(rows, 6)["value"] == "multiple values!"

    assert Enum.at(rows, 7)["key"] == "bar"
    assert Enum.at(rows, 7)["id"] == "doc2"
    assert Enum.at(rows, 7)["value"] == :null
    assert Enum.at(rows, 8)["key"] == "bar"
    assert Enum.at(rows, 8)["id"] == "doc2"
    assert Enum.at(rows, 8)["value"] == "crayon!"
    assert Enum.at(rows, 9)["key"] == "bar"
    assert Enum.at(rows, 9)["id"] == "doc2"
    assert Enum.at(rows, 9)["value"] == "multiple values!"
  end

  test "can do design doc swap", context do
    db_name = context[:db_name]

    docs = [
      %{_id: "doc1", foo: "foo", bar: "bar"},
      %{
        _id: "_design/view1",
        views: %{
          view: %{
            map: """
                function (doc) {
                  if (!doc.foo) {
                    return;
                  }
                  emit(doc.foo);
                }
            """
          }
        }
      },
      %{
        _id: "_design/view2",
        views: %{
          view: %{
            map: """
                function (doc) {
                  if (!doc.bar) {
                    return;
                  }
                  emit(doc.bar);
                }
            """
          }
        }
      }
    ]

    resp = Couch.post("/#{db_name}/_bulk_docs", body: %{:docs => docs})
    assert resp.status_code == 201

    url1 = "/#{db_name}/_design/view1/_view/view"
    url2 = "/#{db_name}/_design/view2/_view/view"

    resp = Couch.get(url1)
    assert resp.status_code == 200
    keys = get_keys(resp)
    assert keys == ["foo"]

    resp = Couch.get(url2)
    assert resp.status_code == 200
    keys = get_keys(resp)
    assert keys == ["bar"]

    view1 = Couch.get("/#{db_name}/_design/view1")
    view2 = Couch.get("/#{db_name}/_design/view2")

    new_view1 = Map.replace!(view1.body, "views", view2.body["views"])

    resp = Couch.put("/#{db_name}/_design/view1", body: new_view1)
    assert resp.status_code in [201, 202]

    resp = Couch.get(url1, query: %{update: false})
    assert resp.status_code == 200
    keys = get_keys(resp)
    assert keys == ["bar"]
  end

  test "send error for failed indexing", context do
    db_name = context[:db_name]

    docs = [
      %{_id: "doc1", foo: "foo", bar: "bar"},
      %{
        _id: "_design/view1",
        views: %{
          view: %{
            map: """
                function (doc) {
                  for (var i=0; i<10000; i++) {
                  emit({doc: doc._id + 1}, doc._id);
                }
              }
            """
          }
        }
      }
    ]

    resp = Couch.post("/#{db_name}/_bulk_docs", body: %{:docs => docs})
    assert resp.status_code == 201

    url = "/#{db_name}/_design/view1/_view/view"

    resp = Couch.get(url, timeout: 500_000)
    assert resp.status_code == 500
    %{:body => %{"error" => error}} = resp
    assert error == "foundationdb_error"
  end

  def update_doc_value(db_name, id, value) do
    resp = Couch.get("/#{db_name}/#{id}")
    doc = convert(resp.body)
    doc = Map.put(doc, "value", value)
    resp = Couch.put("/#{db_name}/#{id}", body: doc)
    assert resp.status_code == 201
  end

  def convert(value) do
    :jiffy.decode(:jiffy.encode(value), [:return_maps])
  end
end

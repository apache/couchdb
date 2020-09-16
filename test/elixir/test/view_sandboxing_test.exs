defmodule ViewSandboxingTest do
  use CouchTestCase

  @moduletag kind: :single_node

  @document %{integer: 1, string: "1", array: [1, 2, 3]}

  @tag :with_db
  test "attempting to change the document has no effect", context do
    db_name = context[:db_name]

    {:ok, _} = create_doc(db_name, @document)

    map_fun = """
    function(doc) {
      doc.integer = 2;
      emit(null, doc);
    }
    """

    resp = query(db_name, map_fun, nil, %{include_docs: true})
    rows = resp["rows"]
    # either we have an error or our doc is unchanged
    assert resp["total_rows"] == 0 or Enum.at(rows, 0)["doc"]["integer"] == 1

    map_fun = """
    function(doc) {
      doc.array[0] = 0;
      emit(null, doc);
    }
    """

    resp = query(db_name, map_fun, nil, %{include_docs: true})
    row = Enum.at(resp["rows"], 0)
    # either we have an error or our doc is unchanged
    assert resp["total_rows"] == 0 or Enum.at(row["doc"]["array"], 0) == 1
  end

  @tag :with_db
  test "view cannot invoke interpreter internals", context do
    db_name = context[:db_name]
    {:ok, _} = create_doc(db_name, @document)

    map_fun = """
    function(doc) {
      gc();
      emit(null, doc);
    }
    """

    # make sure that a view cannot invoke interpreter internals such as the
    # garbage collector
    resp = query(db_name, map_fun)
    assert resp["total_rows"] == 0
  end

  @tag :with_db
  test "view cannot access the map_funs and map_results array", context do
    db_name = context[:db_name]
    {:ok, _} = create_doc(db_name, @document)

    map_fun = """
    function(doc) {
      map_funs.push(1);
      emit(null, doc);
    }
    """

    resp = query(db_name, map_fun)
    assert resp["total_rows"] == 0

    map_fun = """
    function(doc) {
      map_results.push(1);
      emit(null, doc);
    }
    """

    resp = query(db_name, map_fun)
    assert resp["total_rows"] == 0
  end

  @tag :with_db
  test "COUCHDB-925 - altering 'doc' variable in map function affects other map functions",
       context do
    db_name = context[:db_name]

    ddoc = %{
      _id: "_design/foobar",
      language: "javascript",
      views: %{
        view1: %{
          map: """
             function(doc) {
              if (doc.values) {
                doc.values = [666];
              }
              if (doc.tags) {
                doc.tags.push("qwerty");
              }
              if (doc.tokens) {
                doc.tokens["c"] = 3;
              }
            }
          """
        },
        view2: %{
          map: """
          function(doc) {
            if (doc.values) {
              emit(doc._id, doc.values);
            }
            if (doc.tags) {
              emit(doc._id, doc.tags);
            }
            if (doc.tokens) {
              emit(doc._id, doc.tokens);
            }
          }
          """
        }
      }
    }

    doc1 = %{
      _id: "doc1",
      values: [1, 2, 3]
    }

    doc2 = %{
      _id: "doc2",
      tags: ["foo", "bar"],
      tokens: %{a: 1, b: 2}
    }

    {:ok, _} = create_doc(db_name, ddoc)
    {:ok, _} = create_doc(db_name, doc1)
    {:ok, _} = create_doc(db_name, doc2)

    resp1 = view(db_name, "foobar/view1")
    resp2 = view(db_name, "foobar/view2")

    assert Enum.empty?(resp1.body["rows"])
    assert length(resp2.body["rows"]) == 3

    assert doc1[:_id] == Enum.at(resp2.body["rows"], 0)["key"]
    assert doc2[:_id] == Enum.at(resp2.body["rows"], 1)["key"]
    assert doc2[:_id] == Enum.at(resp2.body["rows"], 2)["key"]

    assert length(Enum.at(resp2.body["rows"], 0)["value"]) == 3

    row0_values = Enum.at(resp2.body["rows"], 0)["value"]

    assert Enum.at(row0_values, 0) == 1
    assert Enum.at(row0_values, 1) == 2
    assert Enum.at(row0_values, 2) == 3

    row1_values = Enum.at(resp2.body["rows"], 1)["value"]
    row2_values = Enum.at(resp2.body["rows"], 2)["value"]

    # we can't be 100% sure about the order for the same key
    assert (is_map(row1_values) and row1_values["a"] == 1) or
             (is_list(row1_values) and Enum.at(row1_values, 0) == "foo")

    assert (is_map(row1_values) and row1_values["b"] == 2) or
             (is_list(row1_values) and Enum.at(row1_values, 1) == "bar")

    assert (is_map(row2_values) and row2_values["a"] == 1) or
             (is_list(row2_values) and Enum.at(row2_values, 0) == "foo")

    assert (is_map(row2_values) and row2_values["b"] == 2) or
             (is_list(row2_values) and Enum.at(row2_values, 1) == "bar")

    assert is_list(row1_values) or !Map.has_key?(row1_values, "c")
    assert is_list(row2_values) or !Map.has_key?(row2_values, "c")
  end

  @tag :with_db
  test "runtime code evaluation can be prevented", context do
    db_name = context[:db_name]
    {:ok, _} = create_doc(db_name, @document)

    map_fun = """
    function(doc) {
      var glob = emit.constructor('return this')();
      emit(doc._id, null);
    }
    """

    resp = query(db_name, map_fun)
    assert resp["total_rows"] == 0
  end
end

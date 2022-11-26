defmodule ViewErrorsTest do
  use CouchTestCase

  @moduletag kind: :single_node

  @document %{integer: 1, string: "1", array: [1, 2, 3]}

  @tag :with_db
  test "emit undefined key results as null", context do
    db_name = context[:db_name]
    {:ok, _} = create_doc(db_name, @document)

    map_fun = """
    function(doc) {
      emit(doc.undef, null);
    }
    """

    # emitting a key value that is undefined should result in that row
    # being included in the view results as null
    results = query(db_name, map_fun)
    assert results["total_rows"] == 1
    assert Enum.at(results["rows"], 0)["key"] == :null
  end

  @tag :with_db
  test "exception in map function", context do
    db_name = context[:db_name]
    {:ok, _} = create_doc(db_name, @document)

    map_fun = """
    function(doc) {
      doc.undef(); // throws an error
    }
    """

    # if a view function throws an exception, its results are not included in
    # the view index, but the view does not itself raise an error
    results = query(db_name, map_fun)
    assert results["total_rows"] == 0
  end

  @tag :with_db
  test "emit undefined value results as null", context do
    db_name = context[:db_name]
    {:ok, _} = create_doc(db_name, @document)

    map_fun = """
    function(doc) {
      emit([doc._id, doc.undef], null);
    }
    """

    # if a view function includes an undefined value in the emitted key or
    # value, it is treated as null
    results = query(db_name, map_fun)
    assert results["total_rows"] == 1

    key =
      results["rows"]
      |> Enum.at(0)
      |> Map.get("key")
      |> Enum.at(1)

    assert key == :null
  end

  @tag :with_db
  test "query view with invalid params", context do
    db_name = context[:db_name]
    {:ok, _} = create_doc(db_name, @document)

    body = %{
      language: "javascript",
      map: "function(doc){emit(doc.integer)}"
    }

    # querying a view with invalid params should give a resonable error message
    resp =
      Couch.post("/#{db_name}/_all_docs?startkey=foo",
        headers: ["Content-Type": "application/json"],
        body: body
      )

    assert resp.body["error"] == "bad_request"

    resp =
      Couch.post("/#{db_name}/_all_docs",
        headers: ["Content-Type": "application/x-www-form-urlencoded"],
        body: body
      )

    assert resp.status_code == 415
  end

  @tag :with_db
  test "query parse error", context do
    db_name = context[:db_name]

    map_fun = """
    function(doc) {
      emit(doc.integer, doc.integer);
    }
    """

    ddoc_name = create_view(db_name, map_fun)

    resp = Couch.get("/#{db_name}/#{ddoc_name}/_view/view", query: [group: true])
    assert resp.status_code == 400
    assert resp.body["error"] == "query_parse_error"

    map_fun = "function() {emit(null, null)}"
    ddoc_name = create_view(db_name, map_fun)

    resp =
      Couch.get("/#{db_name}/#{ddoc_name}/_view/view", query: [startkey: 2, endkey: 1])

    assert resp.status_code == 400
    assert resp.body["error"] == "query_parse_error"
    assert String.contains?(resp.body["reason"], "No rows can match")

    design_doc = %{
      _id: "_design/test",
      language: "javascript",
      views: %{
        no_reduce: %{map: "function(doc) {emit(doc._id, null);}"},
        with_reduce: %{
          map: "function (doc) {emit(doc.integer, doc.integer)};",
          reduce: "function (keys, values) { return sum(values); };"
        }
      }
    }

    {:ok, _} = create_doc(db_name, design_doc)

    resp = Couch.get("/#{db_name}/_design/test/_view/no_reduce", query: [group: true])
    assert resp.status_code == 400
    assert resp.body["error"] == "query_parse_error"

    resp = Couch.get("/#{db_name}/_design/test/_view/no_reduce", query: [group_level: 1])
    assert resp.status_code == 400
    assert resp.body["error"] == "query_parse_error"

    resp = Couch.get("/#{db_name}/_design/test/_view/no_reduce", query: [reduce: true])
    assert resp.status_code == 400
    assert resp.body["error"] == "query_parse_error"

    resp = Couch.get("/#{db_name}/_design/test/_view/no_reduce", query: [reduce: false])
    assert resp.status_code == 200

    resp =
      Couch.get("/#{db_name}/_design/test/_view/with_reduce",
        query: [group: true, reduce: false]
      )

    assert resp.status_code == 400
    assert resp.body["error"] == "query_parse_error"

    resp =
      Couch.get("/#{db_name}/_design/test/_view/with_reduce",
        query: [group_level: 1, reduce: false]
      )

    assert resp.status_code == 400
    assert resp.body["error"] == "query_parse_error"
  end

  @tag :with_db
  test "infinite loop", context do
    db_name = context[:db_name]
    {:ok, _} = create_doc(db_name, @document)

    design_doc3 = %{
      _id: "_design/infinite",
      language: "javascript",
      views: %{
        infinite_loop: %{
          map: "function(doc) {while(true){emit(doc,doc);}};"
        }
      }
    }

    {:ok, _} = create_doc(db_name, design_doc3)

    resp = Couch.get("/#{db_name}/_design/infinite/_view/infinite_loop")
    assert resp.status_code == 500
    # This test has two different races. The first is whether
    # the while loop exhausts the JavaScript RAM limits before
    # timing. The second is a race between which of two timeouts
    # fires first. The first timeout is the couch_os_process
    # waiting for data back from couchjs. The second is the
    # gen_server call to couch_os_process.
    err_name = resp.body["error"]
    assert (
      err_name == "os_process_error" or
      err_name == "timeout" or
      err_name == "InternalError"
    )
  end

  @tag :with_db
  test "error responses for invalid multi-get bodies", context do
    db_name = context[:db_name]

    design_doc = %{
      _id: "_design/test",
      language: "javascript",
      views: %{
        no_reduce: %{map: "function(doc) {emit(doc._id, null);}"},
        with_reduce: %{
          map: "function (doc) {emit(doc.integer, doc.integer)};",
          reduce: "function (keys, values) { return sum(values); };"
        }
      }
    }

    {:ok, _} = create_doc(db_name, design_doc)

    resp =
      Couch.post("/#{db_name}/_design/test/_view/no_reduce",
        body: "[]"
      )

    assert resp.status_code == 400
    assert resp.body["error"] == "bad_request"
    assert resp.body["reason"] == "Request body must be a JSON object"

    resp =
      Couch.post("/#{db_name}/_design/test/_view/no_reduce",
        body: %{keys: 1}
      )

    assert resp.status_code == 400
    assert resp.body["error"] == "bad_request"
    assert resp.body["reason"] == "`keys` member must be an array."
  end

  @tag :with_db
  test "reduce overflow error", context do
    db_name = context[:db_name]
    {:ok, _} = create_doc(db_name, @document)

    design_doc2 = %{
      _id: "_design/testbig",
      language: "javascript",
      views: %{
        reduce_too_big: %{
          map: "function (doc) {emit(doc.integer, doc.integer)};",
          reduce:
            "function (keys, values) { var chars = []; for (var i=0; i < 1000; i++) {chars.push('wazzap');};return chars; };"
        }
      }
    }

    {:ok, _} = create_doc(db_name, design_doc2)

    resp = Couch.get("/#{db_name}/_design/testbig/_view/reduce_too_big")
    assert resp.status_code == 200
    # if the reduce grows to fast, throw an overflow error
    assert Enum.at(resp.body["rows"], 0)["error"] == "reduce_overflow_error"
  end

  @tag :with_db
  test "temporary view should give error message", context do
    db_name = context[:db_name]

    resp =
      Couch.post("/#{db_name}/_temp_view",
        headers: ["Content-Type": "application/json"],
        body: %{
          language: "javascript",
          map: "function(doc){emit(doc.integer)}"
        }
      )

    assert resp.status_code == 410
    assert resp.body["error"] == "gone"
    assert resp.body["reason"] == "Temporary views are not supported in CouchDB"
  end

  defp create_view(db_name, map_fun) do
    ddoc_name = "_design/temp_#{System.unique_integer([:positive])}"

    ddoc = %{
      _id: ddoc_name,
      language: "javascript",
      views: %{
        view: %{map: map_fun}
      }
    }

    {:ok, _} = create_doc(db_name, ddoc)
    ddoc_name
  end
end

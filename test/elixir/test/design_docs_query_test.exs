defmodule DesignDocsQueryTest do
  use CouchTestCase

  @moduletag :design_docs
  @moduletag kind: :single_node

  @moduledoc """
  Test CouchDB /{db}/_design_docs
  """

  setup_all do
    db_name = random_db_name()
    {:ok, _} = create_db(db_name)
    on_exit(fn -> delete_db(db_name) end)

    bulk_save(db_name, make_docs(1..5))

    Enum.each(1..5, fn x -> create_ddoc(db_name, x) end)

    {:ok, [db_name: db_name]}
  end

  defp create_ddoc(db_name, idx) do
    ddoc = %{
      _id: "_design/ddoc0#{idx}",
      views: %{
        testing: %{
          map: "function(){emit(1,1)}"
        }
      }
    }

    create_doc(db_name, ddoc)
  end

  test "query _design_docs (GET with no parameters)", context do
    db_name = context[:db_name]
    resp = Couch.get("/#{db_name}/_design_docs")
    assert resp.status_code == 200, "standard get should be 200"
    assert resp.body["total_rows"] == 5, "total_rows mismatch"
    assert length(resp.body["rows"]) == 5, "amount of rows mismatch"
  end

  test "query _design_docs with single key", context do
    db_name = context[:db_name]
    resp = Couch.get("/#{db_name}/_design_docs?key=\"_design/ddoc03\"")

    assert resp.status_code == 200, "standard get should be 200"
    assert length(resp.body["rows"]) == 1, "amount of rows mismatch"
    assert Enum.at(resp.body["rows"], 0)["key"] == "_design/ddoc03"
  end

  test "query _design_docs with multiple key", context do
    resp =
      Couch.get(
        "/#{context[:db_name]}/_design_docs",
        query: %{
          :keys => "[\"_design/ddoc02\", \"_design/ddoc03\"]"
        }
      )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 2
  end

  test "POST with empty body", context do
    resp =
      Couch.post(
        "/#{context[:db_name]}/_design_docs",
        body: %{}
      )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 5
  end

  test "POST with keys and limit", context do
    resp =
      Couch.post(
        "/#{context[:db_name]}/_design_docs",
        body: %{
          :keys => ["_design/ddoc02", "_design/ddoc03"],
          :limit => 1
        }
      )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 1
  end

  test "POST with query parameter and JSON body", context do
    resp =
      Couch.post(
        "/#{context[:db_name]}/_design_docs",
        query: %{
          :limit => 1
        },
        body: %{
          :keys => ["_design/ddoc02", "_design/ddoc03"]
        }
      )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 1
  end

  test "POST edge case with colliding parameters - query takes precedence", context do
    resp =
      Couch.post(
        "/#{context[:db_name]}/_design_docs",
        query: %{
          :limit => 0
        },
        body: %{
          :keys => ["_design/ddoc02", "_design/ddoc03"],
          :limit => 2
        }
      )

    assert resp.status_code == 200
    assert Enum.empty?(Map.get(resp, :body)["rows"])
  end

  test "query _design_docs descending=true", context do
    db_name = context[:db_name]
    resp = Couch.get("/#{db_name}/_design_docs?descending=true")

    assert resp.status_code == 200, "standard get should be 200"
    assert length(resp.body["rows"]) == 5, "amount of rows mismatch"
    assert Enum.at(resp.body["rows"], 0)["key"] == "_design/ddoc05"
  end

  test "query _design_docs descending=false", context do
    db_name = context[:db_name]
    resp = Couch.get("/#{db_name}/_design_docs?descending=false")

    assert resp.status_code == 200, "standard get should be 200"
    assert length(resp.body["rows"]) == 5, "amount of rows mismatch"
    assert Enum.at(resp.body["rows"], 0)["key"] == "_design/ddoc01"
  end

  test "query _design_docs end_key", context do
    db_name = context[:db_name]
    resp = Couch.get("/#{db_name}/_design_docs?end_key=\"_design/ddoc03\"")

    assert resp.status_code == 200, "standard get should be 200"
    assert length(resp.body["rows"]) == 3, "amount of rows mismatch"
    assert Enum.at(resp.body["rows"], 2)["key"] == "_design/ddoc03"
  end

  test "query _design_docs endkey", context do
    db_name = context[:db_name]
    resp = Couch.get("/#{db_name}/_design_docs?endkey=\"_design/ddoc03\"")

    assert resp.status_code == 200, "standard get should be 200"
    assert length(resp.body["rows"]) == 3, "amount of rows mismatch"
    assert Enum.at(resp.body["rows"], 2)["key"] == "_design/ddoc03"
  end

  test "query _design_docs start_key", context do
    db_name = context[:db_name]
    resp = Couch.get("/#{db_name}/_design_docs?start_key=\"_design/ddoc03\"")

    assert resp.status_code == 200, "standard get should be 200"
    assert length(resp.body["rows"]) == 3, "amount of rows mismatch"
    assert Enum.at(resp.body["rows"], 0)["key"] == "_design/ddoc03"
  end

  test "query _design_docs startkey", context do
    db_name = context[:db_name]
    resp = Couch.get("/#{db_name}/_design_docs?startkey=\"_design/ddoc03\"")

    assert resp.status_code == 200, "standard get should be 200"
    assert length(resp.body["rows"]) == 3, "amount of rows mismatch"
    assert Enum.at(resp.body["rows"], 0)["key"] == "_design/ddoc03"
  end

  test "query _design_docs end_key inclusive_end=true", context do
    db_name = context[:db_name]

    resp =
      Couch.get("/#{db_name}/_design_docs",
        query: [end_key: "\"_design/ddoc03\"", inclusive_end: true]
      )

    assert resp.status_code == 200, "standard get should be 200"
    assert length(resp.body["rows"]) == 3, "amount of rows mismatch"
    assert Enum.at(resp.body["rows"], 2)["key"] == "_design/ddoc03"
  end

  test "query _design_docs end_key inclusive_end=false", context do
    db_name = context[:db_name]

    resp =
      Couch.get("/#{db_name}/_design_docs",
        query: [end_key: "\"_design/ddoc03\"", inclusive_end: false]
      )

    assert resp.status_code == 200, "standard get should be 200"
    assert length(resp.body["rows"]) == 2, "amount of rows mismatch"
    assert Enum.at(resp.body["rows"], 1)["key"] == "_design/ddoc02"
  end

  test "query _design_docs end_key inclusive_end=false descending", context do
    db_name = context[:db_name]

    resp =
      Couch.get("/#{db_name}/_design_docs",
        query: [end_key: "\"_design/ddoc03\"", inclusive_end: false, descending: true]
      )

    assert resp.status_code == 200, "standard get should be 200"
    assert length(resp.body["rows"]) == 2, "amount of rows mismatch"
    assert Enum.at(resp.body["rows"], 1)["key"] == "_design/ddoc04"
  end

  test "query _design_docs end_key limit", context do
    db_name = context[:db_name]

    resp =
      Couch.get("/#{db_name}/_design_docs",
        query: [end_key: "\"_design/ddoc05\"", limit: 2]
      )

    assert resp.status_code == 200, "standard get should be 200"
    assert length(resp.body["rows"]) == 2, "amount of rows mismatch"
    assert Enum.at(resp.body["rows"], 1)["key"] == "_design/ddoc02"
  end

  test "query _design_docs end_key skip", context do
    db_name = context[:db_name]

    resp =
      Couch.get("/#{db_name}/_design_docs",
        query: [end_key: "\"_design/ddoc05\"", skip: 2]
      )

    assert resp.status_code == 200, "standard get should be 200"
    assert length(resp.body["rows"]) == 3, "amount of rows mismatch"
    assert Enum.at(resp.body["rows"], 0)["key"] == "_design/ddoc03"
    assert Enum.at(resp.body["rows"], 2)["key"] == "_design/ddoc05"
  end

  test "query _design_docs update_seq", context do
    db_name = context[:db_name]

    resp =
      Couch.get("/#{db_name}/_design_docs",
        query: [end_key: "\"_design/ddoc05\"", update_seq: true]
      )

    assert resp.status_code == 200, "standard get should be 200"
    assert Map.has_key?(resp.body, "update_seq")
  end

  test "query _design_docs post with keys", context do
    db_name = context[:db_name]

    resp =
      Couch.post("/#{db_name}/_design_docs",
        headers: ["Content-Type": "application/json"],
        body: %{keys: ["_design/ddoc02", "_design/ddoc03"]}
      )

    keys =
      resp.body["rows"]
      |> Enum.map(fn p -> p["key"] end)

    assert resp.status_code == 200, "standard get should be 200"
    assert length(resp.body["rows"]) == 2, "amount of rows mismatch"
    assert Enum.member?(keys, "_design/ddoc03")
    assert Enum.member?(keys, "_design/ddoc02")
  end
end

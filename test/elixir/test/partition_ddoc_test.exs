defmodule PartitionDDocTest do
  use CouchTestCase

  @moduledoc """
  Test partition design doc interactions
  """

  setup do
    db_name = random_db_name()
    {:ok, _} = create_db(db_name, query: %{partitioned: true, q: 1})
    on_exit(fn -> delete_db(db_name) end)

    {:ok, [db_name: db_name]}
  end

  test "PUT /dbname/_design/foo", context do
    db_name = context[:db_name]
    resp = Couch.put("/#{db_name}/_design/foo", body: %{stuff: "here"})
    assert resp.status_code == 201
  end

  test "PUT /dbname/_design/foo to update", context do
    db_name = context[:db_name]
    ddoc_id = "_design/foo"

    ddoc = %{
      _id: ddoc_id,
      stuff: "here"
    }

    resp = Couch.put("/#{db_name}/#{ddoc_id}", body: ddoc)
    assert resp.status_code == 201
    %{body: body} = resp

    ddoc = Map.put(ddoc, :_rev, body["rev"])
    ddoc = Map.put(ddoc, :other, "attribute")
    resp = Couch.put("/#{db_name}/#{ddoc_id}", body: ddoc)
    assert resp.status_code == 201
  end

  test "PUT /dbname/_design/foo/readme.txt", context do
    db_name = context[:db_name]
    ddoc_id = "_design/foo"

    ddoc = %{
      _id: ddoc_id,
      stuff: "here"
    }

    resp = Couch.put("/#{db_name}/#{ddoc_id}", body: ddoc)
    assert resp.status_code == 201
    %{body: body} = resp

    att = "This is a readme.txt"

    opts = [
      headers: [{:"Content-Type", "text/plain"}],
      query: [rev: body["rev"]],
      body: att
    ]

    resp = Couch.put("/#{db_name}/#{ddoc_id}/readme.txt", opts)
    assert resp.status_code == 201
  end

  test "DELETE /dbname/_design/foo", context do
    db_name = context[:db_name]
    ddoc_id = "_design/foo"

    ddoc = %{
      _id: ddoc_id,
      stuff: "here"
    }

    resp = Couch.put("/#{db_name}/#{ddoc_id}", body: ddoc)
    assert resp.status_code == 201
    %{body: body} = resp

    resp = Couch.delete("/#{db_name}/#{ddoc_id}", query: [rev: body["rev"]])
    assert resp.status_code == 200
  end

  test "POST /dbname with design doc", context do
    db_name = context[:db_name]
    body = %{_id: "_design/foo", stuff: "here"}
    resp = Couch.post("/#{db_name}", body: body)
    assert resp.status_code == 201
  end

  test "POST /dbname/_bulk_docs with design doc", context do
    db_name = context[:db_name]
    body = %{:docs => [%{_id: "_design/foo", stuff: "here"}]}
    resp = Couch.post("/#{db_name}/_bulk_docs", body: body)
    assert resp.status_code == 201
  end

  test "GET /dbname/_design/foo", context do
    db_name = context[:db_name]
    resp = Couch.put("/#{db_name}/_design/foo", body: %{stuff: "here"})
    assert resp.status_code == 201

    resp = Couch.get("/#{db_name}/_design/foo")
    assert resp.status_code == 200
  end

  test "GET /dbname/_design/foo?rev=$rev", context do
    db_name = context[:db_name]
    resp = Couch.put("/#{db_name}/_design/foo", body: %{stuff: "here"})
    assert resp.status_code == 201
    %{body: body} = resp

    resp = Couch.get("/#{db_name}/_design/foo", query: [rev: body["rev"]])
    assert resp.status_code == 200
  end

  test "GET /dbname/_bulk_get", context do
    db_name = context[:db_name]
    resp = Couch.put("/#{db_name}/_design/foo", body: %{stuff: "here"})
    assert resp.status_code == 201

    body = %{docs: [%{id: "_design/foo"}]}
    resp = Couch.post("/#{db_name}/_bulk_get", body: body)
    assert resp.status_code == 200
    %{body: body} = resp

    assert length(body["results"]) == 1

    %{"results" => [%{"id" => "_design/foo", "docs" => [%{"ok" => _}]}]} = body
  end

  test "GET /dbname/_bulk_get with rev", context do
    db_name = context[:db_name]
    resp = Couch.put("/#{db_name}/_design/foo", body: %{stuff: "here"})
    assert resp.status_code == 201
    %{body: body} = resp

    body = %{docs: [%{id: "_design/foo", rev: body["rev"]}]}
    resp = Couch.post("/#{db_name}/_bulk_get", body: body)
    assert resp.status_code == 200
    %{body: body} = resp

    assert length(body["results"]) == 1
    %{"results" => [%{"id" => "_design/foo", "docs" => [%{"ok" => _}]}]} = body
  end

  test "GET /dbname/_all_docs?key=$ddoc_id", context do
    db_name = context[:db_name]
    resp = Couch.put("/#{db_name}/_design/foo", body: %{stuff: "here"})
    assert resp.status_code == 201

    resp = Couch.get("/#{db_name}/_all_docs", query: [key: "\"_design/foo\""])
    assert resp.status_code == 200
    %{body: body} = resp

    assert length(body["rows"]) == 1
    %{"rows" => [%{"id" => "_design/foo"}]} = body
  end

  test "GET /dbname/_design_docs", context do
    db_name = context[:db_name]
    resp = Couch.put("/#{db_name}/_design/foo", body: %{stuff: "here"})
    assert resp.status_code == 201

    resp = Couch.get("/#{db_name}/_design_docs")
    assert resp.status_code == 200
    %{body: body} = resp

    assert length(body["rows"]) == 1
    %{"rows" => [%{"id" => "_design/foo"}]} = body
  end
end

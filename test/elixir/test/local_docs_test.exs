defmodule LocalDocsTest do
  use CouchTestCase

  @moduletag :local_docs
  @moduletag kind: :single_node

  @moduledoc """
  Test CouchDB _local_docs
  """

  setup_all do
    db_name = random_db_name()
    {:ok, _} = create_db(db_name)
    on_exit(fn -> delete_db(db_name) end)

    resp1 = Couch.put(
      "/#{db_name}/_local/foo",
      body: %{
        _id: "foo",
        bar: "baz"
      }
    )
    assert resp1.status_code == 201

    resp2 = Couch.put(
      "/#{db_name}/_local/foo2",
      body: %{
        _id: "foo",
        bar: "baz2"
      }
    )
    assert resp2.status_code == 201

    {:ok, [db_name: db_name]}
  end

  test "GET with no parameters", context do
    resp = Couch.get(
      "/#{context[:db_name]}/_local_docs"
    )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 2
  end

  test "GET with multiple keys", context do
    resp = Couch.get(
      "/#{context[:db_name]}/_local_docs",
      query: %{
        :keys => "[\"_local/foo\", \"_local/foo2\"]",
      }
    )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 2
  end

  test "POST with empty body", context do
    resp = Couch.post(
      "/#{context[:db_name]}/_local_docs",
      body: %{}
    )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 2
  end

  test "POST with keys and limit", context do
    resp = Couch.post(
      "/#{context[:db_name]}/_local_docs",
      body: %{
        :keys => ["_local/foo", "_local/foo2"],
        :limit => 1
      }
    )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 1
  end

  test "POST with query parameter and JSON body", context do
    resp = Couch.post(
      "/#{context[:db_name]}/_local_docs",
      query: %{
        :limit => 1
      },
      body: %{
        :keys => ["_local/foo", "_local/foo2"]
      }
    )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 1
  end

  test "POST edge case with colliding parameters - query takes precedence", context do
    resp = Couch.post(
      "/#{context[:db_name]}/_local_docs",
      query: %{
        :limit => 0
      },
      body: %{
        :keys => ["_local/foo", "_local/foo2"],
        :limit => 2
      }
    )

    assert resp.status_code == 200
    assert Enum.empty?(Map.get(resp, :body)["rows"])
  end
end

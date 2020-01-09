defmodule DesignDocsTest do
  use CouchTestCase

  @moduletag :design_docs

  @moduledoc """
  Test CouchDB /{db}/_design_docs
  """

  setup_all do
    db_name = random_db_name()
    {:ok, _} = create_db(db_name)
    on_exit(fn -> delete_db(db_name) end)

    {:ok, _} = create_doc(
      db_name,
      %{
        _id: "_design/foo",
        bar: "baz"
      }
    )

    {:ok, _} = create_doc(
      db_name,
      %{
        _id: "_design/foo2",
        bar: "baz2"
      }
    )

    {:ok, [db_name: db_name]}
  end

  test "GET with no parameters", context do
    resp = Couch.get(
      "/#{context[:db_name]}/_design_docs"
    )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 2
  end

  test "GET with multiple keys", context do
    resp = Couch.get(
      "/#{context[:db_name]}/_design_docs",
      query: %{
        :keys => "[\"_design/foo\", \"_design/foo2\"]",
      }
    )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 2
  end

  test "POST with empty body", context do
    resp = Couch.post(
      "/#{context[:db_name]}/_design_docs",
      body: %{}
    )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 2
  end

  test "POST with keys and limit", context do
    resp = Couch.post(
      "/#{context[:db_name]}/_design_docs",
      body: %{
        :keys => ["_design/foo", "_design/foo2"],
        :limit => 1
      }
    )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 1
  end

  test "POST with query parameter and JSON body", context do
    resp = Couch.post(
      "/#{context[:db_name]}/_design_docs",
      query: %{
        :limit => 1
      },
      body: %{
        :keys => ["_design/foo", "_design/foo2"]
      }
    )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 1
  end

  test "POST edge case with colliding parameters - query takes precedence", context do
    resp = Couch.post(
      "/#{context[:db_name]}/_design_docs",
      query: %{
        :limit => 0
      },
      body: %{
        :keys => ["_design/foo", "_design/foo2"],
        :limit => 2
      }
    )

    assert resp.status_code == 200
    assert Enum.empty?(Map.get(resp, :body)["rows"])
  end
end

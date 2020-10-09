defmodule ViewTest do
  use CouchTestCase

  @moduletag :view
  @moduletag kind: :single_node

  @moduledoc """
  Test CouchDB /{db}/_design/{ddoc}/_view/{view}
  """

  setup_all do
    db_name = random_db_name()
    {:ok, _} = create_db(db_name)
    on_exit(fn -> delete_db(db_name) end)

    {:ok, _} = create_doc(
      db_name,
      %{
        _id: "foo",
        bar: "baz"
      }
    )

    {:ok, _} = create_doc(
      db_name,
      %{
        _id: "foo2",
        bar: "baz2"
      }
    )

    map_fun = """
      function(doc) {
        emit(doc._id, doc.bar);
      }
    """


    body = %{
      :docs => [
        %{
          _id: "_design/map",
          views: %{
            some: %{
              map: map_fun
            }
          }
        }
      ]
    }

    resp = Couch.post("/#{db_name}/_bulk_docs", body: body)
    Enum.each(resp.body, &assert(&1["ok"]))

    {:ok, [db_name: db_name]}
  end

  test "GET with no parameters", context do
    resp = Couch.get(
      "/#{context[:db_name]}/_design/map/_view/some"
    )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 2
  end

  test "GET with one key", context do
    resp = Couch.get(
      "/#{context[:db_name]}/_design/map/_view/some",
      query: %{
        :key => "\"foo\"",
      }
    )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 1
  end

  test "GET with multiple keys", context do
    resp = Couch.get(
      "/#{context[:db_name]}/_design/map/_view/some",
      query: %{
        :keys => "[\"foo\", \"foo2\"]",
      }
    )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 2
  end

  test "POST with empty body", context do
    resp = Couch.post(
      "/#{context[:db_name]}/_design/map/_view/some",
      body: %{}
    )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 2
  end

  test "POST with keys and limit", context do
    resp = Couch.post(
      "/#{context[:db_name]}/_design/map/_view/some",
      body: %{
        :keys => ["foo", "foo2"],
        :limit => 1
      }
    )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 1
  end

  test "POST with query parameter and JSON body", context do
    resp = Couch.post(
      "/#{context[:db_name]}/_design/map/_view/some",
      query: %{
        :limit => 1
      },
      body: %{
        :keys => ["foo", "foo2"]
      }
    )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 1
  end

  test "POST edge case with colliding parameters - query takes precedence", context do
    resp = Couch.post(
      "/#{context[:db_name]}/_design/map/_view/some",
      query: %{
        :limit => 1
      },
      body: %{
        :keys => ["foo", "foo2"],
        :limit => 2
      }
    )

    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 1
  end
end

defmodule DesignPathTest do
  use CouchTestCase

  @moduletag :design_docs
  @moduletag kind: :single_node

  @moduledoc """
  Test CouchDB design documents path
  """
  @tag :with_db
  test "design doc path", context do
    db_name = context[:db_name]
    ddoc_path_test(db_name)
  end

  @tag :with_db_name
  test "design doc path with slash in db name", context do
    db_name = URI.encode_www_form(context[:db_name] <> "/with_slashes")
    create_db(db_name)
    ddoc_path_test(db_name)
  end

  defp ddoc_path_test(db_name) do
    create_test_view(db_name, "_design/test")

    resp = Couch.get("/#{db_name}/_design/test")
    assert resp.body["_id"] == "_design/test"

    resp =
      Couch.get(Couch.process_url("/#{db_name}/_design%2Ftest"),
        follow_redirects: true
      )

    assert resp.body["_id"] == "_design/test"

    resp = Couch.get("/#{db_name}/_design/test/_view/testing")
    assert Enum.empty?(Map.get(resp, :body)["rows"])

    design_doc2 = %{
      _id: "_design/test2",
      views: %{
        testing: %{
          map: "function(){emit(1,1)}"
        }
      }
    }

    resp = Couch.put("/#{db_name}/_design/test2", body: design_doc2)
    assert resp.status_code == 201

    resp = Couch.get("/#{db_name}/_design/test2")
    assert resp.body["_id"] == "_design/test2"

    resp =
      Couch.get(Couch.process_url("/#{db_name}/_design%2Ftest2"),
        follow_redirects: true
      )

    assert resp.body["_id"] == "_design/test2"

    resp = Couch.get("/#{db_name}/_design/test2/_view/testing")
    assert Enum.empty?(Map.get(resp, :body)["rows"])
  end

  defp create_test_view(db_name, id) do
    design_doc = %{
      _id: id,
      views: %{
        testing: %{
          map: "function(){emit(1,1)}"
        }
      }
    }

    create_doc(db_name, design_doc)
  end
end

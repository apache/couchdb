defmodule DesignOptionsTest do
  use CouchTestCase

  @moduletag :design_docs
  @moduletag kind: :single_node

  @moduledoc """
  Test CouchDB design documents options include_design and local_seq
  """
  @tag :with_db
  test "design doc options - include_desing=true", context do
    db_name = context[:db_name]

    create_test_view(db_name, "_design/fu", %{include_design: true})

    resp = Couch.get("/#{db_name}/_design/fu/_view/data")
    assert resp.status_code == 200
    assert length(Map.get(resp, :body)["rows"]) == 1
    assert Enum.at(resp.body["rows"], 0)["value"] == "_design/fu"
  end

  @tag :with_db
  test "design doc options - include_desing=false", context do
    db_name = context[:db_name]

    create_test_view(db_name, "_design/bingo", %{include_design: false})

    resp = Couch.get("/#{db_name}/_design/bingo/_view/data")
    assert resp.status_code == 200
    assert Enum.empty?(Map.get(resp, :body)["rows"])
  end

  @tag :with_db
  test "design doc options - include_design default value", context do
    db_name = context[:db_name]

    create_test_view(db_name, "_design/bango", %{})

    resp = Couch.get("/#{db_name}/_design/bango/_view/data")
    assert resp.status_code == 200
    assert Enum.empty?(Map.get(resp, :body)["rows"])
  end

  @tag :with_db
  test "design doc options - local_seq=true", context do
    db_name = context[:db_name]

    create_test_view(db_name, "_design/fu", %{include_design: true, local_seq: true})
    create_doc(db_name, %{})
    resp = Couch.get("/#{db_name}/_design/fu/_view/with_seq")

    row_with_key =
      resp.body["rows"]
      |> Enum.filter(fn p -> p["key"] != :null end)

    assert length(row_with_key) == 2
  end

  defp create_test_view(db_name, id, options) do
    map = "function (doc) {emit(null, doc._id);}"
    withseq = "function(doc) {emit(doc._local_seq, null)}"

    design_doc = %{
      _id: id,
      language: "javascript",
      options: options,
      views: %{
        data: %{map: map},
        with_seq: %{map: withseq}
      }
    }

    create_doc(db_name, design_doc)
  end
end

defmodule LargeDocsTest do
  use CouchTestCase

  @moduletag :large_docs
  @moduletag kind: :single_node

  @long_string "0123456789\n"

  @moduledoc """
  Test saving a bunch of large documents.
  This is a port of the large_docs.js suite
  """

  @tag :with_db
  test "Large docs", context do
    db_name = context[:db_name]
    long_text = String.duplicate(@long_string, 10)

    resp1 = Couch.post("/#{db_name}", body: %{:_id => "0", :longtest => long_text}).body
    resp2 = Couch.post("/#{db_name}", body: %{:_id => "1", :longtest => long_text}).body
    resp3 = Couch.post("/#{db_name}", body: %{:_id => "2", :longtest => long_text}).body
    resp4 = Couch.post("/#{db_name}", body: %{:_id => "3", :longtest => long_text}).body

    assert resp1["ok"]
    assert resp2["ok"]
    assert resp3["ok"]
    assert resp4["ok"]

    %{"rows" => rows} = query(db_name)
    assert Enum.count(rows) === 4
    Enum.each(rows, fn row -> assert row["value"] === long_text end)
  end

  defp query(db_name) do
    map_fun = "function(doc) { emit(null, doc.longtest); }"
    map_doc = %{:views => %{:view => %{:map => map_fun}}}
    %{"rev" => rev} = Couch.put("/#{db_name}/_design/tempddoc", body: map_doc).body
    response = Couch.get("/#{db_name}/_design/tempddoc/_view/view").body
    Couch.delete("/#{db_name}/_design/tempddoc?rev=#{rev}")
    response
  end
end

defmodule ReduceFalseTest do
  use CouchTestCase

  @moduletag :views
  @moduletag kind: :single_node

  @moduledoc """
  Test CouchDB view without reduces
  This is a port of the reduce_false.js suite
  """

  def summate(n) do
    (n + 1) * n / 2
  end

  @tag :with_db
  test "Basic reduce functions", context do
    db_name = context[:db_name]
    view_url = "/#{db_name}/_design/foo/_view/summate"
    num_docs = 5

    map = ~s"""
    function (doc) {
      emit(doc.integer, doc.integer);
    };
    """

    reduce = "function (keys, values) { return sum(values); };"
    red_doc = %{:views => %{:summate => %{:map => map, :reduce => reduce}}}
    assert Couch.put("/#{db_name}/_design/foo", body: red_doc).body["ok"]

    docs = make_docs(1..num_docs)
    resp = Couch.post("/#{db_name}/_bulk_docs", body: %{:docs => docs}, query: %{w: 3})
    assert resp.status_code in [201, 202]

    # Test that the reduce works
    rows = Couch.get(view_url).body["rows"]
    assert length(rows) == 1
    assert hd(rows)["value"] == summate(num_docs)

    # Test that we got our docs back
    rows = Couch.get(view_url, query: %{reduce: false}).body["rows"]
    assert length(rows) == 5

    rows
    |> Enum.with_index(1)
    |> Enum.each(fn {row, i} ->
      assert i == row["value"]
    end)
  end
end

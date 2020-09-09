defmodule ViewCollationTest do
  use CouchTestCase

  @moduledoc """
  Test CouchDB View Collation Behavior
  This is a port of the view_collation.js suite
  """

  @moduletag kind: :single_node

  @values [
    # Special values sort before all other types
    :null,
    false,
    true,

    # Then numbers
    1,
    2,
    3.0,
    4,

    # Then text, case sensitive
    "a",
    "A",
    "aa",
    "b",
    "B",
    "ba",
    "bb",

    # Then arrays, compared element by element until different.
    # Longer arrays sort after their prefixes
    ["a"],
    ["b"],
    ["b", "c"],
    ["b", "c", "a"],
    ["b", "d"],
    ["b", "d", "e"],

    # Then objects, compared each key value in the list until different.
    # Larger objects sort after their subset objects
    {[a: 1]},
    {[a: 2]},
    {[b: 1]},
    {[b: 2]},
    # Member order does matter for collation
    {[b: 2, a: 1]},
    {[b: 2, c: 2]}
  ]

  setup_all do
    db_name = random_db_name()
    {:ok, _} = create_db(db_name)
    on_exit(fn -> delete_db(db_name) end)

    {docs, _} =
      Enum.flat_map_reduce(@values, 1, fn value, idx ->
        doc = %{:_id => Integer.to_string(idx), :foo => value}
        {[doc], idx + 1}
      end)

    resp = Couch.post("/#{db_name}/_bulk_docs", body: %{:docs => docs})
    Enum.each(resp.body, &assert(&1["ok"]))

    map_fun = "function(doc) { emit(doc.foo, null); }"
    map_doc = %{:views => %{:foo => %{:map => map_fun}}}
    resp = Couch.put("/#{db_name}/_design/foo", body: map_doc)
    assert resp.body["ok"]

    {:ok, [db_name: db_name]}
  end

  test "ascending collation order", context do
    resp = Couch.get(url(context))
    pairs = Enum.zip(resp.body["rows"], @values)

    Enum.each(pairs, fn {row, value} ->
      assert row["key"] == convert(value)
    end)
  end

  test "descending collation order", context do
    resp = Couch.get(url(context), query: %{"descending" => "true"})
    pairs = Enum.zip(resp.body["rows"], Enum.reverse(@values))

    Enum.each(pairs, fn {row, value} ->
      assert row["key"] == convert(value)
    end)
  end

  test "key query option", context do
    Enum.each(@values, fn value ->
      resp = Couch.get(url(context), query: %{:key => :jiffy.encode(value)})
      assert length(resp.body["rows"]) == 1
      assert Enum.at(resp.body["rows"], 0)["key"] == convert(value)
    end)
  end

  test "inclusive_end=true", context do
    query = %{:endkey => :jiffy.encode("b"), :inclusive_end => true}
    resp = Couch.get(url(context), query: query)
    assert Enum.at(resp.body["rows"], -1)["key"] == "b"

    query = Map.put(query, :descending, true)
    resp = Couch.get(url(context), query: query)
    assert Enum.at(resp.body["rows"], -1)["key"] == "b"
  end

  test "inclusive_end=false", context do
    query = %{:endkey => :jiffy.encode("b"), :inclusive_end => false}
    resp = Couch.get(url(context), query: query)
    assert Enum.at(resp.body["rows"], -1)["key"] == "aa"

    query = Map.put(query, :descending, true)
    resp = Couch.get(url(context), query: query)
    assert Enum.at(resp.body["rows"], -1)["key"] == "B"

    query = %{
      :endkey => :jiffy.encode("b"),
      :endkey_docid => 11,
      :inclusive_end => false
    }

    resp = Couch.get(url(context), query: query)
    assert Enum.at(resp.body["rows"], -1)["key"] == "aa"

    query = Map.put(query, :endkey_docid, 12)
    resp = Couch.get(url(context), query: query)
    assert Enum.at(resp.body["rows"], -1)["key"] == "b"
  end

  def url(context) do
    "/#{context[:db_name]}/_design/foo/_view/foo"
  end

  def convert(value) do
    :jiffy.decode(:jiffy.encode(value), [:return_maps])
  end
end

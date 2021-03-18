defmodule ConcurrentWritesTest do
  use CouchTestCase

  @moduletag :concurrent_writes
  @moduletag kind: :single_node

  @moduledoc """
  Test CouchDB under concurrent write load
  """

  @tag :with_db
  test "Primary data tests", context do
    n = 120
    db_name = context[:db_name]
    parent = self()
    Enum.each(1..n,
      fn x -> spawn fn ->
          r = Couch.put("/#{db_name}/doc#{x}", body: %{:a => x})
          assert r.status_code == 201
          send parent, :done
        end end)
    Enum.each(1..n, fn _x -> receive do :done -> :done end end)
    Enum.each(1..n, fn x ->
      assert Couch.get("/#{db_name}/doc#{x}").body["a"] == x
    end)
    assert Couch.get("/#{db_name}").body["doc_count"] == n
  end

  @tag :with_db
  test "Secondary data tests", context do
    n = 120
    db_name = context[:db_name]
    map_fun = "function(doc) { emit(null, doc.a); }"
    red_fun = "_sum"
    ddoc_id = "_design/foo"
    ddoc = %{:views => %{:foo => %{:map => map_fun, :reduce => red_fun}}}
    Couch.put("/#{db_name}/#{ddoc_id}", body: ddoc)
    parent = self()
    Enum.each(1..n,
      fn x -> spawn fn ->
          r = Couch.put("/#{db_name}/doc#{x}", body: %{:a => x})
          assert r.status_code == 201
          send parent, :done
        end end)
    Enum.each(1..n, fn _x -> receive do :done -> :done end end)
    rows = Couch.get("/#{db_name}/#{ddoc_id}/_view/foo").body["rows"]
    result = hd(rows)["value"]
    assert result == Enum.sum(1..n)
  end

  @tag :with_db
  test "Secondary data tests with updates", context do
    n = 120
    db_name = context[:db_name]
    map_fun = "function(doc) { emit(null, doc.a); }"
    red_fun = "_sum"
    ddoc_id = "_design/foo"
    ddoc = %{:views => %{:foo => %{:map => map_fun, :reduce => red_fun}}}
    Couch.put("/#{db_name}/#{ddoc_id}", body: ddoc)
    parent = self()
    Enum.each(1..n,
      fn x -> spawn fn ->
          r = Couch.put("/#{db_name}/doc#{x}", body: %{:a => x})
          assert r.status_code == 201
          rev = r.body["rev"]
          Couch.put("/#{db_name}/doc#{x}", body: %{:_rev => rev, :a => x + 1})
          send parent, :done
        end end)
    Enum.each(1..n, fn _x -> receive do :done -> :done end end)
    rows = Couch.get("/#{db_name}/#{ddoc_id}/_view/foo").body["rows"]
    result = hd(rows)["value"]
    assert result == Enum.sum(2..n + 1)
  end

end

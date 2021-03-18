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

  @tag :with_db
  test "Secondary data tests with updates and queries", context do
    n = 120
    query_every_n = 40
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
          if rem(x, query_every_n) == 0 do
              r = Couch.get("/#{db_name}/#{ddoc_id}/_view/foo")
              assert r.status_code == 200
          end
          r = Couch.put("/#{db_name}/doc#{x}", body: %{:_rev => rev, :a => x + 1})
          assert r.status_code == 201
          send parent, :done
        end end)
    Enum.each(1..n, fn _x -> receive do :done -> :done end end)
    rows = Couch.get("/#{db_name}/#{ddoc_id}/_view/foo").body["rows"]
    result = hd(rows)["value"]
    assert result == Enum.sum(2..n + 1)
  end

  # The following test was specifically crafted to trigger the issue fixed in:
  # https://github.com/apache/couchdb/commit/ec4b2132918338d893a800a823cf5f12d5b2efd5
  #
  @tag :with_db
  test "Secondary data tests with deletes and queries", context do
    n = 120
    query_every_n = 40
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
          :timer.sleep(:rand.uniform(1000))
          r = Couch.delete("/#{db_name}/doc#{x}?rev=#{rev}")
          assert r.status_code == 200
          if rem(x, query_every_n) == 0 do
              r = Couch.get("/#{db_name}/#{ddoc_id}/_view/foo")
              assert r.status_code == 200
          end
          send parent, :done
        end end)
    Enum.each(1..n, fn _x -> receive do :done -> :done end end)

    # Keep trying to query the view for a bit to account for the case when
    # partial view results can be returned. After the following commits merges
    # `retry_until` can be removed:
    # https://github.com/apache/couchdb/pull/3391/commits/5a82664d1b0b58dd6c9fe6a79faa51e89211969e
    #
    try do
        retry_until(fn ->
            [] == Couch.get("/#{db_name}/#{ddoc_id}/_view/foo").body["rows"]
        end, 1000, 5000)
    rescue
        RuntimeError -> :ok
    end

    assert [] == Couch.get("/#{db_name}/#{ddoc_id}/_view/foo").body["rows"]
  end

end

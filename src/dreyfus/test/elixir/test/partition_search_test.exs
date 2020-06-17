defmodule PartitionSearchTest do
  use CouchTestCase

  @moduletag :search

  @moduledoc """
  Test Partition functionality with search
  """

  def create_search_docs(db_name, pk1 \\ "foo", pk2 \\ "bar") do
    docs = for i <- 1..10 do
      id = if rem(i, 2) == 0 do
        "#{pk1}:#{i}"
      else
        "#{pk2}:#{i}"
      end
      %{
        :_id => id,
        :value => i,
        :some => "field"
      }
    end

    resp = Couch.post("/#{db_name}/_bulk_docs", headers: ["Content-Type": "application/json"], body: %{:docs => docs}, query: %{w: 3})
    assert resp.status_code in [201, 202]
  end

  def create_ddoc(db_name, opts \\ %{}) do
    index_fn = "function(doc) {\n  if (doc.some) {\n    index('some', doc.some);\n }\n}"
    default_ddoc = %{
      indexes: %{
        books: %{
          analyzer: %{name: "standard"},
          index: index_fn
        }
      }
    }

    ddoc = Enum.into(opts, default_ddoc)

    resp = Couch.put("/#{db_name}/_design/library", body: ddoc)
    assert resp.status_code in [201, 202]
    assert Map.has_key?(resp.body, "ok") == true
  end

  def get_ids (resp) do
    %{:body => %{"rows" => rows}} = resp
    Enum.map(rows, fn row -> row["id"] end)
  end

  @tag :with_partitioned_db
  test "Simple query returns partitioned search results", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_partition/foo/_design/library/_search/books"
    resp = Couch.get(url, query: %{q: "some:field"})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert ids == ["foo:10", "foo:2", "foo:4", "foo:6", "foo:8"]

    url = "/#{db_name}/_partition/bar/_design/library/_search/books"
    resp = Couch.get(url, query: %{q: "some:field"})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert ids == ["bar:1", "bar:3", "bar:5", "bar:7", "bar:9"]
  end

  @tag :with_partitioned_db
  test "Only returns docs in partition not those in shard", context do
    db_name = context[:db_name]
    create_search_docs(db_name, "foo", "bar42")
    create_ddoc(db_name)

    url = "/#{db_name}/_partition/foo/_design/library/_search/books"
    resp = Couch.get(url, query: %{q: "some:field"})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert ids == ["foo:10", "foo:2", "foo:4", "foo:6", "foo:8"]
  end

  @tag :with_partitioned_db
  test "Works with bookmarks and limit", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_partition/foo/_design/library/_search/books"
    resp = Couch.get(url, query: %{q: "some:field", limit: 3})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert ids == ["foo:10", "foo:2", "foo:4"]

    %{:body => %{"bookmark" => bookmark}} = resp

    resp = Couch.get(url, query: %{q: "some:field", limit: 3, bookmark: bookmark})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert ids == ["foo:6", "foo:8"]

    resp = Couch.get(url, query: %{q: "some:field", limit: 2000, bookmark: bookmark})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert ids == ["foo:6", "foo:8"]

    resp = Couch.get(url, query: %{q: "some:field", limit: 2001, bookmark: bookmark})
    assert resp.status_code == 400
  end

  @tag :with_db
  test "Works with limit using POST for on non-partitioned db", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/library/_search/books"
    resp = Couch.post(url, body: %{:q => "some:field", :limit => 1})
    assert resp.status_code == 200
  end

  @tag :with_partitioned_db
  test "Works with limit using POST for partitioned db", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_partition/foo/_design/library/_search/books"
    resp = Couch.post(url, body: %{:q => "some:field", :limit => 1})
    assert resp.status_code == 200
  end

  @tag :with_partitioned_db
  test "Cannot do global query with partition view", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/library/_search/books"
    resp = Couch.get(url, query: %{q: "some:field"})
    assert resp.status_code == 400
    %{:body => %{"reason" => reason}} = resp
    assert Regex.match?(~r/mandatory for queries to this index./, reason)
  end

  @tag :with_partitioned_db
  test "Cannot do partition query with global search ddoc", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name, options: %{partitioned: false})

    url = "/#{db_name}/_partition/foo/_design/library/_search/books"
    resp = Couch.get(url, query: %{q: "some:field"})
    assert resp.status_code == 400
    %{:body => %{"reason" => reason}} = resp
    assert reason == "`partition` not supported on this index"
  end

  @tag :with_db
  test "normal search on non-partitioned dbs still work", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/library/_search/books"
    resp = Couch.get(url, query: %{q: "some:field"})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert Enum.sort(ids) == Enum.sort(["bar:1", "bar:5", "bar:9", "foo:2", "bar:3", "foo:4", "foo:6", "bar:7", "foo:8", "foo:10"])
  end

  @tag :with_db
  test "normal search on non-partitioned dbs without limit", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/library/_search/books"
    resp = Couch.get(url, query: %{q: "some:field"})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert Enum.sort(ids) == Enum.sort(["bar:1", "bar:5", "bar:9", "foo:2", "bar:3", "foo:4", "foo:6", "bar:7", "foo:8", "foo:10"])
  end

  @tag :with_db
  test "normal search on non-partitioned dbs with limit", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/library/_search/books"
    resp = Couch.get(url, query: %{q: "some:field", limit: 3})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert Enum.sort(ids) == Enum.sort(["bar:1", "bar:5", "bar:9"])
  end

  @tag :with_db
  test "normal search on non-partitioned dbs with over limit", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/library/_search/books"
    resp = Couch.get(url, query: %{q: "some:field", limit: 201})
    assert resp.status_code == 400
  end

  @tag :with_partitioned_db
  test "rejects conflicting partition values", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_partition/foo/_design/library/_search/books"
    resp = Couch.post(url, body: %{q: "some:field", partition: "bar"})
    assert resp.status_code == 400
  end

  @tag :with_partitioned_db
  test "restricted parameters are not allowed in query or body", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    body = %{q: "some:field", partition: "foo"}

    Enum.each(
      [
        {:counts, "[\"type\"]"},
        {:group_field, "some"},
        {:ranges, :jiffy.encode(%{price: %{cheap: "[0 TO 100]"}})},
        {:drilldown, "[\"key\",\"a\"]"},
      ],
      fn {key, value} ->
        url = "/#{db_name}/_partition/foo/_design/library/_search/books"
        bannedparam = Map.put(body, key, value)
        get_resp = Couch.get(url, query: bannedparam)
        %{:body => %{"reason" => get_reason}} = get_resp
        assert Regex.match?(~r/are incompatible/, get_reason)
        post_resp = Couch.post(url, body: bannedparam)
        %{:body => %{"reason" => post_reason}} = post_resp
        assert Regex.match?(~r/are incompatible/, post_reason)
      end
    )
  end
end

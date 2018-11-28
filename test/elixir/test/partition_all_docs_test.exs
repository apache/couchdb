defmodule PartitionAllDocsTest do
  use CouchTestCase
  import PartitionHelpers

  @moduledoc """
  Test Partition functionality for for all_docs
  """

  setup_all do
    db_name = random_db_name()
    {:ok, _} = create_db(db_name, query: %{partitioned: true, q: 1})
    on_exit(fn -> delete_db(db_name) end)

    create_partition_docs(db_name)

    {:ok, [db_name: db_name]}
  end

  test "all_docs with partitioned:true returns partitioned fields", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_partition/foo/_all_docs"
    resp = Couch.get(url)
    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert Enum.dedup(partitions) == ["foo"]

    url = "/#{db_name}/_partition/bar/_all_docs"
    resp = Couch.get(url)
    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert Enum.dedup(partitions) == ["bar"]
  end

  test "partition all_docs errors with incorrect partition supplied", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_partition/_bar/_all_docs"
    resp = Couch.get(url)
    assert resp.status_code == 400

    url = "/#{db_name}/_partition//_all_docs"
    resp = Couch.get(url)
    assert resp.status_code == 400
  end

  test "partitioned _all_docs works with startkey, endkey range", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_partition/foo/_all_docs"
    resp = Couch.get(url, query: %{start_key: "\"foo:12\"", end_key: "\"foo:2\""})
    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 5
    assert Enum.dedup(partitions) == ["foo"]
  end

  test "partitioned _all_docs works with keys", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_partition/foo/_all_docs"
    resp = Couch.post(url, body: %{keys: ["foo:2", "foo:4", "foo:6"]})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert length(ids) == 3
    assert ids == ["foo:2", "foo:4", "foo:6"]
  end

  test "partition _all_docs works with limit", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_partition/foo/_all_docs"
    resp = Couch.get(url, query: %{limit: 5})
    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 5
    assert Enum.dedup(partitions) == ["foo"]
  end

  test "partition _all_docs with descending", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_partition/foo/_all_docs"
    resp = Couch.get(url, query: %{descending: true, limit: 5})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert length(ids) == 5
    assert ids == ["foo:98", "foo:96", "foo:94", "foo:92", "foo:90"]

    resp = Couch.get(url, query: %{descending: false, limit: 5})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert length(ids) == 5
    assert ids == ["foo:10", "foo:100", "foo:12", "foo:14", "foo:16"]
  end

  test "partition _all_docs with skip", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_partition/foo/_all_docs"
    resp = Couch.get(url, query: %{skip: 5, limit: 5})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert length(ids) == 5
    assert ids == ["foo:18", "foo:2", "foo:20", "foo:22", "foo:24"]
  end

  test "partition _all_docs with key", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_partition/foo/_all_docs"
    resp = Couch.get(url, query: %{key: "\"foo:22\""})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert length(ids) == 1
    assert ids == ["foo:22"]
  end
end

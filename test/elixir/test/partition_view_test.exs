defmodule ViewPartitionTest do
  use CouchTestCase
  import PartitionHelpers

  @moduledoc """
  Test Partition functionality for views
  """

  def create_reduce_ddoc(db_name, opts \\ %{}) do
    mapFn = """
      function(doc) {
        if (doc.group) {
          emit([doc.some, doc.group], 1);
        }
      }
    """
    default_ddoc = %{
      views: %{
        some: %{
          map: mapFn,
          reduce: "_count"
        }
      }
    }

    ddoc = Enum.into(opts, default_ddoc)

    resp = Couch.put("/#{db_name}/_design/mrtest", body: ddoc)
    assert resp.status_code == 201
    assert Map.has_key?(resp.body, "ok") == true
  end

  def get_reduce_result (resp) do
    %{:body => %{"rows" => rows}} = resp
    rows
  end

  @tag :with_partitioned_db
  test "query with partitioned:true returns partitioned fields", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_partition_ddoc(db_name, options: %{partitioned: true})

    url = "/#{db_name}/_partition/foo/_design/mrtest/_view/some"
    resp = Couch.get(url)
    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) > 0
    assert_correct_partition(partitions, "foo")

    url = "/#{db_name}/_partition/bar/_design/mrtest/_view/some"
    resp = Couch.get(url)
    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) > 0
    assert_correct_partition(partitions, "bar")
  end

  @tag :with_partitioned_db
  test "default view query returns partitioned fields", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_partition_ddoc(db_name)

    url = "/#{db_name}/_partition/foo/_design/mrtest/_view/some"
    resp = Couch.get(url)
    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) > 0
    assert_correct_partition(partitions, "foo")

    url = "/#{db_name}/_partition/bar/_design/mrtest/_view/some"
    resp = Couch.get(url)
    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) > 0
    assert_correct_partition(partitions, "bar")
  end

  @tag :with_partitioned_db
  test "query will not include other docs in shard", context do
    db_name = context[:db_name]
    # bar42 will be put in the same shard as foo for q = 8
    create_partition_docs(db_name, "foo", "bar42")
    create_partition_ddoc(db_name)

    url = "/#{db_name}/_partition/foo/_design/mrtest/_view/some"
    resp = Couch.get(url)
    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) > 0
    assert_correct_partition(partitions, "foo")

    url = "/#{db_name}/_partition/bar42/_design/mrtest/_view/some"
    resp = Couch.get(url)
    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) > 0
    assert_correct_partition(partitions, "bar42")
  end

  @tag :with_partitioned_db
  test "query will return zero results for wrong inputs", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_partition_ddoc(db_name)

    url = "/#{db_name}/_partition/foo/_design/mrtest/_view/some"
    resp = Couch.get(url, query: %{start_key: "\"foo:12\""})
    assert resp.status_code == 200
    assert Map.get(resp, :body)["rows"] == []
  end

  @tag :with_partitioned_db
  test "partitioned ddoc cannot be used in global query", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_partition_ddoc(db_name)

    url = "/#{db_name}/_design/mrtest/_view/some"
    resp = Couch.get(url)
    %{:body => %{"reason" => reason}} = resp
    assert resp.status_code == 400
    assert Regex.match?(~r/mandatory for queries to this view./, reason)
  end

  @tag :with_partitioned_db
  test "partitioned query cannot be used with global ddoc", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_partition_ddoc(db_name, options: %{partitioned: false})

    url = "/#{db_name}/_partition/foo/_design/mrtest/_view/some"
    resp = Couch.get(url)
    %{:body => %{"reason" => reason}} = resp
    assert resp.status_code == 400
    assert Regex.match?(~r/is not supported in this design doc/, reason)
  end

  @tag :with_partitioned_db
  test "view query returns all docs for global query", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_partition_ddoc(db_name, options: %{partitioned: false})

    url = "/#{db_name}/_design/mrtest/_view/some"
    resp = Couch.get(url)
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert length(ids) == 100
  end

  @tag :with_partitioned_db
  test "partition query errors with incorrect partition supplied", context do
    db_name = context[:db_name]
    create_partition_ddoc(db_name)

    url = "/#{db_name}/_partition/_bar/_design/mrtest/_view/some"
    resp = Couch.get(url)
    assert resp.status_code == 400

    url = "/#{db_name}/_partition//_design/mrtest/_view/some"
    resp = Couch.get(url)
    assert resp.status_code == 400
  end

  @tag :with_partitioned_db
  test "partitioned query works with startkey, endkey range", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_partition_ddoc(db_name, options: %{partitioned: true})

    url = "/#{db_name}/_partition/foo/_design/mrtest/_view/some"
    resp = Couch.get(url, query: %{start_key: 12, end_key: 20})
    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 5
    assert_correct_partition(partitions, "foo")
  end

  @tag :with_partitioned_db
  test "partitioned query works with keys", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_partition_ddoc(db_name)

    url = "/#{db_name}/_partition/foo/_design/mrtest/_view/some"
    resp = Couch.post(url, body: %{keys: [2, 4, 6]})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert length(ids) == 3
    assert ids == ["foo:2", "foo:4", "foo:6"]
  end

  @tag :with_partitioned_db
  test "global query works with keys", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_partition_ddoc(db_name, options: %{partitioned: false})

    url = "/#{db_name}/_design/mrtest/_view/some"

    resp = Couch.post(url, body: %{keys: [2, 4, 6]})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert length(ids) == 3
    assert ids == ["foo:2", "foo:4", "foo:6"]
  end

  @tag :with_partitioned_db
  test "partition query works with limit", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_partition_ddoc(db_name)

    url = "/#{db_name}/_partition/foo/_design/mrtest/_view/some"

    resp = Couch.get(url, query: %{limit: 5})
    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 5
    assert_correct_partition(partitions, "foo")
  end

  @tag :with_partitioned_db
  test "partition query with descending", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_partition_ddoc(db_name)

    url = "/#{db_name}/_partition/foo/_design/mrtest/_view/some"

    resp = Couch.get(url, query: %{descending: true, limit: 5})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert length(ids) == 5
    assert ids == ["foo:100", "foo:98", "foo:96", "foo:94", "foo:92"]

    resp = Couch.get(url, query: %{descending: false, limit: 5})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert length(ids) == 5
    assert ids == ["foo:2", "foo:4", "foo:6", "foo:8", "foo:10"]
  end

  @tag :with_partitioned_db
  test "partition query with skip", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_partition_ddoc(db_name)

    url = "/#{db_name}/_partition/foo/_design/mrtest/_view/some"

    resp = Couch.get(url, query: %{skip: 5, limit: 5})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert length(ids) == 5
    assert ids == ["foo:12", "foo:14", "foo:16", "foo:18", "foo:20"]
  end

  @tag :with_partitioned_db
  test "partition query with key", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_partition_ddoc(db_name)

    url = "/#{db_name}/_partition/foo/_design/mrtest/_view/some"

    resp = Couch.get(url, query: %{key: 22})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert length(ids) == 1
    assert ids == ["foo:22"]
  end

  @tag :with_partitioned_db
  test "partition query with startkey_docid and endkey_docid", context do
    mapFn = """
      function(doc) {
        if (doc.some) {
          emit(doc.some);
        }
      }
    """
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_partition_ddoc(db_name, views: %{
      some: %{
        map: mapFn
      }
    })

    url = "/#{db_name}/_partition/foo/_design/mrtest/_view/some"

    resp = Couch.get(url, query: %{
      startkey: "\"field\"",
      endkey: "\"field\"",
      startkey_docid: "foo:18",
      endkey_docid: "foo:24"
    })
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert ids == ["foo:18", "foo:2", "foo:20", "foo:22", "foo:24"]
  end

  @tag :with_partitioned_db
  test "query with update=false works", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_partition_ddoc(db_name)

    url = "/#{db_name}/_partition/foo/_design/mrtest/_view/some"
    resp = Couch.get(url, query: %{
      update: "true",
      limit: 3
    })
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert ids == ["foo:2", "foo:4", "foo:6"]

    # Avoid race conditions by attempting to get a full response
    # from every shard before we do our update:false test
    for i <- 1..12 do
      resp = Couch.get(url)
      assert resp.status_code == 200
    end

    Couch.put("/#{db_name}/foo:1", body: %{some: "field"})

    resp = Couch.get(url, query: %{
      update: "false",
      limit: 3
    })
    Logger.debug "#{inspect resp}"
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert ids == ["foo:2", "foo:4", "foo:6"]
  end

  @tag :with_partitioned_db
  test "query with reduce works", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_reduce_ddoc(db_name)

    url = "/#{db_name}/_partition/foo/_design/mrtest/_view/some"
    resp = Couch.get(url, query: %{
      reduce: true,
      group_level: 1,
    })

    assert resp.status_code == 200
    results = get_reduce_result(resp)
    assert results == [%{"key" => ["field"], "value" => 50}]

    resp = Couch.get(url, query: %{
      reduce: true,
      group_level: 2
    })

    results = get_reduce_result(resp)
    assert results == [
      %{"key" => ["field", "one"], "value" => 16},
      %{"key" => ["field", "two"], "value" => 34}
    ]

    resp = Couch.get(url, query: %{
      reduce: true,
      group: true
    })

    results = get_reduce_result(resp)
    assert results == [
      %{"key" => ["field", "one"], "value" => 16},
      %{"key" => ["field", "two"], "value" => 34}
    ]
  end
end

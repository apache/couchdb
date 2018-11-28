defmodule ViewPartitionTest do
  use CouchTestCase
  import PartitionHelpers

  @moduledoc """
  Test Partition functionality for views
  """

  setup_all do
    db_name = random_db_name()
    {:ok, _} = create_db(db_name, query: %{partitioned: true, q: 1})
    on_exit(fn -> delete_db(db_name) end)

    create_partition_docs(db_name)

    map_fun1 = """
      function(doc) {
        if (doc.some) {
          emit(doc.value, doc.some);
        }
      }
    """

    map_fun2 = """
      function(doc) {
        if (doc.group) {
          emit([doc.some, doc.group], 1);
        }
      }
    """

    query = %{:w => 3}

    body = %{
      :docs => [
        %{
          _id: "_design/map",
          views: %{some: %{map: map_fun1}}
        },
        %{
          _id: "_design/map_some",
          views: %{some: %{map: map_fun2}}
        },
        %{
          _id: "_design/partitioned_true",
          views: %{some: %{map: map_fun1}},
          options: %{partitioned: true}
        },
        %{
          _id: "_design/partitioned_false",
          views: %{some: %{map: map_fun1}},
          options: %{partitioned: false}
        },
        %{
          _id: "_design/reduce",
          views: %{some: %{map: map_fun2, reduce: "_count"}}
        },
        %{
          _id: "_design/include_ddocs",
          views: %{some: %{map: map_fun1}},
          options: %{include_design: true}
        }
      ]
    }

    resp = Couch.post("/#{db_name}/_bulk_docs", query: query, body: body)
    Enum.each(resp.body, &assert(&1["ok"]))

    {:ok, [db_name: db_name]}
  end

  def get_reduce_result(resp) do
    %{:body => %{"rows" => rows}} = resp
    rows
  end

  test "query with partitioned:true returns partitioned fields", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_partition/foo/_design/partitioned_true/_view/some"
    resp = Couch.get(url)
    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert Enum.dedup(partitions) == ["foo"]

    url = "/#{db_name}/_partition/bar/_design/partitioned_true/_view/some"
    resp = Couch.get(url)
    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert Enum.dedup(partitions) == ["bar"]
  end

  test "default view query returns partitioned fields", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_partition/foo/_design/map/_view/some"
    resp = Couch.get(url)
    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert Enum.dedup(partitions) == ["foo"]

    url = "/#{db_name}/_partition/bar/_design/map/_view/some"
    resp = Couch.get(url)
    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert Enum.dedup(partitions) == ["bar"]
  end

  test "query will return zero results for wrong inputs", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_partition/foo/_design/map/_view/some"
    resp = Couch.get(url, query: %{start_key: "\"foo:12\""})
    assert resp.status_code == 200
    assert Map.get(resp, :body)["rows"] == []
  end

  test "partitioned ddoc cannot be used in global query", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_design/map/_view/some"
    resp = Couch.get(url)
    %{:body => %{"reason" => reason}} = resp
    assert resp.status_code == 400
    assert Regex.match?(~r/mandatory for queries to this view./, reason)
  end

  test "partitioned query cannot be used with global ddoc", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_partition/foo/_design/partitioned_false/_view/some"
    resp = Couch.get(url)
    %{:body => %{"reason" => reason}} = resp
    assert resp.status_code == 400
    assert Regex.match?(~r/is not supported in this design doc/, reason)
  end

  test "view query returns all docs for global query", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_design/partitioned_false/_view/some"
    resp = Couch.get(url)
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert length(ids) == 100
  end

  test "partition query errors with incorrect partition supplied", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_partition/_bar/_design/map/_view/some"
    resp = Couch.get(url)
    assert resp.status_code == 400

    url = "/#{db_name}/_partition//_design/map/_view/some"
    resp = Couch.get(url)
    assert resp.status_code == 400
  end

  test "partitioned query works with startkey, endkey range", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_partition/foo/_design/map/_view/some"
    resp = Couch.get(url, query: %{start_key: 12, end_key: 20})
    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 5
    assert Enum.dedup(partitions) == ["foo"]
  end

  test "partitioned query works with keys", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_partition/foo/_design/map/_view/some"
    resp = Couch.post(url, body: %{keys: [2, 4, 6]})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert length(ids) == 3
    assert ids == ["foo:2", "foo:4", "foo:6"]
  end

  test "global query works with keys", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_design/partitioned_false/_view/some"
    resp = Couch.post(url, body: %{keys: [2, 4, 6]})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert length(ids) == 3
    assert ids == ["foo:2", "foo:4", "foo:6"]
  end

  test "partition query works with limit", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_partition/foo/_design/map/_view/some"
    resp = Couch.get(url, query: %{limit: 5})
    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 5
    assert Enum.dedup(partitions) == ["foo"]
  end

  test "partition query with descending", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_partition/foo/_design/map/_view/some"
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

  test "partition query with skip", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_partition/foo/_design/map/_view/some"
    resp = Couch.get(url, query: %{skip: 5, limit: 5})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert length(ids) == 5
    assert ids == ["foo:12", "foo:14", "foo:16", "foo:18", "foo:20"]
  end

  test "partition query with key", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_partition/foo/_design/map/_view/some"
    resp = Couch.get(url, query: %{key: 22})
    assert resp.status_code == 200
    ids = get_ids(resp)
    assert length(ids) == 1
    assert ids == ["foo:22"]
  end

  test "partition query with startkey_docid and endkey_docid", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_partition/foo/_design/map_some/_view/some"

    resp =
      Couch.get(url,
        query: %{
          startkey: "[\"field\",\"one\"]",
          endkey: "[\"field\",\"one\"]",
          startkey_docid: "foo:12",
          endkey_docid: "foo:30"
        }
      )

    assert resp.status_code == 200
    ids = get_ids(resp)
    assert ids == ["foo:12", "foo:18", "foo:24", "foo:30"]
  end

  test "query with reduce works", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_partition/foo/_design/reduce/_view/some"
    resp = Couch.get(url, query: %{reduce: true, group_level: 1})
    assert resp.status_code == 200
    results = get_reduce_result(resp)
    assert results == [%{"key" => ["field"], "value" => 50}]

    resp = Couch.get(url, query: %{reduce: true, group_level: 2})
    results = get_reduce_result(resp)

    assert results == [
             %{"key" => ["field", "one"], "value" => 16},
             %{"key" => ["field", "two"], "value" => 34}
           ]

    resp = Couch.get(url, query: %{reduce: true, group: true})
    results = get_reduce_result(resp)

    assert results == [
             %{"key" => ["field", "one"], "value" => 16},
             %{"key" => ["field", "two"], "value" => 34}
           ]
  end

  test "include_design works correctly", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_partition/foo/_design/include_ddocs/_view/some"
    resp = Couch.get(url)
    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 50
    assert Enum.dedup(partitions) == ["foo"]
  end
end

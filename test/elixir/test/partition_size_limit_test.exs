defmodule PartitionSizeLimitTest do
  use CouchTestCase

  @moduledoc """
  Test Partition size limit functionality
  """

  @max_size 10_240

  setup do
    db_name = random_db_name()
    {:ok, _} = create_db(db_name, query: %{partitioned: true, q: 1})
    on_exit(fn -> delete_db(db_name) end)

    set_config({"couchdb", "max_partition_size", Integer.to_string(@max_size)})

    {:ok, [db_name: db_name]}
  end

  defp get_db_info(dbname) do
    resp = Couch.get("/#{dbname}")
    assert resp.status_code in [200, 202]
    %{:body => body} = resp
    body
  end

  defp get_partition_info(dbname, partition) do
    resp = Couch.get("/#{dbname}/_partition/#{partition}")
    assert resp.status_code in [200, 202]
    %{:body => body} = resp
    body
  end

  defp open_doc(db_name, docid, status_assert \\ [200, 202]) do
    resp = Couch.get("/#{db_name}/#{docid}")
    assert resp.status_code in status_assert
    %{:body => body} = resp
    body
  end

  defp save_doc(db_name, doc, status_assert \\ [201, 202]) do
    resp = Couch.post("/#{db_name}", query: [w: 3], body: doc)
    assert resp.status_code in status_assert
    %{:body => body} = resp
    body["rev"]
  end

  defp delete_doc(db_name, doc, status_assert \\ [200, 202]) do
    url = "/#{db_name}/#{doc["_id"]}"
    rev = doc["_rev"]
    resp = Couch.delete(url, query: [w: 3, rev: rev])
    assert resp.status_code in status_assert
    %{:body => body} = resp
    body["rev"]
  end

  defp fill_partition(db_name, partition \\ "foo") do
    docs =
      1..15
      |> Enum.map(fn i ->
        id = i |> Integer.to_string() |> String.pad_leading(4, "0")
        docid = "#{partition}:#{id}"
        %{_id: docid, value: "0" |> String.pad_leading(1024)}
      end)

    body = %{:w => 3, :docs => docs}
    resp = Couch.post("/#{db_name}/_bulk_docs", body: body)
    assert resp.status_code in [201, 202]
  end

  defp compact(db) do
    assert Couch.post("/#{db}/_compact").status_code == 202

    retry_until(
      fn ->
        Couch.get("/#{db}").body["compact_running"] == false
      end,
      200,
      20_000
    )
  end

  test "fill partition manually", context do
    db_name = context[:db_name]
    partition = "foo"

    resp =
      1..1000
      |> Enum.find_value(0, fn i ->
        id = i |> Integer.to_string() |> String.pad_leading(4, "0")
        docid = "#{partition}:#{id}"
        doc = %{_id: docid, value: "0" |> String.pad_leading(1024)}
        resp = Couch.post("/#{db_name}", query: [w: 3], body: doc)

        if resp.status_code in [201, 202] do
          false
        else
          resp
        end
      end)

    assert resp.status_code == 403
    %{body: body} = resp
    assert body["error"] == "partition_overflow"

    info = get_partition_info(db_name, partition)
    assert info["sizes"]["external"] >= @max_size
  end

  test "full partitions reject POST /dbname", context do
    db_name = context[:db_name]
    fill_partition(db_name)

    doc = %{_id: "foo:bar", value: "stuff"}
    resp = Couch.post("/#{db_name}", query: [w: 3], body: doc)
    assert resp.status_code == 403
    %{body: body} = resp
    assert body["error"] == "partition_overflow"
  end

  test "full partitions reject PUT /dbname/docid", context do
    db_name = context[:db_name]
    fill_partition(db_name)

    doc = %{value: "stuff"}
    resp = Couch.put("/#{db_name}/foo:bar", query: [w: 3], body: doc)
    assert resp.status_code == 403
    %{body: body} = resp
    assert body["error"] == "partition_overflow"
  end

  test "full partitions reject POST /dbname/_bulk_docs", context do
    db_name = context[:db_name]
    fill_partition(db_name)

    body = %{w: 3, docs: [%{_id: "foo:bar"}]}
    resp = Couch.post("/#{db_name}/_bulk_docs", query: [w: 3], body: body)
    assert resp.status_code in [201, 202]
    %{body: body} = resp
    doc_resp = Enum.at(body, 0)
    assert doc_resp["error"] == "partition_overflow"
  end

  test "full partitions with mixed POST /dbname/_bulk_docs", context do
    db_name = context[:db_name]
    fill_partition(db_name)

    body = %{w: 3, docs: [%{_id: "foo:bar"}, %{_id: "baz:bang"}]}
    resp = Couch.post("/#{db_name}/_bulk_docs", query: [w: 3], body: body)
    assert resp.status_code in [201, 202]
    %{body: body} = resp

    doc_resp1 = Enum.at(body, 0)
    assert doc_resp1["error"] == "partition_overflow"

    doc_resp2 = Enum.at(body, 1)
    assert doc_resp2["ok"]
  end

  test "full partitions are still readable", context do
    db_name = context[:db_name]
    fill_partition(db_name)
    open_doc(db_name, "foo:0001")
  end

  test "full partitions can accept deletes", context do
    db_name = context[:db_name]
    fill_partition(db_name)

    doc = open_doc(db_name, "foo:0001")
    delete_doc(db_name, doc)
  end

  test "full partitions can accept updates that reduce size", context do
    db_name = context[:db_name]
    fill_partition(db_name)

    doc = open_doc(db_name, "foo:0001")
    save_doc(db_name, %{doc | "value" => ""})
  end

  test "full partition does not affect other partitions", context do
    db_name = context[:db_name]
    fill_partition(db_name)
    save_doc(db_name, %{_id: "bar:foo", value: "stuff"})
  end

  test "full partition does not affect design documents", context do
    db_name = context[:db_name]
    fill_partition(db_name)
    rev1 = save_doc(db_name, %{_id: "_design/foo", value: "stuff"})
    save_doc(db_name, %{_id: "_design/foo", _rev: rev1, value: "hi"})
    doc = open_doc(db_name, "_design/foo")
    delete_doc(db_name, doc)
  end

  test "replication into a full partition works", context do
    db_name = context[:db_name]
    fill_partition(db_name)
    save_doc(db_name, %{_id: "foo:bar", value: "stuff"}, [403])

    doc = %{
      _id: "foo:bar",
      _rev: <<"1-23202479633c2b380f79507a776743d5">>,
      value: "stuff"
    }

    url = "/#{db_name}/#{doc[:_id]}"
    query = [new_edits: false, w: 3]
    resp = Couch.put(url, query: query, body: doc)
    assert resp.status_code in [201, 202]
  end

  test "compacting a full partition works", context do
    db_name = context[:db_name]
    db_info1 = get_db_info(db_name)
    fill_partition(db_name)
    compact(db_name)
    db_info2 = get_db_info(db_name)
    assert db_info2["sizes"]["file"] != db_info1["sizes"]["file"]
  end

  test "indexing a full partition works", context do
    db_name = context[:db_name]
    fill_partition(db_name)

    ddoc = %{
      _id: "_design/foo",
      views: %{
        bar: %{
          map: "function(doc) {emit(doc.group, 1);}"
        }
      }
    }

    save_doc(db_name, ddoc)

    url = "/#{db_name}/_partition/foo/_design/foo/_view/bar"
    resp = Couch.get(url)
    assert resp.status_code in [200, 202]
    %{body: body} = resp

    assert length(body["rows"]) > 0
  end

  test "purging docs allows writes", context do
    db_name = context[:db_name]
    fill_partition(db_name)

    info = get_partition_info(db_name, "foo")
    limit = info["doc_count"] - 1

    query = [
      start_key: "\"foo:0000\"",
      end_key: "\"foo:9999\"",
      limit: limit
    ]

    resp = Couch.get("/#{db_name}/_all_docs", query: query)
    assert resp.status_code in [200, 202]
    %{body: body} = resp

    pbody =
      body["rows"]
      |> Enum.reduce(%{}, fn row, acc ->
        Map.put(acc, row["id"], [row["value"]["rev"]])
      end)

    resp = Couch.post("/#{db_name}/_purge", query: [w: 3], body: pbody)
    assert resp.status_code in [201, 202]

    save_doc(db_name, %{_id: "foo:bar", value: "some value"})
  end

  test "increasing partition size allows more writes", context do
    db_name = context[:db_name]
    fill_partition(db_name)

    # We use set_config_raw so that we're not setting
    # on_exit handlers that might interfere with the original
    # config change done in setup of this test
    new_size = Integer.to_string(@max_size * 1000)
    set_config_raw("couchdb", "max_partition_size", new_size)

    save_doc(db_name, %{_id: "foo:bar", value: "stuff"})
  end

  test "decreasing partition size disables more writes", context do
    db_name = context[:db_name]

    # We use set_config_raw so that we're not setting
    # on_exit handlers that might interfere with the original
    # config change done in setup of this test
    new_size = Integer.to_string(@max_size * 1000)
    set_config_raw("couchdb", "max_partition_size", new_size)

    fill_partition(db_name)
    save_doc(db_name, %{_id: "foo:bar", value: "stuff"})

    old_size = Integer.to_string(@max_size)
    set_config_raw("couchdb", "max_partition_size", old_size)

    save_doc(db_name, %{_id: "foo:baz", value: "stuff"}, [403])
  end
end

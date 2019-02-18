defmodule PartitionSizeTest do
  use CouchTestCase

  @moduledoc """
  Test Partition size functionality
  """

  setup do
    db_name = random_db_name()
    {:ok, _} = create_db(db_name, query: %{partitioned: true, q: 1})
    on_exit(fn -> delete_db(db_name) end)

    {:ok, [db_name: db_name]}
  end

  def get_db_info(dbname) do
    resp = Couch.get("/#{dbname}")
    assert resp.status_code == 200
    %{:body => body} = resp
    body
  end

  def get_partition_info(dbname, partition) do
    resp = Couch.get("/#{dbname}/_partition/#{partition}")
    assert resp.status_code == 200
    %{:body => body} = resp
    body
  end

  def mk_partition(i) do
    i |> rem(10) |> Integer.to_string() |> String.pad_leading(3, "0")
  end

  def mk_docid(i) do
    id = i |> Integer.to_string() |> String.pad_leading(4, "0")
    "#{mk_partition(i)}:#{id}"
  end

  def mk_docs(db_name) do
    docs =
      for i <- 1..1000 do
        group = Integer.to_string(rem(i, 3))

        %{
          :_id => mk_docid(i),
          :value => i,
          :some => "field",
          :group => group
        }
      end

    body = %{:w => 3, :docs => docs}
    resp = Couch.post("/#{db_name}/_bulk_docs", body: body)
    assert resp.status_code == 201
  end

  def save_doc(db_name, doc) do
    resp = Couch.post("/#{db_name}", query: [w: 3], body: doc)
    assert resp.status_code == 201
    %{:body => body} = resp
    body["rev"]
  end

  test "get empty partition", context do
    db_name = context[:db_name]
    partition = "non_existent_partition"

    info = get_partition_info(db_name, partition)

    assert info["doc_count"] == 0
    assert info["doc_del_count"] == 0
    assert info["partition"] == partition
    assert info["sizes"]["external"] == 0
    assert info["sizes"]["active"] == 0
  end

  test "unknown partition return's zero", context do
    db_name = context[:db_name]
    mk_docs(db_name)

    info = get_partition_info(db_name, "unknown")
    assert info["doc_count"] == 0
    assert info["doc_del_count"] == 0
    assert info["sizes"]["external"] == 0
    assert info["sizes"]["active"] == 0
  end

  test "simple partition size", context do
    db_name = context[:db_name]
    save_doc(db_name, %{_id: "foo:bar", val: 42})

    info = get_partition_info(db_name, "foo")
    assert info["doc_count"] == 1
    assert info["doc_del_count"] == 0
    assert info["sizes"]["external"] > 0
    assert info["sizes"]["active"] > 0
  end

  test "adding docs increases partition sizes", context do
    db_name = context[:db_name]
    save_doc(db_name, %{_id: "foo:bar", val: 42})
    pre_info = get_partition_info(db_name, "foo")

    save_doc(db_name, %{_id: "foo:baz", val: 24})
    post_info = get_partition_info(db_name, "foo")

    assert post_info["doc_count"] == 2
    assert post_info["doc_del_count"] == 0
    assert post_info["sizes"]["external"] > pre_info["sizes"]["external"]
    assert post_info["sizes"]["active"] > pre_info["sizes"]["active"]
  end

  test "updating docs affects partition sizes", context do
    db_name = context[:db_name]
    rev1 = save_doc(db_name, %{_id: "foo:bar", val: ""})
    info1 = get_partition_info(db_name, "foo")

    rev2 =
      save_doc(db_name, %{
        _id: "foo:bar",
        _rev: rev1,
        val: "this is a very long string that is so super long its beyond long"
      })

    info2 = get_partition_info(db_name, "foo")

    save_doc(db_name, %{
      _id: "foo:bar",
      _rev: rev2,
      val: "this string is shorter"
    })

    info3 = get_partition_info(db_name, "foo")

    assert info3["doc_count"] == 1
    assert info3["doc_del_count"] == 0

    assert info3["sizes"]["external"] > info1["sizes"]["external"]
    assert info2["sizes"]["external"] > info3["sizes"]["external"]
  end

  test "deleting a doc affects partition sizes", context do
    db_name = context[:db_name]
    rev1 = save_doc(db_name, %{_id: "foo:bar", val: "some stuff here"})
    info1 = get_partition_info(db_name, "foo")

    save_doc(db_name, %{_id: "foo:bar", _rev: rev1, _deleted: true})
    info2 = get_partition_info(db_name, "foo")

    assert info1["doc_count"] == 1
    assert info1["doc_del_count"] == 0

    assert info2["doc_count"] == 0
    assert info2["doc_del_count"] == 1

    assert info2["sizes"]["external"] < info1["sizes"]["external"]
  end

  test "design docs do not affect partition sizes", context do
    db_name = context[:db_name]
    mk_docs(db_name)

    pre_infos =
      0..9
      |> Enum.map(fn i ->
        get_partition_info(db_name, mk_partition(i))
      end)

    0..5
    |> Enum.map(fn i ->
      base = i |> Integer.to_string() |> String.pad_leading(5, "0")
      docid = "_design/#{base}"
      save_doc(db_name, %{_id: docid, value: "some stuff here"})
    end)

    post_infos =
      0..9
      |> Enum.map(fn i ->
        get_partition_info(db_name, mk_partition(i))
      end)

    assert post_infos == pre_infos
  end

  @tag :skip_on_jenkins
  test "get all partition sizes", context do
    db_name = context[:db_name]
    mk_docs(db_name)

    {esum, asum} =
      0..9
      |> Enum.reduce({0, 0}, fn i, {esize, asize} ->
        partition = mk_partition(i)
        info = get_partition_info(db_name, partition)
        assert info["doc_count"] == 100
        assert info["doc_del_count"] == 0
        assert info["sizes"]["external"] > 0
        assert info["sizes"]["active"] > 0
        {esize + info["sizes"]["external"], asize + info["sizes"]["active"]}
      end)

    db_info = get_db_info(db_name)
    assert db_info["sizes"]["external"] >= esum
    assert db_info["sizes"]["active"] >= asum
  end

  test "get partition size with attachment", context do
    db_name = context[:db_name]

    doc = %{
      _id: "foo:doc-with-attachment",
      _attachments: %{
        "foo.txt": %{
          content_type: "text/plain",
          data: Base.encode64("This is a text document to save")
        }
      }
    }

    save_doc(db_name, doc)

    db_info = get_db_info(db_name)
    foo_info = get_partition_info(db_name, "foo")

    assert foo_info["doc_count"] == 1
    assert foo_info["doc_del_count"] == 0
    assert foo_info["sizes"]["active"] > 0
    assert foo_info["sizes"]["external"] > 0

    assert foo_info["sizes"]["active"] <= db_info["sizes"]["active"]
    assert foo_info["sizes"]["external"] <= db_info["sizes"]["external"]
  end

  test "attachments don't affect other partitions", context do
    db_name = context[:db_name]
    mk_docs(db_name)

    pre_infos =
      0..9
      |> Enum.map(fn i ->
        get_partition_info(db_name, mk_partition(i))
      end)

    doc = %{
      _id: "foo:doc-with-attachment",
      _attachments: %{
        "foo.txt": %{
          content_type: "text/plain",
          data: Base.encode64("This is a text document to save")
        }
      }
    }

    save_doc(db_name, doc)

    att_info = get_partition_info(db_name, "foo")
    assert att_info["doc_count"] == 1
    assert att_info["sizes"]["external"] > 0

    post_infos =
      0..9
      |> Enum.map(fn i ->
        get_partition_info(db_name, mk_partition(i))
      end)

    assert post_infos == pre_infos

    esize =
      ([att_info] ++ post_infos)
      |> Enum.reduce(0, fn info, acc ->
        info["sizes"]["external"] + acc
      end)

    db_info = get_db_info(db_name)
    assert esize == db_info["sizes"]["external"]
  end

  test "partition activity not affect other partition sizes", context do
    db_name = context[:db_name]
    mk_docs(db_name)

    partition1 = "000"
    partition2 = "001"

    info2 = get_partition_info(db_name, partition2)

    doc_id = "#{partition1}:doc-with-attachment"

    doc = %{
      _id: doc_id,
      _attachments: %{
        "foo.txt": %{
          content_type: "text/plain",
          data: Base.encode64("This is a text document to save")
        }
      }
    }

    doc_rev = save_doc(db_name, doc)

    info2_attach = get_partition_info(db_name, partition2)
    assert info2_attach == info2

    doc =
      Enum.into(
        %{
          another: "add another field",
          _rev: doc_rev
        },
        doc
      )

    doc_rev = save_doc(db_name, doc)

    info2_update = get_partition_info(db_name, partition2)
    assert info2_update == info2

    resp = Couch.delete("/#{db_name}/#{doc_id}", query: %{rev: doc_rev})
    assert resp.status_code == 200

    info2_delete = get_partition_info(db_name, partition2)
    assert info2_delete == info2
  end

  test "purging docs decreases partition size", context do
    db_name = context[:db_name]
    mk_docs(db_name)

    partition = "000"

    query = [
      start_key: "\"#{partition}:0000\"",
      end_key: "\"#{partition}:9999\"",
      limit: 50
    ]

    resp = Couch.get("/#{db_name}/_all_docs", query: query)
    assert resp.status_code == 200
    %{body: body} = resp

    pre_info = get_partition_info(db_name, partition)

    pbody =
      body["rows"]
      |> Enum.reduce(%{}, fn row, acc ->
        Map.put(acc, row["id"], [row["value"]["rev"]])
      end)

    resp = Couch.post("/#{db_name}/_purge", query: [w: 3], body: pbody)
    assert resp.status_code == 201

    post_info = get_partition_info(db_name, partition)
    assert post_info["doc_count"] == pre_info["doc_count"] - 50
    assert post_info["doc_del_count"] == 0
    assert post_info["sizes"]["active"] < pre_info["sizes"]["active"]
    assert post_info["sizes"]["external"] < pre_info["sizes"]["external"]
  end
end

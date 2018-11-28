defmodule PartitionSizeTest do
  use CouchTestCase
  import PartitionHelpers

  @moduledoc """
  Test Partition size functionality
  """
  @tag :with_partitioned_db
  test "get partition size", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)

    infoResp = Couch.get("/#{db_name}")
    %{:body => info} = infoResp
    externalSize = info["sizes"]["external"]

    url = "/#{db_name}/_partition/foo"
    resp = Couch.get(url)

    assert resp.status_code == 200
    %{:body => body} = resp
    assert body["doc_count"] == 50
    assert body["partition"] == "foo"
    assert body["sizes"]["external"] == externalSize / 2

    url = "/#{db_name}/_partition/bar"
    resp = Couch.get(url)

    assert resp.status_code == 200
    %{:body => body} = resp
    assert body["doc_count"] == 50
    assert body["partition"] == "bar"
    assert body["sizes"]["external"] == externalSize / 2
  end

  @tag :with_partitioned_db
  test "get partition size for two partitions on same shard", context do
    db_name = context[:db_name]
    create_partition_docs(db_name, "foo", "bar42")

    infoResp = Couch.get("/#{db_name}")
    %{:body => info} = infoResp
    externalSize = info["sizes"]["external"]

    url = "/#{db_name}/_partition/foo"
    resp = Couch.get(url)

    assert resp.status_code == 200
    %{:body => body} = resp
    assert body["doc_count"] == 50
    assert body["partition"] == "foo"
    assert body["sizes"]["external"] == externalSize / 2

    url = "/#{db_name}/_partition/bar42"
    resp = Couch.get(url)

    assert resp.status_code == 200
    %{:body => body} = resp
    assert body["doc_count"] == 50
    assert body["partition"] == "bar42"
    assert body["sizes"]["external"] == externalSize / 2
  end

  @tag :with_partitioned_db
  test "get partition size with attachment", context do
    db_name = context[:db_name]
    # create_partition_docs(db_name, "foo", "bar42")

    id = "foo:doc-with-attachment"

    doc = %{
      _id: id,
      _attachments: %{
        "foo.txt": %{
          content_type: "text/plain",
          data: Base.encode64("This is a text document to save")
        }
      }
    }
    Couch.put("/#{db_name}/#{id}", [body: doc])

    infoResp = Couch.get("/#{db_name}")
    %{:body => info} = infoResp
    externalSize = info["sizes"]["external"]

    url = "/#{db_name}/_partition/foo"
    resp = Couch.get(url)

    assert resp.status_code == 200
    %{:body => body} = resp
    assert body["doc_count"] == 1
    assert body["partition"] == "foo"
    assert body["sizes"]["external"] == externalSize
  end

  @tag :with_partitioned_db
  test "get multiple partition sizes with attachment", context do
    db_name = context[:db_name]
    create_partition_docs(db_name, "foo", "bar42")

    id = "foo:doc-with-attachment"

    doc = %{
      _id: id,
      _attachments: %{
        "foo.txt": %{
          content_type: "text/plain",
          data: Base.encode64("This is a text document to save")
        }
      }
    }
    Couch.put("/#{db_name}/#{id}", [body: doc])

    url = "/#{db_name}/_partition/foo"
    resp = Couch.get(url)

    assert resp.status_code == 200
    %{:body => body} = resp
    assert body["doc_count"] == 51
    assert body["partition"] == "foo"
    #hard coding in values here. Not sure if this is a good idea long term
    assert body["sizes"]["external"] == 4503

    url = "/#{db_name}/_partition/bar42"
    resp = Couch.get(url)

    assert resp.status_code == 200
    %{:body => body} = resp
    assert body["doc_count"] == 50
    assert body["partition"] == "bar42"
    assert body["sizes"]["external"] == 4450
  end
end

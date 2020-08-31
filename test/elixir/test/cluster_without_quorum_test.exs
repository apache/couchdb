defmodule WithoutQuorumTest do
  use CouchTestCase

  @moduletag :without_quorum_test
  @moduletag kind: :degraded_cluster

  @moduledoc """
  Test CouchDB API in a cluster without quorum.
  """
  @tag :with_db_name
  test "Creating/Deleting DB should return 202-Acepted", context do
    db_name = context[:db_name]
    resp = Couch.put("/#{db_name}")
    msg = "Should return 202-Acepted"
    assert resp.status_code == 202, msg
    resp = Couch.delete("/#{db_name}")
    assert resp.status_code == 202, msg
  end

  @tag :with_db_name
  test "Creating/Updating/Deleting doc should return 202-Acepted", context do
    db_name = context[:db_name]
    Couch.put("/#{db_name}")

    resp = Couch.post("/#{context[:db_name]}", body: %{:_id => "0", :a => 1})
    msg = "Should return 202-Acepted"
    assert resp.status_code == 202, msg

    resp = Couch.get("/#{context[:db_name]}/0")
    rev = resp.body["_rev"]

    resp =
      Couch.put("/#{context[:db_name]}/0", body: %{:_id => "0", :_rev => rev, :a => 2})

    msg = "Should return 202-Acepted"
    assert resp.status_code == 202, msg

    resp = Couch.get("/#{context[:db_name]}/0")
    rev = resp.body["_rev"]
    resp = Couch.delete("/#{context[:db_name]}/0", query: %{:rev => rev})
    msg = "Should return 202-Acepted"
    assert resp.status_code == 202, msg

    Couch.delete("/#{db_name}")
  end

  @tag :with_db_name
  test "Creating-Updating/Deleting doc with overriden quorum should return 201-Created/200-OK",
       context do
    db_name = context[:db_name]
    Couch.put("/#{db_name}")

    resp =
      Couch.post(
        "/#{context[:db_name]}",
        query: %{:w => 1},
        body: %{:_id => "0", :a => 1}
      )

    msg = "Should return 201-Created"
    assert resp.status_code in [201, 202], msg

    resp = Couch.get("/#{context[:db_name]}/0")
    rev = resp.body["_rev"]

    resp =
      Couch.put(
        "/#{context[:db_name]}/0",
        query: %{:w => 1},
        body: %{:_id => "0", :_rev => rev, :a => 2}
      )

    msg = "Should return 201-Created"
    assert resp.status_code in [201, 202], msg

    resp = Couch.get("/#{context[:db_name]}/0")
    rev = resp.body["_rev"]
    resp = Couch.delete("/#{context[:db_name]}/0", query: %{:w => 1, :rev => rev})
    msg = "Should return 200-Ok"
    assert resp.status_code == 200, msg

    Couch.delete("/#{db_name}")
  end

  @tag :with_db_name
  test "Copy doc should return 202-Acepted", context do
    db_name = context[:db_name]
    Couch.put("/#{db_name}")

    Couch.post(
      "/#{context[:db_name]}",
      body: %{:_id => "0", :a => 1}
    )

    headers = [Destination: "1"]
    resp = Couch.request(:copy, "/#{context[:db_name]}/0", headers: headers)
    msg = "Should return 202-Acepted"
    assert resp.status_code == 202, msg
    Couch.delete("/#{db_name}")
  end

  @doc_range 1..5

  @tag :with_db_name
  test "Bulk docs should return 202-Acepted", context do
    db_name = context[:db_name]
    Couch.put("/#{db_name}")
    docs = create_docs(@doc_range)
    resp = Couch.post("/#{db_name}/_bulk_docs", body: %{docs: docs})
    msg = "Should return 202-Acepted"
    assert resp.status_code == 202, msg

    Couch.delete("/#{db_name}")
  end

  @tag :with_db_name
  test "Bulk docs overriden quorum should return 201-Created", context do
    db_name = context[:db_name]
    Couch.put("/#{db_name}")
    docs = create_docs(@doc_range)
    resp = Couch.post("/#{db_name}/_bulk_docs", query: %{:w => 1}, body: %{docs: docs})
    msg = "Should return 201-Created"
    assert resp.status_code in [201, 202], msg

    Couch.delete("/#{db_name}")
  end

  @tag :with_db_name
  test "Attachments should return 202-Acepted", context do
    db_name = context[:db_name]
    Couch.put("/#{db_name}")
    resp = Couch.post("/#{context[:db_name]}", body: %{:_id => "0"})
    rev = resp.body["rev"]

    resp =
      Couch.put(
        "/#{context[:db_name]}/0/foo.txt",
        query: %{:rev => rev},
        body: "This is a no bas64 encoded text",
        headers: ["Content-Type": "text/plain;charset=utf-8"]
      )

    msg = "Should return 202-Acepted"
    assert resp.status_code == 202, msg

    rev = resp.body["rev"]
    resp = Couch.delete("/#{context[:db_name]}/0/foo.txt", query: %{:rev => rev})
    msg = "Should return 200-Ok"
    assert resp.status_code == 200, msg

    Couch.delete("/#{db_name}")
  end

  @tag :with_db_name
  test "Attachments overriden quorum should return 201-Created", context do
    db_name = context[:db_name]
    Couch.put("/#{db_name}")
    resp = Couch.post("/#{context[:db_name]}", body: %{:_id => "0"})
    rev = resp.body["rev"]

    resp =
      Couch.put(
        "/#{context[:db_name]}/0/foo.txt",
        query: %{:rev => rev, :w => 1},
        body: "This is a no bas64 encoded text",
        headers: ["Content-Type": "text/plain;charset=utf-8"]
      )

    msg = "Should return 201-Created"
    assert resp.status_code in [201, 202], msg

    rev = resp.body["rev"]

    resp =
      Couch.delete(
        "/#{context[:db_name]}/0/foo.txt",
        query: %{:rev => rev, :w => 1}
      )

    msg = "Should return 200-Ok"
    assert resp.status_code == 200, msg

    Couch.delete("/#{db_name}")
  end
end

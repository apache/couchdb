defmodule PartitionViewUpdateTest do
  use CouchTestCase
  import PartitionHelpers

  @moduledoc """
  Test Partition view update functionality
  """
  @tag :with_partitioned_db
  test "view updates properly remove old keys", context do
    db_name = context[:db_name]
    create_partition_docs(db_name, "foo", "bar")
    create_partition_ddoc(db_name)

    check_key = fn key, num_rows ->
      url = "/#{db_name}/_partition/foo/_design/mrtest/_view/some"
      resp = Couch.get(url, query: [key: key])
      assert resp.status_code == 200
      assert length(resp.body["rows"]) == num_rows
    end

    check_key.(2, 1)

    resp = Couch.get("/#{db_name}/foo:2")
    doc = Map.put(resp.body, "value", 4)
    resp = Couch.put("/#{db_name}/foo:2", query: [w: 3], body: doc)
    assert resp.status_code >= 201 and resp.status_code <= 202

    check_key.(4, 2)
    check_key.(2, 0)
  end

  @tag :skip_on_jenkins
  @tag :with_partitioned_db
  test "query with update=false works", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_partition_ddoc(db_name)

    url = "/#{db_name}/_partition/foo/_design/mrtest/_view/some"

    resp =
      Couch.get(
        url,
        query: %{
          update: "true",
          limit: 3
        }
      )

    assert resp.status_code == 200
    ids = get_ids(resp)
    assert ids == ["foo:2", "foo:4", "foo:6"]

    # Avoid race conditions by attempting to get a full response
    # from every shard before we do our update:false test
    for _ <- 1..12 do
      resp = Couch.get(url)
      assert resp.status_code == 200
    end

    Couch.put("/#{db_name}/foo:1", body: %{some: "field"})

    retry_until(fn ->
      resp =
        Couch.get(
          url,
          query: %{
            update: "false",
            limit: 3
          }
        )

      assert resp.status_code == 200
      ids = get_ids(resp)
      assert ids == ["foo:2", "foo:4", "foo:6"]
    end)
  end

  @tag :with_partitioned_db
  test "purge removes view rows", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_partition_ddoc(db_name)

    url = "/#{db_name}/_partition/foo/_design/mrtest/_view/some"

    resp = Couch.get(url)
    assert resp.status_code == 200
    %{body: body} = resp
    assert length(body["rows"]) == 50

    resp = Couch.get("/#{db_name}/foo:2")
    assert resp.status_code == 200
    %{body: body} = resp
    rev = body["_rev"]

    body = %{"foo:2" => [rev]}
    resp = Couch.post("/#{db_name}/_purge", query: [w: 3], body: body)
    assert resp.status_code == 201

    resp = Couch.get(url)
    assert resp.status_code == 200
    %{body: body} = resp
    assert length(body["rows"]) == 49
  end

  @tag :with_partitioned_db
  test "purged conflict changes view rows", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_partition_ddoc(db_name)

    url = "/#{db_name}/_partition/foo/_design/mrtest/_view/some"

    resp = Couch.get(url)
    assert resp.status_code == 200
    %{body: body} = resp
    assert length(body["rows"]) == 50

    # Create a conflict on foo:2. Since the 4096
    # value is deeper than the conflict we can assert
    # that's in the view before the purge and assert
    # that 8192 is in the view after the purge.
    resp = Couch.get("/#{db_name}/foo:2")
    assert resp.status_code == 200
    %{body: body} = resp
    rev1 = body["_rev"]

    doc = %{_id: "foo:2", _rev: rev1, value: 4096, some: "field"}
    resp = Couch.post("/#{db_name}", query: [w: 3], body: doc)
    assert resp.status_code == 201
    %{body: body} = resp
    rev2 = body["rev"]

    query = [w: 3, new_edits: false]
    conflict_rev = "1-4a75b4efa0804859b3dfd327cbc1c2f9"
    doc = %{_id: "foo:2", _rev: conflict_rev, value: 8192, some: "field"}
    resp = Couch.put("/#{db_name}/foo:2", query: query, body: doc)
    assert resp.status_code == 201

    # Check that our expected row exists
    resp = Couch.get(url, query: [key: 4096])
    assert resp.status_code == 200
    %{body: body} = resp
    [row] = body["rows"]
    assert row["id"] == "foo:2"

    # Remove the current row to be replaced with
    # a row from the conflict
    body = %{"foo:2" => [rev2]}
    resp = Couch.post("/#{db_name}/_purge", query: [w: 3], body: body)
    assert resp.status_code == 201

    resp = Couch.get(url, query: [key: 8192])
    assert resp.status_code == 200
    %{body: body} = resp
    [row] = body["rows"]
    assert row["id"] == "foo:2"
  end
end

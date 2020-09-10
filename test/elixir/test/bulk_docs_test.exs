defmodule BulkDocsTest do
  use CouchTestCase

  @moduletag :bulk_docs
  @moduletag kind: :single_node

  @moduledoc """
  Test CouchDB bulk docs
  This is a port of bulk_docs.js
  """

  @doc_range 1..5

  @tag :with_db
  test "bulk docs can create, update, & delete many docs per request", ctx do
    db = ctx[:db_name]
    docs = create_docs(@doc_range)
    resp = bulk_post(docs, db)
    assert revs_start_with(resp.body, "1-")
    docs = rev(docs, resp.body)
    # Modify each doc's `string` field and re-post
    docs =
      Enum.map(docs, fn doc = %{string: string} ->
        %{doc | string: string <> ".00"}
      end)

    resp = bulk_post(docs, db)
    assert revs_start_with(resp.body, "2-")
    docs = rev(docs, resp.body)
    # Confirm changes were applied for each doc
    assert Enum.all?(docs, fn doc ->
             String.ends_with?(Couch.get("/#{db}/#{doc._id}").body["string"], ".00")
           end)

    docs = Enum.map(docs, &Map.put(&1, :_deleted, true))
    resp = bulk_post(docs, db)
    assert revs_start_with(resp.body, "3-")
    # Confirm docs were deleted
    assert Enum.all?(docs, fn doc ->
             resp = Couch.get("/#{db}/#{doc._id}")
             assert resp.status_code == 404
             assert resp.body["error"] == "not_found"
             assert resp.body["reason"] == "deleted"
           end)
  end

  @tag :with_db
  @tag :skip_on_jenkins
  test "bulk docs can detect conflicts", ctx do
    db = ctx[:db_name]
    docs = create_docs(@doc_range)
    resp = bulk_post(docs, db)
    assert revs_start_with(resp.body, "1-")
    docs = rev(docs, resp.body)
    # Update just the first doc to create a conflict in subsequent bulk update
    doc = hd(docs)
    resp = Couch.put("/#{db}/#{doc._id}", body: doc)
    assert resp.status_code in [201, 202]
    # Attempt to delete all docs
    docs = Enum.map(docs, fn doc -> Map.put(doc, :_deleted, true) end)

    retry_until(fn ->
      resp = bulk_post(docs, db)
      # Confirm first doc not updated, and result has no rev field
      res = hd(resp.body)
      assert res["id"] == "1" and res["error"] == "conflict"
      assert Map.get(res, "rev") == nil
      # Confirm other docs updated normally
      assert revs_start_with(tl(resp.body), "2-")
    end)
  end

  @tag :with_db
  test "bulk docs supplies `id` if not provided in doc", ctx do
    docs = [%{foo: "bar"}]
    res = hd(bulk_post(docs, ctx[:db_name]).body)
    assert res["id"]
    assert res["rev"]
  end

  @tag :with_db
  test "bulk docs raises error for `all_or_nothing` option", ctx do
    opts = [body: %{docs: create_docs(@doc_range), all_or_nothing: true}]
    resp = Couch.post("/#{ctx[:db_name]}/_bulk_docs", opts)
    assert resp.status_code == 417
    assert Enum.all?(resp.body, &(Map.get(&1, "error") == "not_implemented"))
    expected_reason = "all_or_nothing is not supported"
    assert Enum.all?(resp.body, &(Map.get(&1, "reason") == expected_reason))
  end

  @tag :with_db
  test "bulk docs raises conflict error for combined update & delete", ctx do
    db = ctx[:db_name]
    doc = %{_id: "id", val: "val"}
    resp = Couch.put("/#{db}/#{doc._id}", body: doc)
    doc = rev(doc, resp.body)
    update = %{doc | val: "newval"}
    delete = Map.put(doc, :_deleted, true)
    body = bulk_post([update, delete], db).body
    assert Enum.count(body, &(Map.get(&1, "error") == "conflict")) == 1
    assert Enum.count(body, &Map.get(&1, "rev")) == 1
  end

  @tag :with_db
  test "bulk docs raises error for missing `docs` parameter", ctx do
    docs = [%{foo: "bar"}]
    resp = Couch.post("/#{ctx[:db_name]}/_bulk_docs", body: %{doc: docs})
    assert_bad_request(resp, "POST body must include `docs` parameter.")
  end

  @tag :with_db
  test "bulk docs raises error for invlaid `docs` parameter", ctx do
    resp = Couch.post("/#{ctx[:db_name]}/_bulk_docs", body: %{docs: "foo"})
    assert_bad_request(resp, "`docs` parameter must be an array.")
  end

  @tag :with_db
  test "bulk docs raises error for invlaid `new_edits` parameter", ctx do
    opts = [body: %{docs: [], new_edits: 0}]
    resp = Couch.post("/#{ctx[:db_name]}/_bulk_docs", opts)
    assert_bad_request(resp, "`new_edits` parameter must be a boolean.")
  end

  @tag :with_db
  test "bulk docs emits conflict error for duplicate doc `_id`s", ctx do
    docs = [%{_id: "0", a: 0}, %{_id: "1", a: 1}, %{_id: "1", a: 2}, %{_id: "3", a: 3}]
    rows = bulk_post(docs, ctx[:db_name]).body
    assert Enum.at(rows, 1)["id"] == "1"
    assert Enum.at(rows, 1)["ok"]
    assert Enum.at(rows, 2)["error"] == "conflict"
  end

  defp bulk_post(docs, db) do
    retry_until(fn ->
      resp = Couch.post("/#{db}/_bulk_docs", body: %{docs: docs})

      assert resp.status_code in [201, 202] and length(resp.body) == length(docs), """
      Expected 201 and the same number of response rows as in request, but got
      #{pretty_inspect(resp)}
      """

      resp
    end)
  end

  defp revs_start_with(rows, prefix) do
    Enum.all?(rows, fn %{"rev" => rev} -> String.starts_with?(rev, prefix) end)
  end

  defp assert_bad_request(resp, reason) do
    assert resp.status_code == 400
    assert resp.body["error"] == "bad_request"
    assert resp.body["reason"] == reason
  end
end

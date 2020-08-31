defmodule RevisionTest do
  use CouchTestCase

  @moduletag :conflicts
  @moduletag kind: :single_node

  @moduledoc """
  Test CouchDB conflicts
  This is a port of conflicts.js
  (but is arguably more focused on revisions than conflicts)
  """

  setup context do
    # Generate a doc with _rev field for each test
    doc = %{_id: "doc-1", a: 1, b: 1}
    doc = rev(doc, put(context[:db_name], doc))
    %{doc: doc}
  end

  @tag :with_db
  test "multiple updates with same _rev raise conflict errors", context do
    db = context[:db_name]
    doc = context[:doc]
    # doc and doc2 have same _rev
    doc2 = %{doc | a: 2, b: 2}
    # doc updated with new _rev
    _doc = rev(doc, put(db, doc))

    retry_until(fn ->
      assert_conflict(Couch.put("/#{db}/#{doc2._id}", body: doc2))

      resp = Couch.get("/#{db}/_changes")
      assert length(resp.body["results"]) == 1

      doc2 = Map.delete(doc2, :_rev)
      assert_conflict(Couch.put("/#{db}/#{doc2._id}", body: doc2))
    end)
  end

  @tag :with_db
  test "mismatched rev in body and query string returns error", context do
    db = context[:db_name]
    doc = context[:doc]
    resp = Couch.put("/#{db}/#{doc._id}?rev=1-foobar", body: doc)

    expected_reason =
      "Document rev from request body and query string " <> "have different values"

    assert_bad_request(resp, expected_reason)
  end

  @tag :with_db
  test "mismatched rev in body and etag returns error", context do
    opts = [body: context[:doc], headers: [{:"If-Match", "1-foobar"}]]
    resp = Couch.put("/#{context[:db_name]}/foobar", opts)
    expected_reason = "Document rev and etag have different values"
    assert_bad_request(resp, expected_reason)
  end

  @tag :with_db
  test "`new_edits: false` prevents bulk updates (COUCHDB-1178)", context do
    db = context[:db_name]

    ddoc = %{_id: "_design/couchdb-1178", validate_doc_update: "function(){}"}
    assert put(db, ddoc)["ok"] == true

    r0 = %{_id: "doc", val: "r0"}
    r1 = %{_id: "doc", val: "r1", _rev: "1-47f3268e7546965196b57572099f4372"}
    r2 = %{_id: "doc", val: "r2", _rev: "2-1d8171ab3a91475cfece749291e6f897"}
    r3 = %{_id: "doc", val: "r3", _rev: "3-3fb0a342d2ce092fdcc77856dbe8a2ef"}
    assert put(db, r0)["ok"] == true
    assert put(db, r1)["ok"] == true
    assert put(db, r2)["ok"] == true
    # N.b. that we *do not* put r3

    expected = %{
      "_id" => "doc",
      "_rev" => r3._rev,
      "_revisions" => %{
        "ids" => for(r <- [r3._rev, r2._rev, r1._rev], do: suffix(r)),
        "start" => 3
      },
      "val" => r2.val
    }

    assert Couch.get("/#{db}/doc?revs=true").body == expected

    opts = [body: %{docs: [r3, r2, r1], new_edits: false}]
    assert Couch.post("/#{db}/_bulk_docs", opts).body == []
  end

  defp put(db, doc) do
    Couch.put("/#{db}/#{doc._id}", body: doc).body
  end

  defp suffix(rev) do
    hd(tl(String.split(rev, "-")))
  end

  defp assert_conflict(resp) do
    assert resp.status_code == 409
    assert resp.body["error"] == "conflict"
    assert resp.body["reason"] == "Document update conflict."
  end

  defp assert_bad_request(resp, reason) do
    assert resp.status_code == 400
    assert resp.body["error"] == "bad_request"
    assert resp.body["reason"] == reason
  end
end

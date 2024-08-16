defmodule CompactTest do
  use CouchTestCase

  @moduletag :compact

  @moduledoc """
  Test CouchDB compaction
  This is a port of compact.js
  """

  @att_doc_id1 "att_doc1"
  @att_doc_id2 "att_doc2"
  @att_name "foo.txt"
  @att_plaintext String.duplicate("This is plain text", 100)

  @tag :with_db
  test "compaction reduces size of deleted docs", context do
    db = context[:db_name]
    docs = populate(db)
    info = get_info(db)
    orig_data_size = info["sizes"]["active"]
    orig_disk_size = info["sizes"]["file"]
    start_time = info["instance_start_time"]
    assert is_integer(orig_data_size) and is_integer(orig_disk_size)
    assert orig_data_size < orig_disk_size

    delete(db, docs)

    retry_until(fn ->
      deleted_data_size = get_info(db)["data_size"]
      assert deleted_data_size > orig_data_size
    end)

    deleted_data_size = get_info(db)["data_size"]

    compact(db)

    retry_until(fn ->
      assert get_info(db)["instance_start_time"] == start_time
      assert Couch.get("/#{db}").body["doc_count"] == 1
      assert_attachment_available(db)
      assert_deleted_attachment_not_available(db)
      info = get_info(db)
      final_data_size = info["sizes"]["active"]
      final_disk_size = info["sizes"]["file"]
      assert final_data_size < final_disk_size
      assert is_integer(final_data_size) and is_integer(final_disk_size)
      assert final_data_size < deleted_data_size
    end)
  end

  defp assert_attachment_available(db) do
    resp = Couch.get("/#{db}/#{@att_doc_id1}/#{@att_name}")
    assert resp.body == @att_plaintext
    assert resp.headers["content-type"] == "text/plain"
  end

  defp assert_deleted_attachment_not_available(db) do
    resp = Couch.get("/#{db}/#{@att_doc_id2}/#{@att_name}")
    assert resp.status_code == 404
    assert resp.body == %{"error" => "not_found", "reason" => "deleted"}
  end

  defp populate(db) do
    docs = create_docs(0..19)
    resp = Couch.post("/#{db}/_bulk_docs", body: %{docs: docs})
    assert resp.status_code in [201, 202]
    docs = rev(docs, resp.body)

    doc1 = %{
      _id: "#{@att_doc_id1}",
      _attachments: %{
        "#{@att_name}": %{content_type: "text/plain", data: Base.encode64(@att_plaintext)}
      }
    }

    resp = Couch.put("/#{db}/#{doc1._id}", body: doc1)
    assert resp.status_code in [201, 202]

    doc2 = %{
      _id: "#{@att_doc_id2}",
      _attachments: %{
        "#{@att_name}": %{content_type: "text/plain", data: Base.encode64(@att_plaintext)}
      }
    }

    resp = Couch.put("/#{db}/#{doc2._id}", body: doc2)
    assert resp.status_code in [201, 202]

    att_rev2 = resp.body["rev"]
    docs ++ [%{_id: "#{doc2._id}", _rev: att_rev2}]
  end

  defp delete(db, docs) do
    docs = Enum.map(docs, &Map.put(&1, :_deleted, true))
    resp = Couch.post("/#{db}/_bulk_docs", body: %{docs: docs})
    assert resp.status_code in [201, 202]
    assert Couch.post("/#{db}/_ensure_full_commit").body["ok"] == true
  end

  defp get_info(db) do
    Couch.get("/#{db}").body
  end
end

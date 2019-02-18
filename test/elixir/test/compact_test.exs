defmodule CompactTest do
  use CouchTestCase

  @moduletag :compact

  @moduledoc """
  Test CouchDB compaction
  This is a port of compact.js
  """

  @att_doc_id "att_doc"
  @att_name "foo.txt"
  @att_plaintext "This is plain text"

  @tag :skip_on_jenkins
  @tag :with_db
  test "compaction reduces size of deleted docs", context do
    db = context[:db_name]
    docs = populate(db)
    info = get_info(db)
    orig_data_size = info["data_size"]
    orig_disk_size = info["disk_size"]
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
      assert_attachment_available(db)
      info = get_info(db)
      final_data_size = info["data_size"]
      final_disk_size = info["disk_size"]
      assert final_data_size < final_disk_size
      assert is_integer(final_data_size) and is_integer(final_disk_size)
      assert final_data_size < deleted_data_size
    end)
  end

  defp assert_attachment_available(db) do
    resp = Couch.get("/#{db}/#{@att_doc_id}/#{@att_name}")
    assert resp.body == @att_plaintext
    assert resp.headers["content-type"] == "text/plain"
    assert Couch.get("/#{db}").body["doc_count"] == 1
  end

  defp populate(db) do
    docs = create_docs(0..19)
    resp = Couch.post("/#{db}/_bulk_docs", body: %{docs: docs})
    assert resp.status_code == 201
    docs = rev(docs, resp.body)

    doc = %{
      _id: "#{@att_doc_id}",
      _attachments: %{
        "#{@att_name}": %{content_type: "text/plain", data: Base.encode64(@att_plaintext)}
      }
    }

    resp = Couch.put("/#{db}/#{doc._id}", body: doc)
    assert resp.status_code == 201
    docs
  end

  defp delete(db, docs) do
    docs = Enum.map(docs, &Map.put(&1, :_deleted, true))
    resp = Couch.post("/#{db}/_bulk_docs", body: %{docs: docs})
    assert resp.status_code == 201
    assert Couch.post("/#{db}/_ensure_full_commit").body["ok"] == true
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

  defp get_info(db) do
    Couch.get("/#{db}").body
  end
end

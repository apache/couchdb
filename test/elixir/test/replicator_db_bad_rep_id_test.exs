defmodule ReplicationBadIdTest do
  use CouchTestCase

  @moduledoc """
  This is a port of the replicator_db_bad_rep_id.js suite
  """

  @moduletag :replication
  @moduletag kind: :cluster

  @docs [
    %{
      _id: "foo1",
      value: 11
    },
    %{
      _id: "foo2",
      value: 22
    },
    %{
      _id: "foo3",
      value: 33
    }
  ]

  test "replication doc with bad rep id" do
    name = random_db_name()
    src_db_name = name <> "_src"
    tgt_db_name = name <> "_tgt"

    create_db(src_db_name)
    bulk_save(src_db_name, @docs)
    create_db(tgt_db_name)
    delete_db_on_exit([src_db_name, tgt_db_name])

    src_db_url = Couch.process_url("/#{src_db_name}")
    tgt_db_url = Couch.process_url("/#{tgt_db_name}")

    replication_doc = %{
      _id: "foo_rep_#{name}",
      source: src_db_url,
      target: tgt_db_url,
      replication_id: "1234abc"
    }

    {:ok, repdoc} = create_doc("_replicator", replication_doc)
    delete_doc_on_exit("_replicator", repdoc.body["id"])

    retry_until(fn ->
      resp = Couch.get("/_replicator/#{replication_doc[:_id]}")
      assert resp.body["_replication_state"] == "completed"
      resp
    end)

    Enum.each(@docs, fn doc ->
      copy_resp = Couch.get("/#{tgt_db_name}/#{doc[:_id]}")
      assert copy_resp.status_code == 200
      assert copy_resp.body["value"] === doc.value
    end)

    resp = Couch.get("/_replicator/#{replication_doc[:_id]}")
    assert resp.status_code == 200
    assert resp.body["source"] == replication_doc.source
    assert resp.body["target"] == replication_doc.target
    assert resp.body["_replication_state"] == "completed"
    {:ok, _, _} = DateTime.from_iso8601(resp.body["_replication_state_time"])
    assert resp.body["_replication_id"] == nil
  end

  def delete_db_on_exit(db_names) when is_list(db_names) do
    on_exit(fn ->
      Enum.each(db_names, fn name ->
        delete_db(name)
      end)
    end)
  end

  def delete_doc_on_exit(db_name, doc_id) do
    on_exit(fn ->
      resp = Couch.get("/#{db_name}/#{doc_id}")
      Couch.delete("/#{db_name}/#{doc_id}?rev=#{resp.body["_rev"]}")
    end)
  end
end

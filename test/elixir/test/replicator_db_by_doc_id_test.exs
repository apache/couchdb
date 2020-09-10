defmodule ReplicatorDBByDocIdTest do
  use CouchTestCase

  @moduledoc """
  This is a port of the replicator_db_by_doc_id.js suite
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

  test "replicatior db by doc id" do
    name = random_db_name()
    src_db_name = name <> "_src"
    tgt_db_name = name <> "_tgt"

    create_db(src_db_name)
    create_db(tgt_db_name)
    delete_db_on_exit([src_db_name, tgt_db_name])

    # Populate src DB
    ddocs = [
      %{
        _id: "_design/mydesign",
        language: "javascript"
      }
    ]

    docs = @docs ++ ddocs
    bulk_save(src_db_name, docs)

    src_db_url = Couch.process_url("/#{src_db_name}")
    tgt_db_url = build_tgt_uri(tgt_db_name)

    replication_doc = %{
      _id: "foo_cont_rep_#{name}",
      source: src_db_url,
      target: tgt_db_url,
      doc_ids: ["foo666", "foo3", "_design/mydesign", "foo999", "foo1"]
    }

    {:ok, repdoc} = create_doc("_replicator", replication_doc)
    delete_doc_on_exit("_replicator", repdoc.body["id"])

    retry_until(fn ->
      resp = Couch.get("/_replicator/#{replication_doc[:_id]}")
      assert resp.body["_replication_state"] == "completed"
      resp
    end)

    copy_resp = Couch.get("/#{tgt_db_name}/foo1")
    assert copy_resp.status_code == 200
    assert copy_resp.body["value"] === 11

    copy_resp = Couch.get("/#{tgt_db_name}/foo2")
    assert copy_resp.status_code == 404

    copy_resp = Couch.get("/#{tgt_db_name}/foo3")
    assert copy_resp.status_code == 200
    assert copy_resp.body["value"] === 33

    copy_resp = Couch.get("/#{tgt_db_name}/foo666")
    assert copy_resp.status_code == 404

    copy_resp = Couch.get("/#{tgt_db_name}/foo999")
    assert copy_resp.status_code == 404

    # Javascript test suite was executed with admin party
    # the design doc was created during replication.
    # Elixir test suite is executed configuring an admin.
    # The auth info should be provided for the tgt db in order to
    # create the design doc during replication
    copy_resp = Couch.get("/#{tgt_db_name}/_design/mydesign")
    assert copy_resp.status_code == 200

    resp = Couch.get("/_replicator/#{replication_doc[:_id]}")
    assert resp.status_code == 200
    assert resp.body["_replication_stats"]["revisions_checked"] == 3
    assert resp.body["_replication_stats"]["missing_revisions_found"] == 3
    assert resp.body["_replication_stats"]["docs_read"] == 3
    assert resp.body["_replication_stats"]["docs_written"] == 3
    assert resp.body["_replication_stats"]["doc_write_failures"] == 0
  end

  defp build_tgt_uri(db_name) do
    username = System.get_env("EX_USERNAME") || "adm"
    password = System.get_env("EX_PASSWORD") || "pass"

    "/#{db_name}"
    |> Couch.process_url()
    |> URI.parse()
    |> Map.put(:userinfo, "#{username}:#{password}")
    |> URI.to_string()
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

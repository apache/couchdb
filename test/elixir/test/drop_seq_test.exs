defmodule DropSeqTest do
  use CouchTestCase

  @moduletag :drop_seq

  @tag :with_db
  test "peer checkpoint - mrview", context do
    db_name = context[:db_name]
    ddoc_id = "_design/foo"

    create_checkpoint_fn = fn ->
      resp = Couch.put("/#{db_name}/#{ddoc_id}", body: %{
        views: %{
          bar: %{
            map: "function(doc) {}"
          }
        }
      })
      assert resp.status_code == 201
    end

    update_checkpoint_fn = fn ->
      assert Couch.get("/#{db_name}/#{ddoc_id}/_view/bar").status_code == 200
    end

    checkpoint_test_helper(context[:db_name], create_checkpoint_fn, update_checkpoint_fn)
  end

  @tag :with_db
  test "peer checkpoint - search", context do
    db_name = context[:db_name]
    ddoc_id = "_design/foo"

    create_checkpoint_fn = fn ->
      resp = Couch.put("/#{db_name}/#{ddoc_id}", body: %{
        indexes: %{
          bar: %{
            index: "function(doc) {}"
          }
        }
      })
      assert resp.status_code == 201
    end

    update_checkpoint_fn = fn ->
      assert Couch.get("/#{db_name}/#{ddoc_id}/_search/bar?q=*:*").status_code == 200
    end

    checkpoint_test_helper(context[:db_name], create_checkpoint_fn, update_checkpoint_fn)
  end

  @tag :with_db
  test "peer checkpoint - nouveau", context do
    db_name = context[:db_name]
    ddoc_id = "_design/foo"

    create_checkpoint_fn = fn ->
      resp = Couch.put("/#{db_name}/#{ddoc_id}", body: %{
        nouveau: %{
          bar: %{
            index: "function(doc) {}"
          }
        }
      })
      assert resp.status_code == 201
    end

    update_checkpoint_fn = fn ->
      # need to add a new document to force nouveau update to run each time
      # as we only advance the peer checkpoint when JVM-side nouveau has committed
      # the index
      assert Couch.post("/#{db_name}", body: %{}).status_code == 201
      assert Couch.get("/#{db_name}/#{ddoc_id}/_nouveau/bar?q=*:*").status_code == 200
    end

    checkpoint_test_helper(context[:db_name], create_checkpoint_fn, update_checkpoint_fn)
  end

  @tag :with_db
  test "peer checkpoint - custom", context do
    db_name = context[:db_name]
    peer_checkpoint_id = "_local/peer-checkpoint-foo"

    update_checkpoint_fn = fn ->
      resp = Couch.get("/#{db_name}")
      assert resp.status_code == 200
      update_seq = resp.body["update_seq"]

      resp = Couch.put("/#{db_name}/#{peer_checkpoint_id}", body: %{
        update_seq: update_seq
      })
      assert resp.status_code == 201
    end

    checkpoint_test_helper(context[:db_name], update_checkpoint_fn, update_checkpoint_fn)
  end

  @tag :with_db
  test "peer checkpoint - shard split", context do
    db_name = context[:db_name]
    peer_checkpoint_id = "_local/peer-checkpoint-foo"

    create_checkpoint_fn = fn ->
      resp = Couch.get("/#{db_name}")
      assert resp.status_code == 200
      update_seq = resp.body["update_seq"]

      resp = Couch.put("/#{db_name}/#{peer_checkpoint_id}", body: %{
        update_seq: update_seq
      })
      assert resp.status_code == 201

      resp = Couch.get("/#{db_name}/_shards")
      assert resp.status_code == 200
      ranges = Map.keys(resp.body["shards"])
      Enum.each(ranges, fn r ->
        resp = Couch.post("/_reshard/jobs", body: %{
          type: "split",
          db: db_name,
          range: r
        })
        assert resp.status_code == 201
      end)
      wait_for_reshard_jobs_to_complete()
    end

    after_doc_deletion_fn = fn ->
      split_all_shard_ranges(db_name)
      wait_for_reshard_jobs_to_complete()
    end

    update_checkpoint_fn = fn ->
      resp = Couch.get("/#{db_name}")
      assert resp.status_code == 200
      update_seq = resp.body["update_seq"]

      resp = Couch.put("/#{db_name}/#{peer_checkpoint_id}", body: %{
        update_seq: update_seq
      })
      assert resp.status_code == 201
    end

    checkpoint_test_helper(context[:db_name],
      create_checkpoint_fn, update_checkpoint_fn, after_doc_deletion_fn)
  end

  defp checkpoint_test_helper(db_name, create_checkpoint_fn, update_checkpoint_fn) do
    checkpoint_test_helper(db_name, create_checkpoint_fn, update_checkpoint_fn, fn() -> true end)
  end

  defp checkpoint_test_helper(db_name, create_checkpoint_fn, update_checkpoint_fn, after_doc_deletion_fn) do
    doc_id = "testdoc"

    drop_count = get_drop_count(db_name)
    drop_seq = update_drop_seq(db_name)

    # create something that will create a peer checkpoint
    create_checkpoint_fn.()
    assert get_drop_count(db_name) == drop_count

    # create a document
    resp = Couch.put("/#{db_name}/#{doc_id}", body: %{})
    assert resp.status_code == 201
    rev = resp.body["rev"]

    # delete it
    resp = Couch.delete("/#{db_name}/#{doc_id}?rev=#{rev}")
    assert resp.status_code == 200

    after_doc_deletion_fn.()

    # wait for drop seq to change
    wait_for_drop_seq_change(db_name, drop_seq, update_checkpoint_fn)
    assert get_drop_count(db_name) == drop_count

    # confirm deleted doc is still in _changes response
    resp = Couch.get("/#{db_name}/_changes")
    assert resp.status_code == 200
    assert Enum.member?(get_ids(resp), doc_id)

    # compact
    compact(db_name)

    # confirm deleted doc is not in _changes response
    resp = Couch.get("/#{db_name}/_changes")
    assert resp.status_code == 200
    assert !Enum.member?(get_ids(resp), doc_id)
    assert get_drop_count(db_name) == drop_count + 1
  end

  defp wait_for_drop_seq_change(db_name, previous_drop_seq, update_checkpoint_fn) do
    retry_until(
      fn ->
        update_checkpoint_fn.()
        new_drop_seq = update_drop_seq(db_name)
        new_drop_seq != previous_drop_seq
      end,
      200,
      10_000
    )
  end

  defp split_all_shard_ranges(db_name) do
    resp = Couch.get("/#{db_name}/_shards")
    assert resp.status_code == 200
    ranges = Map.keys(resp.body["shards"])
    Enum.each(ranges, fn r ->
      resp = Couch.post("/_reshard/jobs", body: %{
        type: "split",
        db: db_name,
        range: r
      })
      assert resp.status_code == 201
    end)
  end

  defp wait_for_reshard_jobs_to_complete() do
    retry_until(
      fn ->
        resp = Couch.get("/_reshard/jobs")
        assert resp.status_code == 200
        Enum.all?(resp.body["jobs"], fn job ->
          job["job_state"] == "completed"
        end)
      end,
      200,
      10_000
    )
  end

  defp update_drop_seq(db_name) do
    resp = Couch.post("/#{db_name}/_update_drop_seq")
    assert resp.status_code == 201
    resp.body["results"]
  end

  defp get_drop_count(db_name) do
    resp = Couch.get("/#{db_name}")
    assert resp.status_code == 200
    resp.body["drop_count"]
  end

  defp get_ids(resp) do
    %{:body => %{"results" => results}} = resp
    Enum.map(results, fn result -> result["id"] end)
  end

end

defmodule ReshardBasicTest do
  use CouchTestCase
  import ReshardHelpers

  @moduledoc """
  Test resharding basic functionality
  """

  setup_all do
    db1 = random_db_name()
    {:ok, _} = create_db(db1, query: %{q: 1})
    db2 = random_db_name()
    {:ok, _} = create_db(db2, query: %{q: 2})

    on_exit(fn ->
      reset_reshard_state()
      delete_db(db1)
      delete_db(db2)
    end)

    {:ok, [db1: db1, db2: db2]}
  end

  test "basic api querying, no jobs present" do
    summary = get_summary()
    assert summary["state"] == "running"
    assert summary["state_reason"] == :null
    assert summary["total"] == 0
    assert summary["completed"] == 0
    assert summary["failed"] == 0
    assert summary["stopped"] == 0
    assert get_state() == %{"state" => "running", "reason" => :null}
    assert get_jobs() == []
  end

  test "check validation of invalid parameters", context do
    db1 = context[:db1]
    node1 = get_first_node()

    resp = post_job_node(db1, "badnode")
    assert resp.status_code == 400

    resp = post_job_node("badresharddb", node1)
    assert resp.status_code == 400

    resp = post_job_db("badresharddb")
    assert resp.status_code == 400

    resp = post_job_range("badresharddb", "randomgarbage")
    assert resp.status_code == 400

    resp = get_job("badjobid")
    assert resp.status_code == 404

    resp = remove_job("badjobid")
    assert resp.status_code == 404
  end

  test "toggle global state" do
    assert get_state() == %{"state" => "running", "reason" => :null}
    put_state_stopped("xyz")
    assert get_state() == %{"state" => "stopped", "reason" => "xyz"}
    put_state_running()
    assert get_state() == %{"state" => "running", "reason" => :null}
  end

  test "split q=1 db shards on node1 (1 job)", context do
    db = context[:db1]
    node1 = get_first_node()

    resp = post_job_node(db, node1)
    assert resp.status_code in [201, 202]

    body = resp.body
    assert is_list(body)
    assert length(body) == 1

    [job] = body
    id = job["id"]
    assert is_binary(id)
    node = job["node"]
    assert is_binary(node)
    assert node == node1
    assert job["ok"] == true
    shard = job["shard"]
    assert is_binary(shard)

    resp = get_job(id)
    assert resp.status_code == 200

    body = resp.body
    assert body["type"] == "split"
    assert body["id"] == id
    assert body["source"] == shard
    assert is_list(body["history"])
    assert body["job_state"] in ["new", "running", "completed"]
    assert is_list(body["target"])
    assert length(body["target"]) == 2

    wait_job_completed(id)

    resp = get_job(id)
    assert resp.status_code == 200

    body = resp.body
    assert body["job_state"] == "completed"
    assert body["split_state"] == "completed"

    resp = Couch.get("/#{db}/_shards")
    assert resp.status_code == 200
    shards = resp.body["shards"]
    assert node1 not in Map.get(shards, "00000000-ffffffff", [])
    assert shards["00000000-7fffffff"] == [node1]
    assert shards["80000000-ffffffff"] == [node1]

    summary = get_summary()
    assert summary["total"] == 1
    assert summary["completed"] == 1

    resp = remove_job(id)
    assert resp.status_code == 200

    assert get_jobs() == []

    summary = get_summary()
    assert summary["total"] == 0
    assert summary["completed"] == 0
  end

  test "split q=2 shards on node1 (2 jobs)", context do
    db = context[:db2]
    node1 = get_first_node()

    resp = post_job_node(db, node1)
    assert resp.status_code in [201, 202]

    body = resp.body
    assert is_list(body)
    assert length(body) == 2

    [job1, job2] = Enum.sort(body)
    {id1, id2} = {job1["id"], job2["id"]}

    assert get_job(id1).body["id"] == id1
    assert get_job(id2).body["id"] == id2

    summary = get_summary()
    assert summary["total"] == 2

    wait_job_completed(id1)
    wait_job_completed(id2)

    summary = get_summary()
    assert summary["completed"] == 2

    resp = Couch.get("/#{db}/_shards")
    assert resp.status_code == 200
    shards = resp.body["shards"]
    assert node1 not in Map.get(shards, "00000000-7fffffff", [])
    assert node1 not in Map.get(shards, "80000000-ffffffff", [])
    assert shards["00000000-3fffffff"] == [node1]
    assert shards["40000000-7fffffff"] == [node1]
    assert shards["80000000-bfffffff"] == [node1]
    assert shards["c0000000-ffffffff"] == [node1]

    # deleting the source db should remove the jobs
    delete_db(db)
    wait_job_removed(id1)
    wait_job_removed(id2)

    summary = get_summary()
    assert summary["total"] == 0
  end
end

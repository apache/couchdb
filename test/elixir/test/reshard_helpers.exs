defmodule ReshardHelpers do
  use CouchTestCase

  def get_summary do
    resp = Couch.get("/_reshard")
    assert resp.status_code == 200
    resp.body
  end

  def get_state do
    resp = Couch.get("/_reshard/state")
    assert resp.status_code == 200
    resp.body
  end

  def put_state_running do
    resp = Couch.put("/_reshard/state", body: %{:state => "running"})
    assert resp.status_code == 200
    resp
  end

  def put_state_stopped(reason \\ "") do
    body = %{:state => "stopped", :reason => reason}
    resp = Couch.put("/_reshard/state", body: body)
    assert resp.status_code == 200
    resp
  end

  def get_jobs do
    resp = Couch.get("/_reshard/jobs")
    assert resp.status_code == 200
    resp.body["jobs"]
  end

  def post_job_db(db) do
    body = %{:type => :split, :db => db}
    Couch.post("/_reshard/jobs", body: body)
  end

  def post_job_node(db, node) do
    body = %{:type => :split, :db => db, :node => node}
    Couch.post("/_reshard/jobs", body: body)
  end

  def post_job_range(db, range) do
    body = %{:type => :split, :db => db, :range => range}
    Couch.post("/_reshard/jobs", body: body)
  end

  def post_job_node_and_range(db, node, range) do
    body = %{:type => :split, :db => db, :node => node, :range => range}
    Couch.post("/_reshard/jobs", body: body)
  end

  def get_job(id) when is_binary(id) do
    Couch.get("/_reshard/jobs/#{id}")
  end

  def remove_job(id) when is_binary(id) do
    Couch.delete("/_reshard/jobs/#{id}")
  end

  def get_job_state(id) when is_binary(id) do
    resp = Couch.get("/_reshard/jobs/#{id}/state")
    assert resp.status_code == 200
    resp.body["state"]
  end

  def stop_job(id, reason \\ "") when is_binary(id) do
    body = %{:state => "stopped", :reason => reason}
    Couch.post("/_reshard/jobs/#{id}/state", body: body)
  end

  def resume_job(id) when is_binary(id) do
    body = %{:state => "running"}
    Couch.post("/_reshard/jobs/#{id}/state", body: body)
  end

  def job_ids(jobs) do
    Enum.map(fn job -> job["id"] end, jobs)
  end

  def get_first_node do
    mresp = Couch.get("/_membership")
    assert mresp.status_code == 200
    all_nodes = mresp.body["all_nodes"]

    mresp.body["cluster_nodes"]
    |> Enum.filter(fn n -> n in all_nodes end)
    |> Enum.sort()
    |> hd()
  end

  def wait_job_removed(id) do
    retry_until(fn -> get_job(id).status_code == 404 end, 200, 10_000)
  end

  def wait_job_completed(id) do
    wait_job_state(id, "completed")
  end

  def wait_job_state(id, state) do
    retry_until(fn -> get_job_state(id) == state end, 200, 10_000)
  end

  def reset_reshard_state do
    get_jobs()
    |> Enum.map(fn j -> j["id"] end)
    |> Enum.each(fn id -> remove_job(id) end)

    assert get_jobs() == []
    put_state_running()
  end
end

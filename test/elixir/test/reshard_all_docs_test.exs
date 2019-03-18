defmodule ReshardAllDocsTest do
  use CouchTestCase
  import ReshardHelpers

  @moduledoc """
  Test _all_docs interaction with resharding
  """

  setup do
    db = random_db_name()
    {:ok, _} = create_db(db, query: %{q: 2})

    on_exit(fn ->
      reset_reshard_state()
      delete_db(db)
    end)

    {:ok, [db: db]}
  end

  test "all_docs after splitting all shards on node1", context do
    db = context[:db]
    node1 = get_first_node()
    docs = add_docs(1..100, db)

    before_split_all_docs = all_docs(db)
    assert docs == before_split_all_docs

    resp = post_job_node(db, node1)
    assert resp.status_code == 201
    jobid = hd(resp.body)["id"]
    wait_job_completed(jobid)

    assert before_split_all_docs == all_docs(db)

    assert remove_job(jobid).status_code == 200
  end

  test "all_docs after splitting the same range on all nodes", context do
    db = context[:db]
    docs = add_docs(1..100, db)

    before_split_all_docs = all_docs(db)
    assert docs == before_split_all_docs

    resp = post_job_range(db, "00000000-7fffffff")
    assert resp.status_code == 201

    resp.body
    |> Enum.map(fn j -> j["id"] end)
    |> Enum.each(fn id -> wait_job_completed(id) end)

    assert before_split_all_docs == all_docs(db)

    get_jobs()
    |> Enum.map(fn j -> j["id"] end)
    |> Enum.each(fn id -> remove_job(id) end)
  end

  defp add_docs(range, db) do
    docs = create_docs(range)
    w3 = %{:w => 3}
    resp = Couch.post("/#{db}/_bulk_docs", body: %{docs: docs}, query: w3)
    assert resp.status_code == 201
    assert length(resp.body) == length(docs)

    docs
    |> rev(resp.body)
    |> Enum.into(%{}, fn %{:_id => id, :_rev => rev} -> {id, rev} end)
  end

  defp all_docs(db, query \\ %{}) do
    resp = Couch.get("/#{db}/_all_docs", query: query)
    assert resp.status_code == 200

    resp.body["rows"]
    |> Enum.into(%{}, fn %{"id" => id, "value" => v} -> {id, v["rev"]} end)
  end
end

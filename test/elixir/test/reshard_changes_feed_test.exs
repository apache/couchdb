defmodule ReshardChangesFeedTest do
  use CouchTestCase
  import ReshardHelpers

  @moduledoc """
  Test _changes interaction with resharding
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
    add_docs(1..3, db)

    all_before = changes(db)
    first_seq = hd(all_before["results"])["seq"]
    last_seq = all_before["last_seq"]
    since_1_before = docset(changes(db, %{:since => first_seq}))
    since_last_before = docset(changes(db, %{:since => last_seq}))

    resp = post_job_range(db, "00000000-7fffffff")
    assert resp.status_code in [201, 202]

    resp.body
    |> Enum.map(fn j -> j["id"] end)
    |> Enum.each(fn id -> wait_job_completed(id) end)

    all_after = changes(db)
    since_1_after = docset(changes(db, %{:since => first_seq}))
    since_last_after = docset(changes(db, %{:since => last_seq}))

    assert docset(all_before) == docset(all_after)
    assert MapSet.subset?(since_1_before, since_1_after)
    assert MapSet.subset?(since_last_before, since_last_after)

    get_jobs()
    |> Enum.map(fn j -> j["id"] end)
    |> Enum.each(fn id -> remove_job(id) end)
  end

  defp docset(changes) do
    changes["results"]
    |> Enum.map(fn %{"id" => id} -> id end)
    |> MapSet.new()
  end

  defp changes(db, query \\ %{}) do
    resp = Couch.get("/#{db}/_changes", query: query)
    assert resp.status_code == 200
    resp.body
  end

  defp add_docs(range, db) do
    docs = create_docs(range)
    w3 = %{:w => 3}
    resp = Couch.post("/#{db}/_bulk_docs", body: %{docs: docs}, query: w3)
    assert resp.status_code in [201, 202]
    assert length(resp.body) == length(docs)

    docs
    |> rev(resp.body)
    |> Enum.into(%{}, fn %{:_id => id, :_rev => rev} -> {id, rev} end)
  end

  # (Keep for debugging)
  # defp unpack_seq(seq) when is_binary(seq) do
  #   [_, opaque] = String.split(seq, "-")
  #   {:ok, binblob} = Base.url_decode64(opaque, padding: false)
  #   :erlang.binary_to_term(binblob)
  # end
end

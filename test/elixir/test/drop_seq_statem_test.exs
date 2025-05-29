defmodule DropSeqStateM do
  use PropCheck, default_opts: &PropCheck.TestHelpers.config/0
  use PropCheck.StateM
  use CouchTestCase

  # alias Couch.Test.Utils
  # import Utils

  @moduletag capture_log: true

  # expected to pass in all three cluster scenarios
  @moduletag :with_cluster
  @moduletag :without_quorum_test
  @moduletag :with_quorum_test

  property "drop_seq works fine", start_size: 5, max_size: 100, numtests: 2000 do
    forall cmds <- more_commands(100, commands(__MODULE__)) do
      trap_exit do
        db_name = random_db_name()
        {:ok, _} = create_db(db_name, query: %{n: 3, q: 4})
        r = run_commands(__MODULE__, cmds, [{:dbname, db_name}])
        {history, state, result} = r
        delete_db(db_name)

        (result == :ok)
        |> when_fail(
          IO.puts("""
          Commands: #{inspect(cmds, pretty: true)}
          History: #{inspect(history, pretty: true)}
          State: #{inspect(state, pretty: true)}
          Result: #{inspect(result, pretty: true)}
          """)
        )
      end
    end
  end

  require Record

  Record.defrecord(:state,
    docs: [],
    deleted_docs: [],
    current_seq: 0,
    peer_checkpoint_seq: nil,
    drop_seq: nil,
    drop_count: 0
  )

  def initial_state() do
    state()
  end

  @max_doc_ids 10
  @doc_ids 1..@max_doc_ids |> Enum.map(&"doc-#{&1}")

  def doc_id, do: oneof(@doc_ids)

  def command(_state) do
    oneof([
      {:call, __MODULE__, :update_document, [{:var, :dbname}, doc_id()]},
      {:call, __MODULE__, :delete_document, [{:var, :dbname}, doc_id()]},
      {:call, __MODULE__, :update_peer_checkpoint, [{:var, :dbname}]},
      {:call, __MODULE__, :update_drop_seq, [{:var, :dbname}]},
      {:call, __MODULE__, :compact_db, [{:var, :dbname}]},
      {:call, __MODULE__, :changes, [{:var, :dbname}]}
    ])
  end

  def get_document(db_name, doc_id) do
    resp = Couch.get("/#{db_name}/#{doc_id}")

    case resp.status_code do
      200 ->
        {:ok, resp.body}

      404 ->
        {:not_found, resp.body["reason"]}
    end
  end

  def update_document(db_name, doc_id) do
    case get_document(db_name, doc_id) do
      {:ok, doc} ->
        resp = Couch.put("/#{db_name}/#{doc_id}", body: doc)
        assert resp.status_code == 201

      {:not_found, _} ->
        resp = Couch.put("/#{db_name}/#{doc_id}", body: %{})
        assert resp.status_code == 201
    end

    sync_shards(db_name)
  end

  def delete_document(db_name, doc_id) do
    case get_document(db_name, doc_id) do
      {:ok, doc} ->
        rev = doc["_rev"]
        resp = Couch.delete("/#{db_name}/#{doc_id}?rev=#{rev}")
        assert resp.status_code == 200

      {:not_found, _} ->
        :ok
    end

    sync_shards(db_name)
  end

  def update_peer_checkpoint(db_name) do
    resp = Couch.get("/#{db_name}")
    assert resp.status_code == 200
    update_seq = resp.body["update_seq"]

    resp =
      Couch.put("/#{db_name}/_local/peer-checkpoint-foo",
        body: %{
          update_seq: update_seq
        }
      )

    assert resp.status_code == 201
    seq_to_shards(update_seq)
  end

  def update_drop_seq(db_name) do
    resp = Couch.post("/#{db_name}/_update_drop_seq")
    assert resp.status_code == 201
    resp.body
  end

  def compact_db(db_name) do
    compact(db_name)
    # try to avoid seeing pre-compact state of shards immediately after
    # compactor pids exit
    :timer.sleep(1000)
  end

  def changes(db_name) do
    resp = Couch.get("/#{db_name}/_changes")
    assert resp.status_code == 200

    List.foldl(resp.body["results"], {[], []}, fn change, {doc_ids, del_doc_ids} ->
      if change["deleted"] do
        {doc_ids, Enum.sort([change["id"] | del_doc_ids])}
      else
        {Enum.sort([change["id"] | doc_ids]), del_doc_ids}
      end
    end)
  end

  def sync_shards(db_name) do
    resp = Couch.post("/#{db_name}/_sync_shards")
    assert resp.status_code == 202
    :timer.sleep(1000)
  end

  def precondition(s, {:call, _, :update_document, [_db_name, doc_id]}) do
    not doc_exists(s, doc_id)
  end

  def precondition(s, {:call, _, :delete_document, [_db_name, doc_id]}) do
    doc_exists(s, doc_id)
  end

  def precondition(_, _) do
    true
  end

  def next_state(s, _v, {:call, _, :update_document, [_db_name, doc_id]}) do
    state(s,
      current_seq: state(s, :current_seq) + 1,
      docs: Enum.sort([doc_id | state(s, :docs)]),
      deleted_docs: List.keydelete(state(s, :deleted_docs), doc_id, 0)
    )
  end

  def next_state(s, _v, {:call, _, :delete_document, [_db_name, doc_id]}) do
    state(s,
      current_seq: state(s, :current_seq) + 1,
      docs: List.delete(state(s, :docs), doc_id),
      deleted_docs:
        Enum.sort([{doc_id, state(s, :current_seq) + 1} | state(s, :deleted_docs)])
    )
  end

  def next_state(s, _v, {:call, _, :update_peer_checkpoint, [_db_name]}) do
    state(s, peer_checkpoint_seq: state(s, :current_seq))
  end

  def next_state(s, _v, {:call, _, :update_drop_seq, [_db_name]}) do
    # we'll drop all tombstones if _update_drop_seq is called when there
    # are no peer checkpoint docs as the only peers are the shard syncs
    # which update automatically
    drop_seq =
      if state(s, :peer_checkpoint_seq) == nil,
        do: state(s, :current_seq),
        else: state(s, :peer_checkpoint_seq)

    state(s, drop_seq: drop_seq)
  end

  def next_state(s, _v, {:call, _, :compact_db, [_db_name]}) do
    {keep_docs, drop_docs} =
      Enum.split_with(state(s, :deleted_docs), fn {_, seq} ->
        state(s, :drop_seq) == nil or seq > state(s, :drop_seq)
      end)

    state(s,
      deleted_docs: keep_docs,
      drop_count: state(s, :drop_count) + length(drop_docs)
    )
  end

  def next_state(s, _v, {:call, _, :changes, [_db_name]}) do
    s
  end

  def postcondition(s, {:call, _, :changes, [_db_name]}, {doc_ids, del_doc_ids}) do
    doc_ids == doc_ids(s) and del_doc_ids == deleted_doc_ids(s)
  end

  def postcondition(_, _, _), do: true

  def doc_exists(s, doc_id), do: doc_id in state(s, :docs)

  def deleted_doc_exists(s, doc_id) do
    List.keymember?(state(s, :deleted_docs), doc_id, 0)
  end

  def doc_ids(s), do: state(s, :docs)

  def deleted_doc_ids(s) do
    Enum.map(state(s, :deleted_docs), fn {doc_id, _} -> doc_id end)
  end

  def seq_to_shards(seq) do
    for {node, [b, e], {seq_num, _uuid, _epoch}} <- decode_seq(seq) do
      b_hex = :couch_util.to_hex(<<b::32-integer>>)
      e_hex = :couch_util.to_hex(<<e::32-integer>>)
      range = "#{b_hex}-#{e_hex}"
      {node, range, seq_num}
    end
  end

  def decode_seq(seq) do
    seq = String.replace(seq, ~r/\d+-/, "", global: false)
    :erlang.binary_to_term(Base.url_decode64!(seq, padding: false))
  end
end

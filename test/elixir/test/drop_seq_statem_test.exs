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

  property "_update_drop_seq works correctly",
    start_size: 10,
    max_size: 100,
    numtests: 2_000 do
    forall cmds <- commands(__MODULE__) do
      trap_exit do
        db_name = random_db_name()
        n = Enum.random(1..3)
        q = Enum.random(1..10)

        {:ok, _} = create_db(db_name, query: %{n: n, q: q})
        r = run_commands(__MODULE__, cmds, [{:dbname, db_name}])
        {_history, _state, result} = r
        delete_db(db_name)

        (result == :ok)
        |> when_fail(when_fail_fn(n, q, r, cmds))
      end
    end
  end

  def when_fail_fn(n, q, r, cmds) do
    IO.puts("\nn: #{n}, q: #{q}")
    print_report(r, cmds)
  end

  defmodule State do
    defstruct docs: [],
              deleted_docs: [],
              current_seq: 0,
              peer_checkpoint_seq: nil,
              drop_seq: nil,
              drop_count: 0,
              changed: false,
              stale: false
  end

  def initial_state, do: %State{}

  @max_docids 20
  @docids 1..@max_docids |> Enum.map(&"doc-#{&1}")

  def doc_id, do: oneof(@docids)

  def index_type do
    oneof([:mrview, :nouveau])
  end

  def command(s) do
    case s do
      %State{stale: true} ->
        {:call, __MODULE__, :update_indexes, [{:var, :dbname}]}

      %State{changed: true} ->
        {:call, __MODULE__, :changes, [{:var, :dbname}]}

      %State{} ->
        frequency([
          {10, {:call, __MODULE__, :update_document, [{:var, :dbname}, doc_id()]}},
          {10, {:call, __MODULE__, :delete_document, [{:var, :dbname}, doc_id()]}},
          {10, {:call, __MODULE__, :update_peer_checkpoint, [{:var, :dbname}]}},
          {10, {:call, __MODULE__, :update_drop_seq, [{:var, :dbname}]}},
          {10, {:call, __MODULE__, :compact_db, [{:var, :dbname}]}},
          {5, {:call, __MODULE__, :split_shard, [{:var, :dbname}]}},
          {1, {:call, __MODULE__, :create_index, [{:var, :dbname}, index_type()]}}
        ])
    end
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

        assert resp.status_code == 201,
               "Couch.put failed #{resp.status_code} #{inspect(resp.body)}"

      {:not_found, _} ->
        resp = Couch.put("/#{db_name}/#{doc_id}", body: %{})

        assert resp.status_code == 201,
               "Couch.put failed #{resp.status_code} #{inspect(resp.body)}"
    end

    sync_shards(db_name)
  end

  def delete_document(db_name, doc_id) do
    case get_document(db_name, doc_id) do
      {:ok, doc} ->
        rev = doc["_rev"]
        resp = Couch.delete("/#{db_name}/#{doc_id}?rev=#{rev}")

        assert resp.status_code == 200,
               "Couch.delete failed #{resp.status_code} #{inspect(resp.body)}"

      {:not_found, _} ->
        :ok
    end

    sync_shards(db_name)
  end

  def update_peer_checkpoint(db_name) do
    resp = Couch.get("/#{db_name}")

    assert resp.status_code == 200,
           "Couch.get failed #{resp.status_code} #{inspect(resp.body)}"

    update_seq = resp.body["update_seq"]

    resp =
      Couch.put("/#{db_name}/_local/peer-checkpoint-foo",
        body: %{
          update_seq: update_seq
        }
      )

    assert resp.status_code == 201,
           "update_peer_checkpoint failed #{resp.status_code} #{inspect(resp.body)}"

    sync_shards(db_name)
    seq_to_shards(update_seq)
  end

  def update_drop_seq(db_name) do
    resp = Couch.post("/#{db_name}/_update_drop_seq")

    assert resp.status_code == 201,
           "update_drop_seq failed #{resp.status_code} #{inspect(resp.body)}"

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

  def split_shard(db_name) do
    resp = Couch.get("/#{db_name}/_shards")
    assert resp.status_code == 200
    range = Enum.random(Map.keys(resp.body["shards"]))

    resp =
      Couch.post("/_reshard/jobs",
        body: %{
          type: "split",
          db: db_name,
          range: range
        }
      )

    assert resp.status_code == 201,
           "split_shard failed #{resp.status_code} #{inspect(resp.body)}"

    retry_until(
      fn ->
        resp = Couch.get("/_reshard/jobs")
        assert resp.status_code == 200

        Enum.all?(resp.body["jobs"], fn job ->
          if job["job_state"] == "completed" do
            resp = Couch.delete("/_reshard/jobs/#{job["id"]}")
            assert resp.status_code == 200
          end

          job["job_state"] == "completed"
        end)
      end,
      200,
      10_000
    )
  end

  def create_index(db_name, index_type) do
    num = Enum.random(1..1_000_000)
    ddoc_id = "_design/#{index_type}-#{num}"

    case get_document(db_name, ddoc_id) do
      {:ok, _doc} ->
        create_index(db_name, index_type)

      {:not_found, _} ->
        :ok
    end

    doc =
      case index_type do
        :mrview ->
          %{
            views: %{
              bar: %{
                map: """
                function(doc) {
                  emit(#{num});
                }
                """
              }
            }
          }

        :nouveau ->
          %{
            nouveau: %{
              bar: %{
                index: """
                function(doc) {
                  index("double", "foo", #{num});
                }
                """
              }
            }
          }
      end

    resp = Couch.put("/#{db_name}/#{ddoc_id}", body: doc)

    assert resp.status_code == 201,
           "create_index failed #{resp.status_code} #{inspect(resp.body)}"

    sync_shards(db_name)
    ddoc_id
  end

  def update_indexes(db_name) do
    resp = Couch.get("/#{db_name}/_design_docs")
    assert resp.status_code == 200

    Enum.each(resp.body["rows"], fn row ->
      case row["id"] do
        "_design/mrview-" <> _ ->
          resp = Couch.get("/#{db_name}/#{row["id"]}/_view/bar")

          assert resp.status_code == 200,
                 "query mrview failed #{resp.status_code} #{inspect(resp.body)}"

        "_design/nouveau-" <> _ ->
          resp = Couch.get("/#{db_name}/#{row["id"]}/_nouveau/bar?q=*:*")

          assert resp.status_code == 200,
                 "query nouveau failed #{resp.status_code} #{inspect(resp.body)}"
      end
    end)
  end

  def sync_shards(db_name) do
    resp = Couch.post("/#{db_name}/_sync_shards")

    assert resp.status_code == 202,
           "sync_shards failed #{resp.status_code} #{inspect(resp.body)}"

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
    %State{
      s
      | current_seq: s.current_seq + 1,
        docs: Enum.sort([doc_id | s.docs]),
        deleted_docs: List.keydelete(s.deleted_docs, doc_id, 0),
        changed: true,
        stale: true
    }
  end

  def next_state(s, _v, {:call, _, :delete_document, [_db_name, doc_id]}) do
    %State{
      s
      | current_seq: s.current_seq + 1,
        docs: List.delete(s.docs, doc_id),
        deleted_docs: Enum.sort([{doc_id, s.current_seq + 1} | s.deleted_docs]),
        changed: true,
        stale: true
    }
  end

  def next_state(s, _v, {:call, _, :update_peer_checkpoint, [_db_name]}) do
    %State{s | peer_checkpoint_seq: s.current_seq, changed: true}
  end

  def next_state(s, _v, {:call, _, :update_drop_seq, [_db_name]}) do
    # we'll drop all tombstones if _update_drop_seq is called when there
    # are no peer checkpoint docs as the only peers are the shard syncs
    # which update automatically
    # n.b: indexes and their peer checkpoints will always be fresh as we
    # force update_indexes after every doc update.
    drop_seq =
      if s.peer_checkpoint_seq == nil,
        do: s.current_seq,
        else: s.peer_checkpoint_seq

    %State{s | drop_seq: drop_seq, changed: true}
  end

  def next_state(s, _v, {:call, _, :compact_db, [_db_name]}) do
    {keep_docs, drop_docs} =
      Enum.split_with(s.deleted_docs, fn {_, seq} ->
        s.drop_seq == nil or seq > s.drop_seq
      end)

    %State{
      s
      | deleted_docs: keep_docs,
        drop_count: s.drop_count + length(drop_docs),
        changed: true
    }
  end

  def next_state(s, _v, {:call, _, :changes, [_db_name]}) do
    %State{s | changed: false}
  end

  def next_state(s, _v, {:call, _, :split_shard, [_db_name]}) do
    %State{s | changed: true}
  end

  def next_state(s, v, {:call, _, :create_index, [_db_name, _index_type]}) do
    %State{
      s
      | current_seq: s.current_seq + 1,
        docs: Enum.sort([v | s.docs]),
        changed: true,
        stale: true
    }
  end

  def next_state(s, _v, {:call, _, :update_indexes, [_db_name]}) do
    %State{s | stale: false}
  end

  def postcondition(s, {:call, _, :changes, [_db_name]}, {doc_ids, del_doc_ids}) do
    doc_ids == doc_ids(s) and del_doc_ids == deleted_doc_ids(s)
  end

  def postcondition(_, _, _), do: true

  def doc_exists(s, doc_id), do: doc_id in s.docs

  def deleted_doc_exists(s, doc_id) do
    List.keymember?(s.deleted_docs, doc_id, 0)
  end

  def doc_ids(s), do: s.docs

  def deleted_doc_ids(s) do
    Enum.map(s.deleted_docs, fn {doc_id, _} -> doc_id end)
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

defmodule DropSeqStateM do
  use PropCheck, default_opts: &PropCheck.TestHelpers.config/0
  use PropCheck.StateM
  use CouchTestCase

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
        n = Enum.random(2..3)
        q = Enum.random(1..10)
        {:ok, _} = create_db(db_name, query: %{n: n, q: q})
        r = run_commands(__MODULE__, cmds, [{:dbname, db_name}])
        {_history, _state, result} = r
        delete_db(db_name)

        (result == :ok)
        |> when_fail(when_fail_fn(db_name, n, q, r, cmds))
      end
    end
  end

  def when_fail_fn(db_name, n, q, r, cmds) do
    IO.puts("\ndb_name: #{db_name}, n: #{n}, q: #{q}")
    print_report(r, cmds)
    print_script(n, q, cmds)
  end

  def print_script(n, q, cmds) do
    url = "http://foo:bar@localhost:15984"
    db_name = "propcheck-repro"
    db_url = "#{url}/#{db_name}"

    IO.puts("""
    #!/bin/sh
    set -e
    COUNT=0
    curl -X DELETE #{db_url} || true
    curl --fail -X PUT "#{db_url}?q=#{q}&n=#{n}"
    """)

    Enum.each(cmds, fn cmd ->
      case cmd do
        {:set, _var, {:call, __MODULE__, :update_document, [_dbname, doc_id]}} ->
          IO.puts("""
          curl --fail -X PUT "#{db_url}/#{doc_id}" -d "{}"
          """)

        {:set, _var, {:call, __MODULE__, :delete_document, [_dbname, doc_id]}} ->
          IO.puts("""
          REV=$(curl --fail -X GET "#{db_url}/#{doc_id}" | jq -r ._rev)
          curl --fail -X DELETE "#{db_url}/#{doc_id}?rev=$REV"
          """)

        {:set, _var, {:call, __MODULE__, :check_actual_state, [_dbname]}} ->
          IO.puts("""
          curl --fail -X GET "#{db_url}/_changes"
          """)

        {:set, _var, {:call, __MODULE__, :compact_db, [_dbname]}} ->
          IO.puts("""
          curl --fail -X POST "#{db_url}/_compact" -Hcontent-type:application/json
          sleep 10
          """)

        {:set, _var, {:call, __MODULE__, :update_drop_seq, [_dbname]}} ->
          IO.puts("""
          curl --fail -X POST "#{db_url}/_update_drop_seq" -Hcontent-type:application/json
          """)

        {:set, _var, {:call, __MODULE__, :update_peer_checkpoint, [_dbname]}} ->
          IO.puts("""
          SEQ=$(curl --fail -X GET "#{db_url}" | jq -r .update_seq)
          curl --fail -X PUT "#{db_url}/_local/peer-checkpoint-foo" --data-binary @- << EOF
          {
            "update_seq": "$SEQ"
          }
          EOF
          """)

        {:set, _var, {:call, __MODULE__, :delete_peer_checkpoint, [_dbname]}} ->
          IO.puts("""
          curl --fail -X DELETE "#{db_url}/_local/peer-checkpoint-foo"
          """)

        {:set, _var, {:call, __MODULE__, :split_shard, [_dbname]}} ->
          IO.puts("""
          RANGE=$(curl --fail -X GET "#{db_url}/_shards" | jq -r '.shards | keys[]' | shuf -n 1)
          curl --fail -X POST "#{url}/_reshard/jobs" -Hcontent-type:application/json --data-binary @- << EOF
          {
            "type":"split",
            "db":"#{db_name}",
            "range":"$RANGE"
          }
          EOF
          sleep 10
          """)

        {:set, _var, {:call, __MODULE__, :create_index, [_dbname, :mrview]}} ->
          IO.puts("""
          COUNT=$((COUNT+1))
          curl --fail -X PUT "#{db_url}/_design/ddoc-mrview-${COUNT}" --data-binary @- << EOF
          {
            "views": {
              "foo": {
                "map": "function(doc) { emit(#{:rand.uniform_real()}); }"
              }
            }
          }
          EOF
          """)

        {:set, _var, {:call, __MODULE__, :create_index, [_dbname, :nouveau]}} ->
          IO.puts("""
          COUNT=$((COUNT+1))
          curl --fail -X PUT "#{db_url}/_design/ddoc-nouveau-${COUNT}" --data-binary @- << EOF
          {
            "nouveau": {
              "foo": {
                "index": "function(doc) { index('double', 'foo', #{:rand.uniform_real()}); }"
              }
            }
          }
          EOF
          """)

        {:set, _var, {:call, __MODULE__, :update_indexes, [_dbname]}} ->
          IO.puts("""
          for DDOC_ID in $(curl --fail "#{db_url}/_design_docs" | jq -r .rows[].id)
          do
            if echo $DDOC_ID | grep -q nouveau
            then
              curl --fail -X GET "#{db_url}/$DDOC_ID/_nouveau/foo?q=*:*"
            fi
            if echo $DDOC_ID | grep -q mrview
            then
              curl --fail -X GET "#{db_url}/$DDOC_ID/_view/foo?limit=1"
            fi
          done
          """)
      end
    end)
  end

  defmodule State do
    defstruct docs: [],
              deleted_docs: [],
              current_seq: 0,
              peer_checkpoint_seq: nil,
              index_seq: nil,
              drop_seq: nil,
              drop_count: 0,
              check_actual_state: false
  end

  defmodule ActualState do
    defstruct docs: [],
              deleted_docs: [],
              drop_count: 0
  end

  def initial_state, do: %State{}

  @max_docids 20
  @docids 1..@max_docids |> Enum.map(&"doc-#{&1}")

  def doc_id, do: oneof(@docids)

  def doc_id(%State{docs: doc_ids}), do: elements(doc_ids)

  def index_type do
    oneof([:mrview, :nouveau])
  end

  def command(s) do
    case s do
      %State{check_actual_state: true} ->
        {:call, __MODULE__, :check_actual_state, [{:var, :dbname}]}

      %State{} ->
        # repro's restrictions in precondition function as runs fail with
        # "Precondition failed" if propcheck chooses a command that fails the
        # precondition instead of generating a new one like the docs say it should
        # clearly I'm doing something wrong to make this crap necessary. fix me.
        frequency(
          [
            {10, {:call, __MODULE__, :update_document, [{:var, :dbname}, doc_id()]}},
            {10, {:call, __MODULE__, :update_peer_checkpoint, [{:var, :dbname}]}},
            {10, {:call, __MODULE__, :compact_db, [{:var, :dbname}]}},
            {5, {:call, __MODULE__, :split_shard, [{:var, :dbname}]}},
            {1, {:call, __MODULE__, :create_index, [{:var, :dbname}, index_type()]}},
            {5, {:call, __MODULE__, :update_indexes, [{:var, :dbname}]}}
          ] ++
            for cmd <- [
                  {10,
                   {:call, __MODULE__, :delete_document, [{:var, :dbname}, doc_id(s)]}}
                ],
                s.docs != [] do
              cmd
            end ++
            for cmd <- [
                  {10, {:call, __MODULE__, :update_drop_seq, [{:var, :dbname}]}}
                ],
                s.docs != [] or s.deleted_docs != [] do
              cmd
            end ++
            for cmd <- [
                  {10, {:call, __MODULE__, :delete_peer_checkpoint, [{:var, :dbname}]}}
                ],
                s.peer_checkpoint_seq != nil do
              cmd
            end
        )
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

    wait_for_internal_replication(db_name)
    get_range(db_name, doc_id)
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

    wait_for_internal_replication(db_name)
    get_range(db_name, doc_id)
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

    wait_for_internal_replication(db_name)
    seq_to_shards(update_seq)
  end

  def delete_peer_checkpoint(db_name) do
    resp = Couch.delete("/#{db_name}/_local/peer-checkpoint-foo")
    assert resp.status_code == 200
    wait_for_internal_replication(db_name)
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
    :timer.sleep(500)
  end

  def check_actual_state(db_name) do
    resp = Couch.get("/#{db_name}/")
    assert resp.status_code == 200

    # update_seq = String.to_integer(List.first(String.split(resp.body["update_seq"], "-")))
    drop_count = resp.body["drop_count"]

    resp = Couch.get("/#{db_name}/_changes")
    assert resp.status_code == 200

    acc0 = %ActualState{drop_count: drop_count}

    List.foldl(resp.body["results"], acc0, fn change, acc1 ->
      cond do
        String.starts_with?(change["id"], "_design/") ->
          acc1

        change["deleted"] ->
          %ActualState{acc1 | deleted_docs: Enum.sort([change["id"] | acc1.deleted_docs])}

        true ->
          %ActualState{acc1 | docs: Enum.sort([change["id"] | acc1.docs])}
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

    range
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

    wait_for_internal_replication(db_name)
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

  def wait_for_internal_replication(db_name) do
    resp = Couch.post("/#{db_name}/_sync_shards")

    assert resp.status_code == 202,
           "sync_shards failed #{resp.status_code} #{inspect(resp.body)}"

    # mem3_rep configured for 100ms frequency
    :timer.sleep(3000)
  end

  def get_range(db_name, doc_id) do
    resp = Couch.get("/#{db_name}/_shards/#{doc_id}")
    assert resp.status_code == 200
    resp.body["range"]
  end

  def precondition(s, {:call, _, :update_document, [_db_name, doc_id]}) do
    not doc_exists(s, doc_id)
  end

  def precondition(s, {:call, _, :delete_document, [_db_name, doc_id]}) do
    doc_exists(s, doc_id)
  end

  def precondition(s, {:call, _, :update_drop_seq, [_db_name]}) do
    s.docs != [] or s.deleted_docs != []
  end

  def precondition(s, {:call, _, :update_indexes, [_db_name]}) do
    s.index_seq != nil
  end

  def precondition(s, {:call, _, :delete_peer_checkpoint, [_db_name]}) do
    s.peer_checkpoint_seq != nil
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
        check_actual_state: true
    }
  end

  def next_state(s, _v, {:call, _, :delete_document, [_db_name, doc_id]}) do
    %State{
      s
      | current_seq: s.current_seq + 1,
        docs: List.delete(s.docs, doc_id),
        deleted_docs: Enum.sort([{doc_id, s.current_seq + 1} | s.deleted_docs]),
        check_actual_state: true
    }
  end

  def next_state(s, _v, {:call, _, :update_peer_checkpoint, [_db_name]}) do
    %State{
      s
      | peer_checkpoint_seq: s.current_seq,
        check_actual_state: true
    }
  end

  def next_state(s, _v, {:call, _, :delete_peer_checkpoint, [_db_name]}) do
    %State{
      s
      | peer_checkpoint_seq: nil,
        check_actual_state: true
    }
  end

  def next_state(s, _v, {:call, _, :update_drop_seq, [_db_name]}) do
    drop_seq =
      cond do
        s.peer_checkpoint_seq != nil and s.index_seq != nil ->
          min(s.peer_checkpoint_seq, s.index_seq)

        s.index_seq != nil ->
          s.index_seq

        s.peer_checkpoint_seq != nil ->
          s.peer_checkpoint_seq

        true ->
          s.current_seq
      end

    %State{
      s
      | drop_seq: drop_seq,
        check_actual_state: true
    }
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
        check_actual_state: true
    }
  end

  def next_state(s, _v, {:call, _, :check_actual_state, [_db_name]}) do
    %State{s | check_actual_state: false}
  end

  def next_state(s = %State{index_seq: nil}, _v, {:call, _, :split_shard, [_db_name]}) do
    %State{s | check_actual_state: true}
  end

  def next_state(s, _v, {:call, _, :split_shard, [_db_name]}) do
    # _reshard/jobs forcibly updates all indexes
    %State{s | index_seq: s.current_seq, check_actual_state: true}
  end

  def next_state(s, _v, {:call, _, :create_index, [_db_name, _index_type]}) do
    %State{
      s
      | current_seq: s.current_seq + 1,
        check_actual_state: true
    }
  end

  def next_state(s, _v, {:call, _, :update_indexes, [_db_name]}) do
    %State{s | index_seq: s.current_seq, check_actual_state: true}
  end

  def postcondition(s, {:call, _, :check_actual_state, [_db_name]}, actual) do
    doc_ids(s) == actual.docs and deleted_doc_ids(s) == actual.deleted_docs and
      s.drop_count == actual.drop_count
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

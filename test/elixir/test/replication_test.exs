defmodule ReplicationTest do
  use CouchTestCase

  @moduledoc """
  Test CouchDB View Collation Behavior
  This is a port of the view_collation.js suite
  """

  # TODO: Parameterize these
  @admin_account "adm:pass"
  # @db_pairs_prefixes [
  #   {"local-to-local", "", ""},
  #   {"remote-to-local", "http://localhost:15984/", ""},
  #   {"local-to-remote", "", "http://localhost:15984/"},
  #   {"remote-to-remote", "http://localhost:15984/", "http://localhost:15984/"}
  # ]
  @db_pairs_prefixes [{"local-to-local", "", ""}]

  # @tag config: [
  #   {"replicator", "startup_jitter", "0"}
  # ]
  # test "source database does not exist" do
  #   name = random_db_name()
  #   check_not_found(name <> "_src", name <> "_tgt")
  # end
  #
  # @tag config: [
  #   {"replicator", "startup_jitter", "0"}
  # ]
  # test "source database not found with path - COUCHDB-317" do
  #   name = random_db_name()
  #   check_not_found(name <> "_src", name <> "_tgt")
  # end
  #
  # @tag config: [
  #   {"replicator", "startup_jitter", "0"}
  # ]
  # test "source database not found with host" do
  #   name = random_db_name()
  #   url = "http://localhost:15984/" <> name <> "_src"
  #   check_not_found(url, name <> "_tgt")
  # end

  def check_not_found(src, tgt) do
    body = %{:source => src, :target => tgt}
    resp = Couch.post("/_replicate", body: body)
    assert resp.body["error"] == "db_not_found"
  end

  Enum.each(@db_pairs_prefixes, fn {name, src_prefix, tgt_prefix} ->
    @src_prefix src_prefix
    @tgt_prefix tgt_prefix
    @tag config: [
      {"replicator", "startup_jitter", "0"}
    ]
    # test "simple #{name} replication" do
    #   run_simple_repl(@src_prefix, @tgt_prefix)
    # end
    # test "replicate with since_seq - #{name}" do
    #   run_since_seq_repl(@src_prefix, @tgt_prefix)
    # end
    # test "validate_doc_update failure replications - #{name}" do
    #   run_vdu_repl(@src_prefix, @tgt_prefix)
    # end
    test "create_target filter option - #{name}" do
      run_create_target_repl(@src_prefix, @tgt_prefix)
    end
  end)

  def run_simple_repl(src_prefix, tgt_prefix) do
    base_db_name = random_db_name()
    src_db_name = base_db_name <> "_src"
    tgt_db_name = base_db_name <> "_tgt"

    create_db(src_db_name)
    create_db(tgt_db_name)

    on_exit(fn -> delete_db(src_db_name) end)
    on_exit(fn -> delete_db(tgt_db_name) end)

    att1_data = get_att1_data()
    att2_data = get_att2_data()

    ddoc = %{
      :_id => "_design/foo",
      :language => "javascript",
      :value => "ddoc"
    }
    docs = make_docs(1..20) ++ [ddoc]
    docs = save_docs(src_db_name, docs)

    docs = for doc <- docs do
      if doc[:integer] >= 10 and doc[:integer] < 15 do
        add_attachment(src_db_name, doc, body: att1_data)
      else
        doc
      end
    end

    result = replicate(src_prefix <> src_db_name, tgt_prefix <> tgt_db_name)
    assert result["ok"]

    src_info = get_db_info(src_db_name)
    tgt_info = get_db_info(tgt_db_name)

    assert src_info["doc_count"] == tgt_info["doc_count"]

    assert is_binary(result["session_id"])
    assert is_list(result["history"])
    assert length(result["history"]) == 1
    history = Enum.at(result["history"], 0)
    assert is_binary(history["start_time"])
    assert is_binary(history["end_time"])
    assert history["start_last_seq"] == 0
    assert history["missing_checked"] == src_info["doc_count"]
    assert history["missing_found"] == src_info["doc_count"]
    assert history["docs_read"] == src_info["doc_count"]
    assert history["docs_written"] == src_info["doc_count"]
    assert history["doc_write_failures"] == 0

    for doc <- docs do
      resp = Couch.get!("/#{tgt_db_name}/#{doc[:_id]}")
      copy = to_atoms(resp.body)
      assert cmp_json(doc, copy)

      if doc[:integer] >= 10 and doc[:integer] < 15 do
        atts = copy[:"_attachments"]
        assert is_map(atts)
        att = atts[:"readme.txt"]
        assert is_map(att)
        assert att[:revpos] == 2
        assert String.match?(att[:content_type], ~r/text\/plain/)
        assert att[:stub]

        resp = Couch.get!("/#{tgt_db_name}/#{copy[:_id]}/readme.txt")
        assert String.length(resp.body) == String.length(att1_data)
        assert resp.body == att1_data
      end
    end

    # Add one more doc to source and more attachments to existing docs
    new_doc = %{:_id => "foo666", :value => "d"}
    [new_doc] = save_docs(src_db_name, [new_doc])

    docs = for doc <- docs do
      if doc[:integer] >= 10 and doc[:integer] < 15 do
        ctype = "application/binary"
        opts = [name: "data.dat", body: att2_data, content_type: ctype]
        add_attachment(src_db_name, doc, opts)
      else
        doc
      end
    end

    result = replicate(src_prefix <> src_db_name, tgt_prefix <> tgt_db_name)
    assert result["ok"]

    src_info = get_db_info(src_db_name)
    tgt_info = get_db_info(tgt_db_name)

    assert tgt_info["doc_count"] == src_info["doc_count"]

    assert is_binary(result["session_id"])
    assert is_list(result["history"])
    assert length(result["history"]) == 2
    history = Enum.at(result["history"], 0)
    assert history["session_id"] == result["session_id"]
    assert is_binary(history["start_time"])
    assert is_binary(history["end_time"])
    assert history["missing_checked"] == 6
    assert history["missing_found"] == 6
    assert history["docs_read"] == 6
    assert history["docs_written"] == 6
    assert history["doc_write_failures"] == 0

    resp = Couch.get!("/#{tgt_db_name}/#{new_doc[:_id]}")
    copy = to_atoms(resp.body)
    assert copy[:_id] == new_doc[:_id]
    assert copy[:value] == new_doc[:value]

    for i <- 10..14 do
      doc = Enum.at(docs, i - 1)
      resp = Couch.get!("/#{tgt_db_name}/#{i}")
      copy = to_atoms(resp.body)
      assert cmp_json(doc, copy)

      atts = copy[:_attachments]
      assert is_map(atts)
      att = atts[:"readme.txt"]
      assert is_map(atts)
      assert att[:revpos] == 2
      assert String.match?(att[:content_type], ~r/text\/plain/)
      assert att[:stub]

      resp = Couch.get!("/#{tgt_db_name}/#{i}/readme.txt")
      assert String.length(resp.body) == String.length(att1_data)
      assert resp.body == att1_data

      att = atts[:"data.dat"]
      assert is_map(att)
      assert att[:revpos] == 3
      assert String.match?(att[:content_type], ~r/application\/binary/)
      assert att[:stub]

      resp = Couch.get!("/#{tgt_db_name}/#{i}/data.dat")
      assert String.length(resp.body) == String.length(att2_data)
      assert resp.body == att2_data
    end

    # Test deletion is replicated
    del_doc = %{
      :_id => "1",
      :_rev => Enum.at(docs, 0)[:_rev],
      :_deleted => true
    }
    [del_doc] = save_docs(src_db_name, [del_doc])

    result = replicate(src_prefix <> src_db_name, tgt_prefix <> tgt_db_name)
    assert result["ok"]

    src_info = get_db_info(src_db_name)
    tgt_info = get_db_info(tgt_db_name)

    assert tgt_info["doc_count"] == src_info["doc_count"]
    assert tgt_info["doc_del_count"] == src_info["doc_del_count"]
    assert tgt_info["doc_del_count"] == 1

    assert is_list(result["history"])
    assert length(result["history"]) == 3
    history = Enum.at(result["history"], 0)
    assert history["missing_checked"] == 1
    assert history["missing_found"] == 1
    assert history["docs_read"] == 1
    assert history["docs_written"] == 1
    assert history["doc_write_failures"] == 0

    resp = Couch.get("/#{tgt_db_name}/#{del_doc[:_id]}")
    assert resp.status_code == 404

    resp = Couch.get!("/#{tgt_db_name}/_changes")
    [change] = Enum.filter(resp.body["results"], &(&1["id"] == del_doc[:_id]))
    assert change["id"] == del_doc[:_id]
    assert change["deleted"]

    # Test replicating a conflict
    doc = Couch.get!("/#{src_db_name}/2").body
    [doc] = save_docs(src_db_name, [Map.put(doc, :value, "white")])

    copy = Couch.get!("/#{tgt_db_name}/2").body
    save_docs(tgt_db_name, [Map.put(copy, :value, "black")])

    result = replicate(src_prefix <> src_db_name, tgt_prefix <> tgt_db_name)
    assert result["ok"]

    src_info = get_db_info(src_db_name)
    tgt_info = get_db_info(tgt_db_name)

    assert tgt_info["doc_count"] == src_info["doc_count"]

    assert is_list(result["history"])
    assert length(result["history"]) == 4
    history = Enum.at(result["history"], 0)
    assert history["missing_checked"] == 1
    assert history["missing_found"] == 1
    assert history["docs_read"] == 1
    assert history["docs_written"] == 1
    assert history["doc_write_failures"] == 0

    copy = Couch.get!("/#{tgt_db_name}/2", query: %{:conflicts => true}).body
    assert String.match?(copy["_rev"], ~r/^2-/)
    assert is_list(copy["_conflicts"])
    assert length(copy["_conflicts"]) == 1
    conflict = Enum.at(copy["_conflicts"], 0)
    assert String.match?(conflict, ~r/^2-/)

    # Re-replicate updated conflict
    [doc] = save_docs(src_db_name, [Map.put(doc, :value, "yellow")])

    result = replicate(src_prefix <> src_db_name, tgt_prefix <> tgt_db_name)
    assert result["ok"]

    src_info = get_db_info(src_db_name)
    tgt_info = get_db_info(tgt_db_name)

    assert tgt_info["doc_count"] == src_info["doc_count"]

    assert is_list(result["history"])
    assert length(result["history"]) == 5
    history = Enum.at(result["history"], 0)
    assert history["missing_checked"] == 1
    assert history["missing_found"] == 1
    assert history["docs_read"] == 1
    assert history["docs_written"] == 1
    assert history["doc_write_failures"] == 0

    copy = Couch.get!("/#{tgt_db_name}/2", query: %{:conflicts => true}).body
    assert String.match?(copy["_rev"], ~r/^3-/)
    assert is_list(copy["_conflicts"])
    assert length(copy["_conflicts"]) == 1
    conflict = Enum.at(copy["_conflicts"], 0)
    assert String.match?(conflict, ~r/^2-/)

    # Resolve the conflict and re-replicate new revision
    resolve_doc = %{:_id => "2", :_rev => conflict, :_deleted => true}
    save_docs(tgt_db_name, [resolve_doc])
    save_docs(src_db_name, [Map.put(doc, :value, "rainbow")])

    result = replicate(src_prefix <> src_db_name, tgt_prefix <> tgt_db_name)
    assert result["ok"]

    src_info = get_db_info(src_db_name)
    tgt_info = get_db_info(tgt_db_name)

    assert tgt_info["doc_count"] == src_info["doc_count"]

    assert is_list(result["history"])
    assert length(result["history"]) == 6
    history = Enum.at(result["history"], 0)
    assert history["missing_checked"] == 1
    assert history["missing_found"] == 1
    assert history["docs_read"] == 1
    assert history["docs_written"] == 1
    assert history["doc_write_failures"] == 0

    copy = Couch.get!("/#{tgt_db_name}/2", query: %{:conflicts => true}).body

    assert String.match?(copy["_rev"], ~r/^4-/)
    assert not Map.has_key?(copy, "_conflicts")

    # Test that existing revisions are not replicated
    src_docs = [
      %{:_id => "foo1", :value => 111},
      %{:_id => "foo2", :value => 222},
      %{:_id => "foo3", :value => 333}
    ]
    save_docs(src_db_name, src_docs)
    save_docs(tgt_db_name, Enum.filter(src_docs, &(&1[:_id] != "foo2")))

    result = replicate(src_prefix <> src_db_name, tgt_prefix <> tgt_db_name)
    assert result["ok"]

    src_info = get_db_info(src_db_name)
    tgt_info = get_db_info(tgt_db_name)

    assert tgt_info["doc_count"] == src_info["doc_count"]

    assert is_list(result["history"])
    assert length(result["history"]) == 7
    history = Enum.at(result["history"], 0)
    assert history["missing_checked"] == 3
    assert history["missing_found"] == 1
    assert history["docs_read"] == 1
    assert history["docs_written"] == 1
    assert history["doc_write_failures"] == 0

    docs = [
      %{:_id => "foo4", :value => 444},
      %{:_id => "foo5", :value => 555}
    ]
    save_docs(src_db_name, docs)
    save_docs(tgt_db_name, docs)

    result = replicate(src_prefix <> src_db_name, tgt_prefix <> tgt_db_name)
    assert result["ok"]

    src_info = get_db_info(src_db_name)
    tgt_info = get_db_info(tgt_db_name)

    assert tgt_info["doc_count"] == src_info["doc_count"]

    assert is_list(result["history"])
    assert length(result["history"]) == 8
    history = Enum.at(result["history"], 0)
    assert history["missing_checked"] == 2
    assert history["missing_found"] == 0
    assert history["docs_read"] == 0
    assert history["docs_written"] == 0
    assert history["doc_write_failures"] == 0

    # Test nothing to replicate
    result = replicate(src_prefix <> src_db_name, tgt_prefix <> tgt_db_name)
    assert result["ok"]
    assert result["no_changes"]
  end

  def run_since_seq_repl(src_prefix, tgt_prefix) do
    base_db_name = random_db_name()
    src_db_name = base_db_name <> "_src"
    tgt_db_name = base_db_name <> "_tgt"
    repl_src = src_prefix <> src_db_name
    repl_tgt = tgt_prefix <> tgt_db_name

    create_db(src_db_name)
    create_db(tgt_db_name)

    on_exit(fn -> delete_db(src_db_name) end)
    on_exit(fn -> delete_db(tgt_db_name) end)

    docs = make_docs(1..5)
    docs = save_docs(src_db_name, docs)

    changes = get_db_changes(src_db_name)["results"]
    since_seq = Enum.at(changes, 2)["seq"]

    # TODO: In JS we re-fetch _changes with since_seq, is that
    # really necessary?
    expected_ids = for change <- Enum.drop(changes, 3) do
      change["id"]
    end
    assert length(expected_ids) == 2

    cancel_replication(repl_src, repl_tgt)
    result = replicate(repl_src, repl_tgt, body: %{:since_seq => since_seq})
    cancel_replication(repl_src, repl_tgt)

    assert result["ok"]
    assert is_list(result["history"])
    history = Enum.at(result["history"], 0)
    assert history["missing_checked"] == 2
    assert history["missing_found"] == 2
    assert history["docs_read"] == 2
    assert history["docs_written"] == 2
    assert history["doc_write_failures"] == 0

    Enum.each(docs, fn doc ->
      result = Couch.get("/#{tgt_db_name}/#{doc[:_id]}")
      if Enum.member?(expected_ids, doc[:_id]) do
        assert result.status_code < 300
        assert cmp_json(doc, to_atoms(result.body))
      else
        assert result.status_code == 404
      end
    end)
  end

  def run_vdu_repl(src_prefix, tgt_prefix) do
    base_db_name = random_db_name()
    src_db_name = base_db_name <> "_src"
    tgt_db_name = base_db_name <> "_tgt"
    repl_src = src_prefix <> src_db_name
    repl_tgt = tgt_prefix <> tgt_db_name

    create_db(src_db_name)
    create_db(tgt_db_name)

    on_exit(fn -> delete_db(src_db_name) end)
    on_exit(fn -> delete_db(tgt_db_name) end)

    docs = make_docs(1..7)
    docs = for doc <- docs do
      if doc[:integer] == 2 do
        Map.put(doc, "_attachments", %{
          "hello.txt" => %{
            :content_type => "text/plain",
            :data => "aGVsbG8gd29ybGQ=" # base64:encode("hello world")
          }
        })
      else
        doc
      end
    end
    docs = save_docs(src_db_name, docs)

    ddoc = %{
      :_id => "_design/test",
      :language => "javascript",
      :validate_doc_update => """
        function(newDoc, oldDoc, userCtx, secObj) {
          if((newDoc.integer % 2) !== 0) {
            throw {forbidden: "I only like multiples of 2."};
          }
        }
      """
    }
    [_] = save_docs(tgt_db_name, [ddoc])

    result = replicate(repl_src, repl_tgt)
    assert result["ok"]

    assert is_list(result["history"])
    history = Enum.at(result["history"], 0)
    assert history["missing_checked"] == 7
    assert history["missing_found"] == 7
    assert history["docs_read"] == 7
    assert history["docs_written"] == 3
    assert history["doc_write_failures"] == 4

    for doc <- docs do
      result = Couch.get("/#{tgt_db_name}/#{doc[:_id]}")
      if rem(doc[:integer], 2) == 0 do
        assert result.status_code < 300
        assert result.body["integer"] == doc[:integer]
      else
        assert result.status_code == 404
      end
    end
  end

  def run_create_target_repl(src_prefix, tgt_prefix) do
    base_db_name = random_db_name()
    src_db_name = base_db_name <> "_src"
    tgt_db_name = base_db_name <> "_tgt"
    repl_src = src_prefix <> src_db_name
    repl_tgt = tgt_prefix <> tgt_db_name

    create_db(src_db_name)

    on_exit(fn -> delete_db(src_db_name) end)
    # This is created by the replication
    on_exit(fn -> delete_db(tgt_db_name) end)

    docs = make_docs(1..2)
    save_docs(src_db_name, docs)

    replicate(repl_src, repl_tgt, body: %{:create_target => true})

    src_info = get_db_info(src_db_name)
    tgt_info = get_db_info(tgt_db_name)

    assert tgt_info["doc_count"] == src_info["doc_count"]

    src_shards = seq_to_shards(src_info["update_seq"])
    tgt_shards = seq_to_shards(tgt_info["update_seq"])
    assert tgt_shards == src_shards
  end


  def get_db_info(db_name) do
    resp = Couch.get("/#{db_name}")
    assert HTTPotion.Response.success?(resp)
    resp.body
  end

  def replicate(src, tgt, options \\ []) do
    src = maybe_add_creds(src)
    tgt = maybe_add_creds(tgt)
    defaults = [headers: [], body: %{}]
    options = Keyword.merge(defaults, options) |> Enum.into(%{})
    %{headers: headers, body: body} = options
    body = [source: src, target: tgt] |> Enum.into(body)
    opts = [headers: headers, body: body, timeout: 30_000]
    resp = Couch.post("/_replicate", opts)
    assert HTTPotion.Response.success?(resp), "#{inspect resp}"
    resp.body
  end

  def cancel_replication(src, tgt) do
    body = %{:cancel => true}
    try do
      replicate(src, tgt, body: body)
    rescue
      ExUnit.AssertionError -> :ok
    end
  end

  def get_db_changes(db_name) do
    resp = Couch.get("/#{db_name}/_changes")
    assert HTTPotion.Response.success?(resp), "#{inspect resp}"
    resp.body
  end

  def save_docs(db_name, docs) do
    query = %{w: 3}
    body = %{docs: docs}
    resp = Couch.post("/#{db_name}/_bulk_docs", query: query, body: body)
    assert HTTPotion.Response.success?(resp)
    for {doc, resp} <- Enum.zip(docs, resp.body) do
      Map.put(doc, :_rev, resp["rev"])
    end
  end

  def add_attachment(db_name, doc, att \\ []) do
    defaults = [
      name: <<"readme.txt">>,
      body: <<"some text">>,
      content_type: "text/plain"
    ]
    att = Keyword.merge(defaults, att) |> Enum.into(%{})
    uri = "/#{db_name}/#{URI.encode(doc[:_id])}/#{att[:name]}"
    headers = ["Content-Type": att[:content_type]]
    params = if doc[:_rev] do
      %{:w => 3, :rev => doc[:_rev]}
    else
      %{:w => 3}
    end
    resp = Couch.put(uri, headers: headers, query: params, body: att[:body])
    assert HTTPotion.Response.success?(resp)
    Map.put(doc, :_rev, resp.body["rev"])
  end

  def make_docs(ids) do
    for id <- ids, str_id = Integer.to_string(id) do
      %{:_id => str_id, :integer => id, :string => str_id}
    end
  end

  def maybe_add_creds(uri) do
    case URI.parse(uri) do
      %{scheme: nil} ->
        uri
      %{userinfo: nil} = uri ->
        URI.to_string(Map.put(uri, :userinfo, @admin_account))
      _ ->
        uri
    end
  end

  def get_att1_data do
    File.read!("test/data/lorem.txt")
  end

  def get_att2_data do
    File.read!("test/data/lorem_b64.txt")
  end

  def cmp_json(lhs, rhs) when is_map(lhs) and is_map(rhs) do
    Enum.reduce_while(lhs, true, fn {k, v}, true ->
      if Map.has_key?(rhs, k) do
        if cmp_json(v, rhs[k]) do
          {:cont, true}
        else
          {:halt, false}
        end
      else
        {:halt, false}
      end
    end)
  end

  def cmp_json(lhs, rhs), do: lhs == rhs

  def seq_to_shards(seq) do
    for {_node, range, update_seq} <- decode_seq(seq) do
      {range, update_seq}
    end
  end

  def decode_seq(seq) do
    seq = String.replace(seq, ~r/\d+-/, "", global: false)
    :erlang.binary_to_term(Base.url_decode64!(seq, padding: false))
  end

  def to_atoms(json) when is_map(json) do
    Enum.map(json, fn {k, v} ->
      {String.to_atom(k), to_atoms(v)}
    end) |> Enum.into(%{})
  end

  def to_atoms(json) when is_list(json) do
    Enum.map(json, &(to_atoms(&1)))
  end

  def to_atoms(json) do
    json
  end
end

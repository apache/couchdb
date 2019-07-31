defmodule ReplicationTest do
  use CouchTestCase

  @moduledoc """
  Test CouchDB View Collation Behavior
  This is a port of the view_collation.js suite
  """

  # TODO: Parameterize these
  @admin_account "adm:pass"
  @db_pairs_prefixes [
    {"remote-to-remote", "http://127.0.0.1:15984/", "http://127.0.0.1:15984/"}
  ]

  # This should probably go into `make elixir` like what
  # happens for JavaScript tests.
  @moduletag config: [{"replicator", "startup_jitter", "0"}]

  @moduletag :skip_on_jenkins

  test "source database not found with host" do
    name = random_db_name()
    src_url = "http://127.0.0.1:15984/" <> name <> "_src"
    tgt_url = "http://127.0.0.1:15984/" <> name <> "_tgt"
    check_not_found(src_url, tgt_url)
  end

  def check_not_found(src, tgt) do
    body = %{:source => src, :target => tgt}
    resp = Couch.post("/_replicate", body: body)
    assert resp.body["error"] == "db_not_found"
  end

  test "replicating attachment without conflict - COUCHDB-885" do
    name = random_db_name()
    src_db_name = name <> "_src"
    tgt_db_name = name <> "_tgt"

    create_db(src_db_name)
    create_db(tgt_db_name)
    delete_on_exit([src_db_name, tgt_db_name])

    doc = %{"_id" => "doc1"}
    [doc] = save_docs(src_db_name, [doc])

    repl_src = "http://127.0.0.1:15984/" <> src_db_name
    repl_tgt = "http://127.0.0.1:15984/" <> tgt_db_name
    result = replicate(repl_src, repl_tgt)
    assert result["ok"]
    assert is_list(result["history"])
    history = Enum.at(result["history"], 0)
    assert history["docs_written"] == 1
    assert history["docs_read"] == 1
    assert history["doc_write_failures"] == 0

    doc =
      Map.put(doc, "_attachments", %{
        "hello.txt" => %{
          "content_type" => "text/plain",
          # base64:encode("hello world")
          "data" => "aGVsbG8gd29ybGQ="
        },
        "foo.dat" => %{
          "content_type" => "not/compressible",
          # base64:encode("i am not gziped")
          "data" => "aSBhbSBub3QgZ3ppcGVk"
        }
      })

    [doc] = save_docs(src_db_name, [doc])

    repl_src = "http://127.0.0.1:15984/" <> src_db_name
    repl_tgt = "http://127.0.0.1:15984/" <> tgt_db_name
    result = replicate(repl_src, repl_tgt)

    assert result["ok"]
    assert is_list(result["history"])
    assert length(result["history"]) == 2
    history = Enum.at(result["history"], 0)
    assert history["docs_written"] == 1
    assert history["docs_read"] == 1
    assert history["doc_write_failures"] == 0

    query = %{
      :conflicts => true,
      :deleted_conflicts => true,
      :attachments => true,
      :att_encoding_info => true
    }

    opts = [headers: [Accept: "application/json"], query: query]
    resp = Couch.get("/#{tgt_db_name}/#{doc["_id"]}", opts)
    assert HTTPotion.Response.success?(resp)
    assert is_map(resp.body)
    refute Map.has_key?(resp.body, "_conflicts")
    refute Map.has_key?(resp.body, "_deleted_conflicts")

    atts = resp.body["_attachments"]

    assert atts["hello.txt"]["content_type"] == "text/plain"
    assert atts["hello.txt"]["data"] == "aGVsbG8gd29ybGQ="
    assert atts["hello.txt"]["encoding"] == "gzip"

    assert atts["foo.dat"]["content_type"] == "not/compressible"
    assert atts["foo.dat"]["data"] == "aSBhbSBub3QgZ3ppcGVk"
    refute Map.has_key?(atts["foo.dat"], "encoding")
  end

  test "replication cancellation" do
    name = random_db_name()
    src_db_name = name <> "_src"
    tgt_db_name = name <> "_tgt"

    create_db(src_db_name)
    create_db(tgt_db_name)
    delete_on_exit([src_db_name, tgt_db_name])

    save_docs(src_db_name, make_docs(1..6))

    repl_body = %{:continuous => true, :create_target => true}
    repl_src = "http://127.0.0.1:15984/" <> src_db_name
    repl_tgt = "http://127.0.0.1:15984/" <> tgt_db_name
    result = replicate(repl_src, repl_tgt, body: repl_body)

    assert result["ok"]
    assert is_binary(result["_local_id"])
    repl_id = result["_local_id"]

    task = get_task(repl_id, 3_000)
    assert is_map(task)

    assert task["replication_id"] == repl_id

    repl_body = %{
      "replication_id" => repl_id,
      cancel: true
    }

    result = Couch.post("/_replicate", body: repl_body)
    assert result.status_code == 200

    wait_for_repl_stop(repl_id)

    assert get_task(repl_id, 0) == nil

    result = Couch.post("/_replicate", body: repl_body)
    assert result.status_code == 404
  end

  @tag user: [name: "joe", password: "erly", roles: ["erlanger"]]
  test "unauthorized replication cancellation", ctx do
    name = random_db_name()
    src_db_name = name <> "_src"
    tgt_db_name = name <> "_tgt"

    create_db(src_db_name)
    create_db(tgt_db_name)
    delete_on_exit([src_db_name, tgt_db_name])

    save_docs(src_db_name, make_docs(1..6))

    repl_src = "http://127.0.0.1:15984/" <> src_db_name
    repl_tgt = "http://127.0.0.1:15984/" <> tgt_db_name
    repl_body = %{"continuous" => true}
    result = replicate(repl_src, repl_tgt, body: repl_body)

    assert result["ok"]
    assert is_binary(result["_local_id"])
    repl_id = result["_local_id"]

    task = get_task(repl_id, 5_000)
    assert is_map(task)

    sess = Couch.login(ctx[:userinfo])
    resp = Couch.Session.get(sess, "/_session")
    assert resp.body["ok"]
    assert resp.body["userCtx"]["name"] == "joe"

    repl_body = %{
      "replication_id" => repl_id,
      cancel: true
    }

    resp = Couch.Session.post(sess, "/_replicate", body: repl_body)
    assert resp.status_code == 401
    assert resp.body["error"] == "unauthorized"

    assert Couch.Session.logout(sess).body["ok"]

    resp = Couch.post("/_replicate", body: repl_body)
    assert resp.status_code == 200
  end

  Enum.each(@db_pairs_prefixes, fn {name, src_prefix, tgt_prefix} ->
    @src_prefix src_prefix
    @tgt_prefix tgt_prefix

    test "simple #{name} replication - #{name}" do
      run_simple_repl(@src_prefix, @tgt_prefix)
    end

    test "replicate with since_seq - #{name}" do
      run_since_seq_repl(@src_prefix, @tgt_prefix)
    end

    test "validate_doc_update failure replications - #{name}" do
      run_vdu_repl(@src_prefix, @tgt_prefix)
    end

    test "create_target filter option - #{name}" do
      run_create_target_repl(@src_prefix, @tgt_prefix)
    end

    test "filtered replications - #{name}" do
      run_filtered_repl(@src_prefix, @tgt_prefix)
    end

    test "replication restarts after filter change - COUCHDB-892 - #{name}" do
      run_filter_changed_repl(@src_prefix, @tgt_prefix)
    end

    test "replication by doc ids - #{name}" do
      run_by_id_repl(@src_prefix, @tgt_prefix)
    end

    test "continuous replication - #{name}" do
      run_continuous_repl(@src_prefix, @tgt_prefix)
    end

    @tag config: [
           {"attachments", "compression_level", "8"},
           {"attachments", "compressible_types", "text/*"}
         ]
    test "compressed attachment replication - #{name}" do
      run_compressed_att_repl(@src_prefix, @tgt_prefix)
    end

    @tag user: [name: "joe", password: "erly", roles: ["erlanger"]]
    test "non-admin user on target - #{name}", ctx do
      run_non_admin_target_user_repl(@src_prefix, @tgt_prefix, ctx)
    end

    @tag user: [name: "joe", password: "erly", roles: ["erlanger"]]
    test "non-admin or reader user on source - #{name}", ctx do
      run_non_admin_or_reader_source_user_repl(@src_prefix, @tgt_prefix, ctx)
    end
  end)

  def run_simple_repl(src_prefix, tgt_prefix) do
    base_db_name = random_db_name()
    src_db_name = base_db_name <> "_src"
    tgt_db_name = base_db_name <> "_tgt"

    create_db(src_db_name)
    create_db(tgt_db_name)
    delete_on_exit([src_db_name, tgt_db_name])

    att1_data = get_att1_data()
    att2_data = get_att2_data()

    ddoc = %{
      "_id" => "_design/foo",
      "language" => "javascript",
      "value" => "ddoc"
    }

    docs = make_docs(1..20) ++ [ddoc]
    docs = save_docs(src_db_name, docs)

    docs =
      for doc <- docs do
        if doc["integer"] >= 10 and doc["integer"] < 15 do
          add_attachment(src_db_name, doc, body: att1_data)
        else
          doc
        end
      end

    repl_src = src_prefix <> src_db_name
    repl_tgt = tgt_prefix <> tgt_db_name
    result = replicate(repl_src, repl_tgt)
    assert result["ok"]

    src_info =
      retry_until(fn ->
        src_info = get_db_info(src_db_name)
        tgt_info = get_db_info(tgt_db_name)

        assert src_info["doc_count"] == tgt_info["doc_count"]
        src_info
      end)

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
      copy = Couch.get!("/#{tgt_db_name}/#{doc["_id"]}").body
      assert cmp_json(doc, copy)

      if doc["integer"] >= 10 and doc["integer"] < 15 do
        atts = copy["_attachments"]
        assert is_map(atts)
        att = atts["readme.txt"]
        assert is_map(att)
        assert att["revpos"] == 2
        assert String.match?(att["content_type"], ~r/text\/plain/)
        assert att["stub"]

        resp = Couch.get!("/#{tgt_db_name}/#{copy["_id"]}/readme.txt")
        assert String.length(resp.body) == String.length(att1_data)
        assert resp.body == att1_data
      end
    end

    # Add one more doc to source and more attachments to existing docs
    new_doc = %{"_id" => "foo666", "value" => "d"}
    [new_doc] = save_docs(src_db_name, [new_doc])

    docs =
      for doc <- docs do
        if doc["integer"] >= 10 and doc["integer"] < 15 do
          ctype = "application/binary"
          opts = [name: "data.dat", body: att2_data, content_type: ctype]
          add_attachment(src_db_name, doc, opts)
        else
          doc
        end
      end

    result = replicate(src_prefix <> src_db_name, tgt_prefix <> tgt_db_name)
    assert result["ok"]

    retry_until(fn ->
      src_info = get_db_info(src_db_name)
      tgt_info = get_db_info(tgt_db_name)

      assert tgt_info["doc_count"] == src_info["doc_count"]
    end)

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

    copy = Couch.get!("/#{tgt_db_name}/#{new_doc["_id"]}").body
    assert copy["_id"] == new_doc["_id"]
    assert copy["value"] == new_doc["value"]

    for i <- 10..14 do
      doc = Enum.at(docs, i - 1)
      copy = Couch.get!("/#{tgt_db_name}/#{i}").body
      assert cmp_json(doc, copy)

      atts = copy["_attachments"]
      assert is_map(atts)
      att = atts["readme.txt"]
      assert is_map(atts)
      assert att["revpos"] == 2
      assert String.match?(att["content_type"], ~r/text\/plain/)
      assert att["stub"]

      resp = Couch.get!("/#{tgt_db_name}/#{i}/readme.txt")
      assert String.length(resp.body) == String.length(att1_data)
      assert resp.body == att1_data

      att = atts["data.dat"]
      assert is_map(att)
      assert att["revpos"] == 3
      assert String.match?(att["content_type"], ~r/application\/binary/)
      assert att["stub"]

      resp = Couch.get!("/#{tgt_db_name}/#{i}/data.dat")
      assert String.length(resp.body) == String.length(att2_data)
      assert resp.body == att2_data
    end

    # Test deletion is replicated
    del_doc = %{
      "_id" => "1",
      "_rev" => Enum.at(docs, 0)["_rev"],
      "_deleted" => true
    }

    [del_doc] = save_docs(src_db_name, [del_doc])

    result = replicate(src_prefix <> src_db_name, tgt_prefix <> tgt_db_name)
    assert result["ok"]

    retry_until(fn ->
      src_info = get_db_info(src_db_name)
      tgt_info = get_db_info(tgt_db_name)

      assert tgt_info["doc_count"] == src_info["doc_count"]
      assert tgt_info["doc_del_count"] == src_info["doc_del_count"]
      assert tgt_info["doc_del_count"] == 1
    end)

    assert is_list(result["history"])
    assert length(result["history"]) == 3
    history = Enum.at(result["history"], 0)
    assert history["missing_checked"] == 1
    assert history["missing_found"] == 1
    assert history["docs_read"] == 1
    assert history["docs_written"] == 1
    assert history["doc_write_failures"] == 0

    resp = Couch.get("/#{tgt_db_name}/#{del_doc["_id"]}")
    assert resp.status_code == 404

    resp = Couch.get!("/#{tgt_db_name}/_changes")
    [change] = Enum.filter(resp.body["results"], &(&1["id"] == del_doc["_id"]))
    assert change["id"] == del_doc["_id"]
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
    resolve_doc = %{"_id" => "2", "_rev" => conflict, "_deleted" => true}
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
      %{"_id" => "foo1", "value" => 111},
      %{"_id" => "foo2", "value" => 222},
      %{"_id" => "foo3", "value" => 333}
    ]

    save_docs(src_db_name, src_docs)
    save_docs(tgt_db_name, Enum.filter(src_docs, &(&1["_id"] != "foo2")))

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
      %{"_id" => "foo4", "value" => 444},
      %{"_id" => "foo5", "value" => 555}
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
    delete_on_exit([src_db_name, tgt_db_name])

    docs = make_docs(1..5)
    docs = save_docs(src_db_name, docs)

    changes = get_db_changes(src_db_name)["results"]
    since_seq = Enum.at(changes, 2)["seq"]

    # TODO: In JS we re-fetch _changes with since_seq, is that
    # really necessary?
    expected_ids =
      for change <- Enum.drop(changes, 3) do
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
      result = Couch.get("/#{tgt_db_name}/#{doc["_id"]}")

      if Enum.member?(expected_ids, doc["_id"]) do
        assert result.status_code < 300
        assert cmp_json(doc, result.body)
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
    delete_on_exit([src_db_name, tgt_db_name])

    docs = make_docs(1..7)

    docs =
      for doc <- docs do
        if doc["integer"] == 2 do
          Map.put(doc, "_attachments", %{
            "hello.txt" => %{
              :content_type => "text/plain",
              # base64:encode("hello world")
              :data => "aGVsbG8gd29ybGQ="
            }
          })
        else
          doc
        end
      end

    docs = save_docs(src_db_name, docs)

    ddoc = %{
      "_id" => "_design/test",
      "language" => "javascript",
      "validate_doc_update" => """
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
      result = Couch.get("/#{tgt_db_name}/#{doc["_id"]}")

      if rem(doc["integer"], 2) == 0 do
        assert result.status_code < 300
        assert result.body["integer"] == doc["integer"]
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
    delete_on_exit([src_db_name, tgt_db_name])
    # tgt_db_name is created by the replication

    docs = make_docs(1..2)
    save_docs(src_db_name, docs)

    replicate(repl_src, repl_tgt, body: %{:create_target => true})

    retry_until(fn ->
      src_info = get_db_info(src_db_name)
      tgt_info = get_db_info(tgt_db_name)

      assert tgt_info["doc_count"] == src_info["doc_count"]

      src_shards = seq_to_shards(src_info["update_seq"])
      tgt_shards = seq_to_shards(tgt_info["update_seq"])
      assert tgt_shards == src_shards
    end)
  end

  def run_filtered_repl(src_prefix, tgt_prefix) do
    base_db_name = random_db_name()
    src_db_name = base_db_name <> "_src"
    tgt_db_name = base_db_name <> "_tgt"
    repl_src = src_prefix <> src_db_name
    repl_tgt = tgt_prefix <> tgt_db_name

    create_db(src_db_name)
    create_db(tgt_db_name)
    delete_on_exit([src_db_name, tgt_db_name])

    docs = make_docs(1..30)

    ddoc = %{
      "_id" => "_design/mydesign",
      "language" => "javascript",
      "filters" => %{
        "myfilter" => """
          function(doc, req) {
            var modulus = Number(req.query.modulus);
            var special = req.query.special;
            return (doc.integer % modulus === 0) || (doc.string === special);
          }
        """
      }
    }

    [_ | docs] = save_docs(src_db_name, [ddoc | docs])

    repl_body = %{
      "filter" => "mydesign/myfilter",
      "query_params" => %{
        "modulus" => "2",
        "special" => "7"
      }
    }

    result = replicate(repl_src, repl_tgt, body: repl_body)
    assert result["ok"]

    Enum.each(docs, fn doc ->
      resp = Couch.get!("/#{tgt_db_name}/#{doc["_id"]}")

      if rem(doc["integer"], 2) == 0 || doc["string"] == "7" do
        assert resp.status_code < 300
        assert cmp_json(doc, resp.body)
      else
        assert resp.status_code == 404
      end
    end)

    assert is_list(result["history"])
    assert length(result["history"]) == 1
    history = Enum.at(result["history"], 0)

    # We (incorrectly) don't record update sequences for things
    # that don't pass the changes feed filter. Historically the
    # last document to pass was the second to last doc which has
    # an update sequence of 30. Work that has been applied to avoid
    # conflicts from duplicate IDs breaking _bulk_docs updates added
    # a sort to the logic which changes this. Now the last document
    # to pass has a doc id of "8" and is at update_seq 29 (because only
    # "9" and the design doc are after it).
    #
    # In the future the fix ought to be that we record that update
    # sequence of the database. BigCouch has some existing work on
    # this in the clustered case because if you have very few documents
    # that pass the filter then (given single node's behavior) you end
    # up having to rescan a large portion of the database.
    # we can't rely on sequences in a cluster
    # not only can one figure appear twice (at least for n>1), there's also
    # hashes involved now - so comparing seq==29 is lottery
    # (= cutting off hashes is nonsense) above, there was brute-force
    # comparing all attrs of all docs - now we did check if excluded docs
    # did NOT make it in any way, we can't rely on sequences in a
    # cluster (so leave out)

    # 16 => 15 docs with even integer field  + 1 doc with string field "7"
    assert history["missing_checked"] == 16
    assert history["missing_found"] == 16
    assert history["docs_read"] == 16
    assert history["docs_written"] == 16
    assert history["doc_write_failures"] == 0

    new_docs = make_docs(50..55)
    new_docs = save_docs(src_db_name, new_docs)

    result = replicate(repl_src, repl_tgt, body: repl_body)
    assert result["ok"]

    Enum.each(new_docs, fn doc ->
      resp = Couch.get!("/#{tgt_db_name}/#{doc["_id"]}")

      if rem(doc["integer"], 2) == 0 do
        assert resp.status_code < 300
        assert cmp_json(doc, resp.body)
      else
        assert resp.status_code == 404
      end
    end)

    assert is_list(result["history"])
    assert length(result["history"]) == 2
    history = Enum.at(result["history"], 0)

    assert history["missing_checked"] == 3
    assert history["missing_found"] == 3
    assert history["docs_read"] == 3
    assert history["docs_written"] == 3
    assert history["doc_write_failures"] == 0
  end

  def run_filter_changed_repl(src_prefix, tgt_prefix) do
    base_db_name = random_db_name()
    src_db_name = base_db_name <> "_src"
    tgt_db_name = base_db_name <> "_tgt"
    repl_src = src_prefix <> src_db_name
    repl_tgt = tgt_prefix <> tgt_db_name

    create_db(src_db_name)
    create_db(tgt_db_name)
    delete_on_exit([src_db_name, tgt_db_name])

    filter_fun_1 = """
      function(doc, req) {
        if(doc.value < Number(req.query.maxvalue)) {
          return true;
        } else {
          return false;
        }
      }
    """

    filter_fun_2 = """
      function(doc, req) {
        return true;
      }
    """

    docs = [
      %{"_id" => "foo1", "value" => 1},
      %{"_id" => "foo2", "value" => 2},
      %{"_id" => "foo3", :value => 3},
      %{"_id" => "foo4", :value => 4}
    ]

    ddoc = %{
      "_id" => "_design/mydesign",
      :language => "javascript",
      :filters => %{
        :myfilter => filter_fun_1
      }
    }

    [ddoc | _] = save_docs(src_db_name, [ddoc | docs])

    repl_body = %{
      :filter => "mydesign/myfilter",
      :query_params => %{
        :maxvalue => "3"
      }
    }

    result = replicate(repl_src, repl_tgt, body: repl_body)
    assert result["ok"]

    assert is_list(result["history"])
    assert length(result["history"]) == 1
    history = Enum.at(result["history"], 0)
    assert history["docs_read"] == 2
    assert history["docs_written"] == 2
    assert history["doc_write_failures"] == 0

    resp = Couch.get!("/#{tgt_db_name}/foo1")
    assert HTTPotion.Response.success?(resp)
    assert resp.body["value"] == 1

    resp = Couch.get!("/#{tgt_db_name}/foo2")
    assert HTTPotion.Response.success?(resp)
    assert resp.body["value"] == 2

    resp = Couch.get!("/#{tgt_db_name}/foo3")
    assert resp.status_code == 404

    resp = Couch.get!("/#{tgt_db_name}/foo4")
    assert resp.status_code == 404

    # Replication should start from scratch after the filter's code changed
    ddoc = Map.put(ddoc, :filters, %{:myfilter => filter_fun_2})
    [_] = save_docs(src_db_name, [ddoc])

    result = replicate(repl_src, repl_tgt, body: repl_body)
    assert result["ok"]

    assert is_list(result["history"])
    assert length(result["history"]) == 1
    history = Enum.at(result["history"], 0)
    assert history["docs_read"] == 3
    assert history["docs_written"] == 3
    assert history["doc_write_failures"] == 0

    resp = Couch.get!("/#{tgt_db_name}/foo1")
    assert HTTPotion.Response.success?(resp)
    assert resp.body["value"] == 1

    resp = Couch.get!("/#{tgt_db_name}/foo2")
    assert HTTPotion.Response.success?(resp)
    assert resp.body["value"] == 2

    resp = Couch.get!("/#{tgt_db_name}/foo3")
    assert HTTPotion.Response.success?(resp)
    assert resp.body["value"] == 3

    resp = Couch.get!("/#{tgt_db_name}/foo4")
    assert HTTPotion.Response.success?(resp)
    assert resp.body["value"] == 4

    resp = Couch.get!("/#{tgt_db_name}/_design/mydesign")
    assert HTTPotion.Response.success?(resp)
  end

  def run_by_id_repl(src_prefix, tgt_prefix) do
    target_doc_ids = [
      %{
        :initial => ["1", "2", "10"],
        :after => [],
        :conflict_id => "2"
      },
      %{
        :initial => ["1", "2"],
        :after => ["7"],
        :conflict_id => "1"
      },
      %{
        :initial => ["1", "foo_666", "10"],
        :after => ["7"],
        :conflict_id => "10"
      },
      %{
        :initial => ["_design/foo", "8"],
        :after => ["foo_5"],
        :conflict_id => "8"
      },
      %{
        :initial => ["_design%2Ffoo", "8"],
        :after => ["foo_5"],
        :conflict_id => "8"
      },
      %{
        :initial => [],
        :after => ["foo_1000", "_design/foo", "1"],
        :conflict_id => "1"
      }
    ]

    Enum.each(target_doc_ids, fn test_data ->
      run_by_id_repl_impl(src_prefix, tgt_prefix, test_data)
    end)
  end

  def run_by_id_repl_impl(src_prefix, tgt_prefix, test_data) do
    base_db_name = random_db_name()
    src_db_name = base_db_name <> "_src"
    tgt_db_name = base_db_name <> "_tgt"
    repl_src = src_prefix <> src_db_name
    repl_tgt = tgt_prefix <> tgt_db_name

    retry_until(fn ->
      create_db(src_db_name)
      create_db(tgt_db_name)
    end)

    delete_on_exit([src_db_name, tgt_db_name])

    docs = make_docs(1..10)

    ddoc = %{
      "_id" => "_design/foo",
      :language => "javascript",
      "integer" => 1
    }

    doc_ids = test_data[:initial]

    num_missing =
      Enum.count(doc_ids, fn doc_id ->
        String.starts_with?(doc_id, "foo_")
      end)

    total_replicated = length(doc_ids) - num_missing

    [_ | docs] = save_docs(src_db_name, [ddoc | docs])

    repl_body = %{:doc_ids => doc_ids}
    result = replicate(repl_src, repl_tgt, body: repl_body)
    assert result["ok"]

    if total_replicated == 0 do
      assert result["no_changes"]
    else
      assert is_binary(result["start_time"])
      assert is_binary(result["end_time"])
      assert result["docs_read"] == total_replicated
      assert result["docs_written"] == total_replicated
      assert result["doc_write_failures"] == 0
    end

    Enum.each(doc_ids, fn doc_id ->
      doc_id = URI.decode(doc_id)
      orig = Couch.get!("/#{src_db_name}/#{doc_id}")
      copy = Couch.get!("/#{tgt_db_name}/#{doc_id}")

      if String.starts_with?(doc_id, "foo_") do
        assert orig.status_code == 404
        assert copy.status_code == 404
      else
        assert HTTPotion.Response.success?(orig)
        assert HTTPotion.Response.success?(copy)
        assert cmp_json(orig.body, copy.body)
      end
    end)

    # Be absolutely sure that other docs were not replicated
    Enum.each(docs, fn doc ->
      encoded_id = URI.encode_www_form(doc["_id"])
      copy = Couch.get!("/#{tgt_db_name}/#{doc["_id"]}")
      is_doc_id = &Enum.member?(doc_ids, &1)

      if is_doc_id.(doc["_id"]) or is_doc_id.(encoded_id) do
        assert HTTPotion.Response.success?(copy)
      else
        assert copy.status_code == 404
      end
    end)

    retry_until(fn ->
      tgt_info = get_db_info(tgt_db_name)
      assert tgt_info["doc_count"] == total_replicated
    end)

    doc_ids_after = test_data[:after]

    num_missing_after =
      Enum.count(doc_ids_after, fn doc_id ->
        String.starts_with?(doc_id, "foo_")
      end)

    repl_body = %{:doc_ids => doc_ids_after}
    result = replicate(repl_src, repl_tgt, body: repl_body)
    assert result["ok"]

    total_replicated_after = length(doc_ids_after) - num_missing_after

    if total_replicated_after == 0 do
      assert result["no_changes"]
    else
      assert is_binary(result["start_time"])
      assert is_binary(result["end_time"])
      assert result["docs_read"] == total_replicated_after
      assert result["docs_written"] == total_replicated_after
      assert result["doc_write_failures"] == 0
    end

    Enum.each(doc_ids_after, fn doc_id ->
      orig = Couch.get!("/#{src_db_name}/#{doc_id}")
      copy = Couch.get!("/#{tgt_db_name}/#{doc_id}")

      if String.starts_with?(doc_id, "foo_") do
        assert orig.status_code == 404
        assert copy.status_code == 404
      else
        assert HTTPotion.Response.success?(orig)
        assert HTTPotion.Response.success?(copy)
        assert cmp_json(orig.body, copy.body)
      end
    end)

    # Be absolutely sure that other docs were not replicated
    all_doc_ids = doc_ids ++ doc_ids_after

    Enum.each(docs, fn doc ->
      encoded_id = URI.encode_www_form(doc["_id"])
      copy = Couch.get!("/#{tgt_db_name}/#{doc["_id"]}")
      is_doc_id = &Enum.member?(all_doc_ids, &1)

      if is_doc_id.(doc["_id"]) or is_doc_id.(encoded_id) do
        assert HTTPotion.Response.success?(copy)
      else
        assert copy.status_code == 404
      end
    end)

    retry_until(fn ->
      tgt_info = get_db_info(tgt_db_name)

      assert tgt_info["doc_count"] == total_replicated + total_replicated_after,
             "#{inspect(test_data)}"
    end)

    # Update a source document and re-replicate (no conflict introduced)
    conflict_id = test_data[:conflict_id]
    doc = Couch.get!("/#{src_db_name}/#{conflict_id}").body
    assert is_map(doc)
    doc = Map.put(doc, "integer", 666)
    [doc] = save_docs(src_db_name, [doc])

    att1 = [
      name: "readme.txt",
      body: get_att1_data(),
      content_type: "text/plain"
    ]

    att2 = [
      name: "data.dat",
      body: get_att2_data(),
      content_type: "application/binary"
    ]

    doc = add_attachment(src_db_name, doc, att1)
    doc = add_attachment(src_db_name, doc, att2)

    repl_body = %{:doc_ids => [conflict_id]}
    result = replicate(repl_src, repl_tgt, body: repl_body)
    assert result["ok"]

    assert result["docs_read"] == 1
    assert result["docs_written"] == 1
    assert result["doc_write_failures"] == 0

    query = %{"conflicts" => "true"}
    copy = Couch.get!("/#{tgt_db_name}/#{conflict_id}", query: query)
    assert HTTPotion.Response.success?(copy)
    assert copy.body["integer"] == 666
    assert String.starts_with?(copy.body["_rev"], "4-")
    assert not Map.has_key?(doc, "_conflicts")

    atts = copy.body["_attachments"]
    assert is_map(atts)
    assert is_map(atts["readme.txt"])
    assert atts["readme.txt"]["revpos"] == 3
    assert String.match?(atts["readme.txt"]["content_type"], ~r/text\/plain/)
    assert atts["readme.txt"]["stub"]

    att1_data = Couch.get!("/#{tgt_db_name}/#{conflict_id}/readme.txt").body
    assert String.length(att1_data) == String.length(att1[:body])
    assert att1_data == att1[:body]

    assert is_map(atts["data.dat"])
    assert atts["data.dat"]["revpos"] == 4
    ct_re = ~r/application\/binary/
    assert String.match?(atts["data.dat"]["content_type"], ct_re)
    assert atts["data.dat"]["stub"]

    att2_data = Couch.get!("/#{tgt_db_name}/#{conflict_id}/data.dat").body
    assert String.length(att2_data) == String.length(att2[:body])
    assert att2_data == att2[:body]

    # Generate a conflict using replication by doc ids
    orig = Couch.get!("/#{src_db_name}/#{conflict_id}").body
    orig = Map.update!(orig, "integer", &(&1 + 100))
    [_] = save_docs(src_db_name, [orig])

    copy = Couch.get!("/#{tgt_db_name}/#{conflict_id}").body
    copy = Map.update!(copy, "integer", &(&1 + 1))
    [_] = save_docs(tgt_db_name, [copy])

    result = replicate(repl_src, repl_tgt, body: repl_body)
    assert result["ok"]
    assert result["docs_read"] == 1
    assert result["docs_written"] == 1
    assert result["doc_write_failures"] == 0

    retry_until(fn ->
      copy = Couch.get!("/#{tgt_db_name}/#{conflict_id}", query: query).body
      assert String.match?(copy["_rev"], ~r/^5-/)
      assert is_list(copy["_conflicts"])
      assert length(copy["_conflicts"]) == 1
      conflict_rev = Enum.at(copy["_conflicts"], 0)
      assert String.match?(conflict_rev, ~r/^5-/)
    end)
  end

  def run_continuous_repl(src_prefix, tgt_prefix) do
    base_db_name = random_db_name()
    src_db_name = base_db_name <> "_src"
    tgt_db_name = base_db_name <> "_tgt"
    repl_src = src_prefix <> src_db_name
    repl_tgt = tgt_prefix <> tgt_db_name

    create_db(src_db_name)
    create_db(tgt_db_name)
    delete_on_exit([src_db_name, tgt_db_name])

    ddoc = %{
      "_id" => "_design/mydesign",
      "language" => "javascript",
      "filters" => %{
        "myfilter" => "function(doc, req) { return true; }"
      }
    }

    docs = make_docs(1..25)
    docs = save_docs(src_db_name, docs ++ [ddoc])

    att1_data = get_att1_data()

    docs =
      for doc <- docs do
        if doc["integer"] >= 10 and doc["integer"] < 15 do
          add_attachment(src_db_name, doc)
        else
          doc
        end
      end

    repl_body = %{:continuous => true}
    result = replicate(repl_src, repl_tgt, body: repl_body)

    assert result["ok"]
    assert is_binary(result["_local_id"])

    repl_id = result["_local_id"]
    task = get_task(repl_id, 30_000)
    assert is_map(task), "Error waiting for replication to start"

    wait_for_repl(src_db_name, repl_id, 26)

    Enum.each(docs, fn doc ->
      resp = Couch.get!("/#{tgt_db_name}/#{doc["_id"]}")
      assert resp.status_code < 300
      assert cmp_json(doc, resp.body)

      if doc["integer"] >= 10 and doc["integer"] < 15 do
        atts = resp.body["_attachments"]
        assert is_map(atts)
        att = atts["readme.txt"]
        assert is_map(att)
        assert att["revpos"] == 2
        assert String.match?(att["content_type"], ~r/text\/plain/)
        assert att["stub"]

        resp = Couch.get!("/#{tgt_db_name}/#{doc["_id"]}/readme.txt")
        assert String.length(resp.body) == String.length("some text")
        assert resp.body == "some text"
      end
    end)

    src_info = get_db_info(src_db_name)
    tgt_info = get_db_info(tgt_db_name)

    assert tgt_info["doc_count"] == src_info["doc_count"]

    # Add attachments to more source docs
    docs =
      for doc <- docs do
        is_ddoc = String.starts_with?(doc["_id"], "_design/")

        case doc["integer"] do
          n when n >= 10 and n < 15 ->
            ctype = "application/binary"
            opts = [name: "data.dat", body: att1_data, content_type: ctype]
            add_attachment(src_db_name, doc, opts)

          _ when is_ddoc ->
            add_attachment(src_db_name, doc)

          _ ->
            doc
        end
      end

    wait_for_repl(src_db_name, repl_id, 32)

    Enum.each(docs, fn doc ->
      is_ddoc = String.starts_with?(doc["_id"], "_design/")

      case doc["integer"] do
        N when (N >= 10 and N < 15) or is_ddoc ->
          resp = Couch.get!("/#{tgt_db_name}/#{doc["_id"]}")
          atts = resp.body["_attachments"]
          assert is_map(atts)
          att = atts["readme.txt"]
          assert is_map(att)
          assert att["revpos"] == 2
          assert String.match?(att["content_type"], ~r/text\/plain/)
          assert att["stub"]

          resp = Couch.get!("/#{tgt_db_name}/#{doc["_id"]}/readme.txt")
          assert String.length(resp.body) == String.length("some text")
          assert resp.body == "some text"

          if not is_ddoc do
            att = atts["data.dat"]
            assert is_map(att)
            assert att["revpos"] == 3
            assert String.match?(att["content_type"], ~r/application\/binary/)
            assert att["stub"]

            resp = Couch.get!("/#{tgt_db_name}/#{doc["_id"]}/data.dat")
            assert String.length(resp.body) == String.length(att1_data)
            assert resp.body == att1_data
          end

        _ ->
          :ok
      end
    end)

    src_info = get_db_info(src_db_name)
    tgt_info = get_db_info(tgt_db_name)

    assert tgt_info["doc_count"] == src_info["doc_count"]

    ddoc = List.last(docs)
    ctype = "application/binary"
    opts = [name: "data.dat", body: att1_data, content_type: ctype]
    add_attachment(src_db_name, ddoc, opts)

    wait_for_repl(src_db_name, repl_id, 33)

    resp = Couch.get("/#{tgt_db_name}/#{ddoc["_id"]}")
    atts = resp.body["_attachments"]
    assert is_map(atts)
    att = atts["readme.txt"]
    assert is_map(att)
    assert att["revpos"] == 2
    assert String.match?(att["content_type"], ~r/text\/plain/)
    assert att["stub"]

    resp = Couch.get!("/#{tgt_db_name}/#{ddoc["_id"]}/readme.txt")
    assert String.length(resp.body) == String.length("some text")
    assert resp.body == "some text"

    att = atts["data.dat"]
    assert is_map(att)
    assert att["revpos"] == 3
    assert String.match?(att["content_type"], ~r/application\/binary/)
    assert att["stub"]

    resp = Couch.get!("/#{tgt_db_name}/#{ddoc["_id"]}/data.dat")
    assert String.length(resp.body) == String.length(att1_data)
    assert resp.body == att1_data

    src_info = get_db_info(src_db_name)
    tgt_info = get_db_info(tgt_db_name)

    assert tgt_info["doc_count"] == src_info["doc_count"]

    # Check creating new normal documents
    new_docs = make_docs(26..35)
    new_docs = save_docs(src_db_name, new_docs)

    wait_for_repl(src_db_name, repl_id, 43)

    Enum.each(new_docs, fn doc ->
      resp = Couch.get!("/#{tgt_db_name}/#{doc["_id"]}")
      assert resp.status_code < 300
      assert cmp_json(doc, resp.body)
    end)

    src_info = get_db_info(src_db_name)
    tgt_info = get_db_info(tgt_db_name)

    assert tgt_info["doc_count"] == src_info["doc_count"]

    # Delete docs from the source

    doc1 = Enum.at(new_docs, 0)
    query = %{:rev => doc1["_rev"]}
    Couch.delete!("/#{src_db_name}/#{doc1["_id"]}", query: query)

    doc2 = Enum.at(new_docs, 6)
    query = %{:rev => doc2["_rev"]}
    Couch.delete!("/#{src_db_name}/#{doc2["_id"]}", query: query)

    wait_for_repl(src_db_name, repl_id, 45)

    resp = Couch.get("/#{tgt_db_name}/#{doc1["_id"]}")
    assert resp.status_code == 404
    resp = Couch.get("/#{tgt_db_name}/#{doc2["_id"]}")
    assert resp.status_code == 404

    changes = get_db_changes(tgt_db_name, %{:since => tgt_info["update_seq"]})
    # quite unfortunately, there is no way on relying on ordering in a cluster
    # but we can assume a length of 2
    changes =
      for change <- changes["results"] do
        {change["id"], change["deleted"]}
      end

    assert Enum.sort(changes) == [{doc1["_id"], true}, {doc2["_id"], true}]

    # Cancel the replication
    repl_body = %{:continuous => true, :cancel => true}
    resp = replicate(repl_src, repl_tgt, body: repl_body)
    assert resp["ok"]
    assert resp["_local_id"] == repl_id

    doc = %{"_id" => "foobar", "value" => 666}
    [doc] = save_docs(src_db_name, [doc])

    wait_for_repl_stop(repl_id, 30_000)

    resp = Couch.get("/#{tgt_db_name}/#{doc["_id"]}")
    assert resp.status_code == 404
  end

  def run_compressed_att_repl(src_prefix, tgt_prefix) do
    base_db_name = random_db_name()
    src_db_name = base_db_name <> "_src"
    tgt_db_name = base_db_name <> "_tgt"
    repl_src = src_prefix <> src_db_name
    repl_tgt = tgt_prefix <> tgt_db_name

    create_db(src_db_name)
    create_db(tgt_db_name)
    delete_on_exit([src_db_name, tgt_db_name])

    doc = %{"_id" => "foobar"}
    [doc] = save_docs(src_db_name, [doc])

    att1_data = get_att1_data()
    num_copies = 1 + round(128 * 1024 / String.length(att1_data))

    big_att =
      List.foldl(Enum.to_list(1..num_copies), "", fn _, acc ->
        acc <> att1_data
      end)

    doc = add_attachment(src_db_name, doc, body: big_att)

    # Disable attachment compression
    set_config_raw("attachments", "compression_level", "0")

    result = replicate(repl_src, repl_tgt)
    assert result["ok"]
    assert is_list(result["history"])
    assert length(result["history"]) == 1
    history = Enum.at(result["history"], 0)
    assert history["missing_checked"] == 1
    assert history["missing_found"] == 1
    assert history["docs_read"] == 1
    assert history["docs_written"] == 1
    assert history["doc_write_failures"] == 0

    token = Enum.random(1..1_000_000)
    query = %{att_encoding_info: true, bypass_cache: token}
    resp = Couch.get("/#{tgt_db_name}/#{doc["_id"]}", query: query)
    assert resp.status_code < 300
    assert is_map(resp.body["_attachments"])
    att = resp.body["_attachments"]["readme.txt"]
    assert att["encoding"] == "gzip"
    assert is_integer(att["length"])
    assert is_integer(att["encoded_length"])
    assert att["encoded_length"] < att["length"]
  end

  def run_non_admin_target_user_repl(src_prefix, tgt_prefix, ctx) do
    base_db_name = random_db_name()
    src_db_name = base_db_name <> "_src"
    tgt_db_name = base_db_name <> "_tgt"
    repl_src = src_prefix <> src_db_name
    repl_tgt = tgt_prefix <> tgt_db_name

    create_db(src_db_name)
    create_db(tgt_db_name)
    delete_on_exit([src_db_name, tgt_db_name])

    set_security(tgt_db_name, %{
      :admins => %{
        :names => ["superman"],
        :roles => ["god"]
      }
    })

    docs = make_docs(1..6)
    ddoc = %{"_id" => "_design/foo", "language" => "javascript"}
    docs = save_docs(src_db_name, [ddoc | docs])

    sess = Couch.login(ctx[:userinfo])
    resp = Couch.Session.get(sess, "/_session")
    assert resp.body["ok"]
    assert resp.body["userCtx"]["name"] == "joe"

    opts = [
      userinfo: ctx[:userinfo],
      headers: [cookie: sess.cookie]
    ]

    result = replicate(repl_src, repl_tgt, opts)

    assert Couch.Session.logout(sess).body["ok"]

    assert result["ok"]
    history = Enum.at(result["history"], 0)
    assert history["docs_read"] == length(docs)
    # ddoc write failed
    assert history["docs_written"] == length(docs) - 1
    # ddoc write failed
    assert history["doc_write_failures"] == 1

    Enum.each(docs, fn doc ->
      resp = Couch.get("/#{tgt_db_name}/#{doc["_id"]}")

      if String.starts_with?(doc["_id"], "_design/") do
        assert resp.status_code == 404
      else
        assert HTTPotion.Response.success?(resp)
        assert cmp_json(doc, resp.body)
      end
    end)
  end

  def run_non_admin_or_reader_source_user_repl(src_prefix, tgt_prefix, ctx) do
    base_db_name = random_db_name()
    src_db_name = base_db_name <> "_src"
    tgt_db_name = base_db_name <> "_tgt"
    repl_src = src_prefix <> src_db_name
    repl_tgt = tgt_prefix <> tgt_db_name

    create_db(src_db_name)
    create_db(tgt_db_name)
    delete_on_exit([src_db_name, tgt_db_name])

    set_security(tgt_db_name, %{
      :admins => %{
        :names => ["superman"],
        :roles => ["god"]
      },
      :readers => %{
        :names => ["john"],
        :roles => ["secret"]
      }
    })

    docs = make_docs(1..6)
    ddoc = %{"_id" => "_design/foo", "language" => "javascript"}
    docs = save_docs(src_db_name, [ddoc | docs])

    sess = Couch.login(ctx[:userinfo])
    resp = Couch.Session.get(sess, "/_session")
    assert resp.body["ok"]
    assert resp.body["userCtx"]["name"] == "joe"

    opts = [
      userinfo: ctx[:userinfo],
      headers: [cookie: sess.cookie]
    ]

    assert_raise(ExUnit.AssertionError, fn ->
      replicate(repl_src, repl_tgt, opts)
    end)

    assert Couch.Session.logout(sess).body["ok"]

    Enum.each(docs, fn doc ->
      resp = Couch.get("/#{tgt_db_name}/#{doc["_id"]}")
      assert resp.status_code == 404
    end)
  end

  def get_db_info(db_name) do
    resp = Couch.get("/#{db_name}")
    assert HTTPotion.Response.success?(resp)
    resp.body
  end

  def replicate(src, tgt, options \\ []) do
    {userinfo, options} = Keyword.pop(options, :userinfo)

    userinfo =
      if userinfo == nil do
        @admin_account
      else
        userinfo
      end

    src = set_user(src, userinfo)
    tgt = set_user(tgt, userinfo)

    defaults = [headers: [], body: %{}, timeout: 30_000]
    options = defaults |> Keyword.merge(options) |> Enum.into(%{})

    %{body: body} = options
    body = [source: src, target: tgt] |> Enum.into(body)
    options = Map.put(options, :body, body)

    resp = Couch.post("/_replicate", Enum.to_list(options))
    assert HTTPotion.Response.success?(resp), "#{inspect(resp)}"
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

  def get_db_changes(db_name, query \\ %{}) do
    resp = Couch.get("/#{db_name}/_changes", query: query)
    assert HTTPotion.Response.success?(resp), "#{inspect(resp)} #{inspect(query)}"
    resp.body
  end

  def save_docs(db_name, docs) do
    query = %{w: 3}
    body = %{docs: docs}
    resp = Couch.post("/#{db_name}/_bulk_docs", query: query, body: body)
    assert HTTPotion.Response.success?(resp)

    for {doc, resp} <- Enum.zip(docs, resp.body) do
      assert resp["ok"], "Error saving doc: #{doc["_id"]}"
      Map.put(doc, "_rev", resp["rev"])
    end
  end

  def set_security(db_name, sec_props) do
    resp = Couch.put("/#{db_name}/_security", body: :jiffy.encode(sec_props))
    assert HTTPotion.Response.success?(resp)
    assert resp.body["ok"]
  end

  def add_attachment(db_name, doc, att \\ []) do
    defaults = [
      name: <<"readme.txt">>,
      body: <<"some text">>,
      content_type: "text/plain"
    ]

    att = defaults |> Keyword.merge(att) |> Enum.into(%{})
    uri = "/#{db_name}/#{URI.encode(doc["_id"])}/#{att[:name]}"
    headers = ["Content-Type": att[:content_type]]

    params =
      if doc["_rev"] do
        %{:w => 3, :rev => doc["_rev"]}
      else
        %{:w => 3}
      end

    retry_until(fn ->
      resp = Couch.put(uri, headers: headers, query: params, body: att[:body])
      assert HTTPotion.Response.success?(resp)
      Map.put(doc, "_rev", resp.body["rev"])
    end)
  end

  def wait_for_repl(src_db_name, repl_id, expect_revs_checked) do
    wait_for_repl(src_db_name, repl_id, expect_revs_checked, 30_000)
  end

  def wait_for_repl(_, _, _, wait_left) when wait_left <= 0 do
    assert false, "Timeout waiting for replication"
  end

  def wait_for_repl(src_db_name, repl_id, expect_revs_checked, wait_left) do
    task = get_task(repl_id, 0)
    through_seq = task["through_seq"] || "0"
    revs_checked = task["revisions_checked"]
    changes = get_db_changes(src_db_name, %{:since => through_seq})

    if length(changes["results"]) > 0 or revs_checked < expect_revs_checked do
      :timer.sleep(500)
      wait_for_repl(src_db_name, repl_id, expect_revs_checked, wait_left - 500)
    end

    task
  end

  def wait_for_repl_stop(repl_id) do
    wait_for_repl_stop(repl_id, 30_000)
  end

  def wait_for_repl_stop(repl_id, wait_left) when wait_left <= 0 do
    assert false, "Timeout waiting for replication task to stop: #{repl_id}"
  end

  def wait_for_repl_stop(repl_id, wait_left) do
    task = get_task(repl_id, 0)

    if is_map(task) do
      :timer.sleep(500)
      wait_for_repl_stop(repl_id, wait_left - 500)
    end
  end

  def get_last_seq(db_name) do
    body = get_db_changes(db_name, %{:since => "now"})
    body["last_seq"]
  end

  def get_task(repl_id, delay) when delay <= 0 do
    try_get_task(repl_id)
  end

  def get_task(repl_id, delay) do
    case try_get_task(repl_id) do
      result when is_map(result) ->
        result

      _ ->
        :timer.sleep(500)
        get_task(repl_id, delay - 500)
    end
  end

  def try_get_task(repl_id) do
    resp = Couch.get("/_active_tasks")
    assert HTTPotion.Response.success?(resp)
    assert is_list(resp.body)

    Enum.find(resp.body, nil, fn task ->
      task["replication_id"] == repl_id
    end)
  end

  def set_user(uri, userinfo) do
    case URI.parse(uri) do
      %{scheme: nil} ->
        uri

      %{userinfo: nil} = uri ->
        URI.to_string(Map.put(uri, :userinfo, userinfo))

      _ ->
        uri
    end
  end

  def get_att1_data do
    File.read!(Path.expand("data/lorem.txt", __DIR__))
  end

  def get_att2_data do
    File.read!(Path.expand("data/lorem_b64.txt", __DIR__))
  end

  def cmp_json(lhs, rhs) when is_map(lhs) and is_map(rhs) do
    Enum.reduce_while(lhs, true, fn {k, v}, true ->
      if Map.has_key?(rhs, k) do
        if cmp_json(v, rhs[k]) do
          {:cont, true}
        else
          Logger.error("#{inspect(lhs)} != #{inspect(rhs)}")
          {:halt, false}
        end
      else
        Logger.error("#{inspect(lhs)} != #{inspect(rhs)}")
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

  def delete_on_exit(db_names) when is_list(db_names) do
    on_exit(fn ->
      Enum.each(db_names, fn name ->
        delete_db(name)
      end)
    end)
  end
end

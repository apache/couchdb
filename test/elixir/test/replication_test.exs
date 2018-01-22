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
    #
    # @tag config: [
    #   {"replicator", "startup_jitter", "0"}
    # ]
    # test "simple #{name} replication" do
    #   run_simple_repl(@src_prefix, @tgt_prefix)
    # end
    #
    # @tag config: [
    #   {"replicator", "startup_jitter", "0"}
    # ]
    # test "replicate with since_seq - #{name}" do
    #   run_since_seq_repl(@src_prefix, @tgt_prefix)
    # end
    #
    # @tag config: [
    #   {"replicator", "startup_jitter", "0"}
    # ]
    # test "validate_doc_update failure replications - #{name}" do
    #   run_vdu_repl(@src_prefix, @tgt_prefix)
    # end
    #
    # @tag config: [
    #   {"replicator", "startup_jitter", "0"}
    # ]
    # test "create_target filter option - #{name}" do
    #   run_create_target_repl(@src_prefix, @tgt_prefix)
    # end
    #
    # @tag config: [
    #   {"replicator", "startup_jitter", "0"}
    # ]
    # test "filtered replications - #{name}" do
    #   run_filtered_repl(@src_prefix, @tgt_prefix)
    # end
    #
    # @tag config: [
    #   {"replicator", "startup_jitter", "0"}
    # ]
    # test "replication restarts after filter change - COUCHDB-892 - #{name}" do
    #   run_filter_changed_repl(@src_prefix, @tgt_prefix)
    # end
    #
    # @tag config: [
    #   {"replicator", "startup_jitter", "0"}
    # ]
    # test "replication by doc ids - #{name}" do
    #   run_by_id_repl(@src_prefix, @tgt_prefix)
    # end

    @tag config: [
      {"replicator", "startup_jitter", "0"}
    ]
    test "continuous replication - #{name}" do
      run_continuous_repl(@src_prefix, @tgt_prefix)
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
      "_id" => "_design/foo",
      "language" => "javascript",
      "value" => "ddoc"
    }
    docs = make_docs(1..20) ++ [ddoc]
    docs = save_docs(src_db_name, docs)

    docs = for doc <- docs do
      if doc["integer"] >= 10 and doc["integer"] < 15 do
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

    docs = for doc <- docs do
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

    on_exit(fn -> delete_db(src_db_name) end)
    on_exit(fn -> delete_db(tgt_db_name) end)

    docs = make_docs(1..7)
    docs = for doc <- docs do
      if doc["integer"] == 2 do
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

  def run_filtered_repl(src_prefix, tgt_prefix) do
    base_db_name = random_db_name()
    src_db_name = base_db_name <> "_src"
    tgt_db_name = base_db_name <> "_tgt"
    repl_src = src_prefix <> src_db_name
    repl_tgt = tgt_prefix <> tgt_db_name

    create_db(src_db_name)
    create_db(tgt_db_name)

    on_exit(fn -> delete_db(src_db_name) end)
    on_exit(fn -> delete_db(tgt_db_name) end)

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
      if(rem(doc["integer"], 2) == 0 || doc["string"] == "7") do
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
      if(rem(doc["integer"], 2) == 0) do
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

    on_exit(fn -> delete_db(src_db_name) end)
    on_exit(fn -> delete_db(tgt_db_name) end)

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

    create_db(src_db_name)
    create_db(tgt_db_name)

    on_exit(fn -> delete_db(src_db_name) end)
    on_exit(fn -> delete_db(tgt_db_name) end)

    docs = make_docs(1..10)
    ddoc = %{
      "_id" => "_design/foo",
      :language => "javascript",
      "integer" => 1
    }

    doc_ids = test_data[:initial]
    num_missing = Enum.count(doc_ids, fn doc_id ->
      String.starts_with?(doc_id, "foo_")
    end)
    total_replicated = length(doc_ids) - num_missing

    [_ | docs] = save_docs(src_db_name, [ddoc | docs])

    repl_body = %{:doc_ids => doc_ids}
    result = replicate(repl_src, repl_tgt, body: repl_body)
    assert result["ok"]

    if(total_replicated == 0) do
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

      if(String.starts_with?(doc_id, "foo_")) do
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
      is_doc_id = &(Enum.member?(doc_ids, &1))
      if(is_doc_id.(doc["_id"]) or is_doc_id.(encoded_id)) do
        assert HTTPotion.Response.success?(copy)
      else
        assert copy.status_code == 404
      end
    end)

    tgt_info = get_db_info(tgt_db_name)
    assert tgt_info["doc_count"] == total_replicated

    doc_ids_after = test_data[:after]
    num_missing_after = Enum.count(doc_ids_after, fn doc_id ->
      String.starts_with?(doc_id, "foo_")
    end)

    repl_body = %{:doc_ids => doc_ids_after}
    result = replicate(repl_src, repl_tgt, body: repl_body)
    assert result["ok"]

    total_replicated_after = length(doc_ids_after) - num_missing_after
    if(total_replicated_after == 0) do
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

      if(String.starts_with?(doc_id, "foo_")) do
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
      is_doc_id = &(Enum.member?(all_doc_ids, &1))
      if(is_doc_id.(doc["_id"]) or is_doc_id.(encoded_id)) do
        assert HTTPotion.Response.success?(copy)
      else
        assert copy.status_code == 404
      end
    end)

    tgt_info = get_db_info(tgt_db_name)
    assert tgt_info["doc_count"] == total_replicated + total_replicated_after, "#{inspect test_data}"

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

    copy = Couch.get!("/#{tgt_db_name}/#{conflict_id}", query: query).body
    assert String.match?(copy["_rev"], ~r/^5-/)
    assert is_list(copy["_conflicts"])
    assert length(copy["_conflicts"]) == 1
    conflict_rev = Enum.at(copy["_conflicts"], 0)
    assert String.match?(conflict_rev, ~r/^5-/)
  end

  def run_continuous_repl(src_prefix, tgt_prefix) do
    base_db_name = random_db_name()
    src_db_name = base_db_name <> "_src"
    tgt_db_name = base_db_name <> "_tgt"
    repl_src = src_prefix <> src_db_name
    repl_tgt = tgt_prefix <> tgt_db_name

    create_db(src_db_name)
    create_db(tgt_db_name)

    ddoc = %{
      "_id" => "_design/mydesign",
      :language => "javascript",
      :filters => %{
        :myfilter => "function(doc, req) { return true; }"
      }
    }
    docs = make_docs(1..25)
    docs = save_docs(src_db_name, docs ++ [ddoc])

    att1_data = get_att1_data()
    att2_data = get_att2_data()

    docs = for doc <- docs do
      if doc["integer"] >= 10 and doc["integer"] < 15 do
        add_attachment(src_db_name, doc, body: att1_data)
      else
        doc
      end
    end

    repl_body = %{:continuous => true}
    result = replicate(repl_src, repl_tgt, body: repl_body)

    assert result["ok"]
    assert is_binary(result["_local_id"])

    repl_id = result["_local_id"]

    Logger.debug "#{src_db_name} - #{repl_id}"
    wait_for_seq(src_db_name, repl_id)

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
        assert String.length(resp.body) == String.length(att1_data)
        assert resp.body == att1_data
      end
    end)

    src_info = get_db_info(src_db_name)
    tgt_info = get_db_info(tgt_db_name)

    assert tgt_info["doc_count"] == src_info["doc_count"]

    # Add attachments to more source docs
    docs = for doc <- docs do
      is_ddoc = String.starts_with?(doc["_id"], "_design/")
      case doc["integer"] do
        n when n >= 10 and n < 15 ->
          ctype = "application/binary"
          opts = [name: "data.dat", body: att2_data, content_type: ctype]
          add_attachment(src_db_name, doc, opts)
        _ when is_ddoc ->
          add_attachment(src_db_name, doc)
        _ ->
          doc
      end
    end

    wait_for_seq(src_db_name, repl_id)

    Enum.each(docs, fn doc ->
      is_ddoc = String.starts_with?(doc["_id"], "_design/")
      case doc["integer"] do
        N when N >= 10 and N < 15 or is_ddoc ->
          resp = Couch.get!("/#{tgt_db_name}/#{doc["_id"]}")
          atts = resp.body["_attachments"]
          assert is_map(atts)
          att = atts["readme.txt"]
          assert is_map(att)
          assert att["revpos"] == 2
          assert String.match?(att["content_type"], ~r/text\/plain/)
          assert att["stub"]

          resp = Couch.get!("/#{tgt_db_name}/#{doc["_id"]}/readme.txt")
          assert String.length(resp.body) == String.length(att1_data)
          assert resp.body == att1_data

          if not is_ddoc do
            att = atts["data.dat"]
            assert is_map(att)
            assert att["revpos"] == 3
            assert String.match?(att["content_type"], ~r/application\/binary/)
            assert att["stub"]

            resp = Couch.get!("/#{tgt_db_name}/#{doc["_id"]}/data.dat")
            assert String.length(resp.body) == String.length(att2_data)
            assert resp.body == att2_data
          end
        _ ->
          :ok
      end
    end)

    src_info = get_db_info(src_db_name)
    tgt_info = get_db_info(tgt_db_name)

    assert tgt_info["doc_count"] == src_info["doc_count"]

    ddoc = Enum.last(docs)
    ctype = "application/binary"
    opts = [name: "data.dat", body: att2_data, content_type: ctype]
    add_attachment(src_db_name, ddoc, opts)

    wait_for_seq(src_db_name, repl_id)

    copy = Couch.get("/#{tgt_db_name}/#{ddoc["_id"]}")
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

    att = atts["data.dat"]
    assert is_map(att)
    assert att["revpos"] == 3
    assert String.match?(att["content_type"], ~r/application\/binary/)
    assert att["stub"]

    resp = Couch.get!("/#{tgt_db_name}/#{copy["_id"]}/data.dat")
    assert String.length(resp.body) == String.length(att2_data)
    assert resp.body == att2_data

    src_info = get_db_info(src_db_name)
    tgt_info = get_db_info(tgt_db_name)

    assert tgt_info["doc_count"] == src_info["doc_count"]

    # Check creating new normal documents
    new_docs = make_docs(25..35)
    new_docs = save_docs(src_db_name, new_docs)

    wait_for_seq(src_db_name, repl_id)

    Enum.each(new_docs, fn doc ->
      resp = Couch.get!("/#{tgt_db_name}/#{doc["_id"]}")
      assert resp.status_code < 300
      assert cmp_json(doc, resp.body)
    end)

    src_info = get_db_info(src_db_name)
    tgt_info = get_db_info(tgt_db_name)

    assert tgt_info["doc_count"] == src_info["doc_count"]

    # Delete docs from the source

    doc1 = Enum.at(docs, 0)
    doc2 = Enum.at(docs, 6)

    Couch.delete!(src_db_name, doc1["_id"])
    Couch.delete!(src_db_name, doc2["_id"])

    wait_for_seq(src_db_name, repl_id)

    resp = Couch.get(tgt_db_name, doc1["_id"])
    assert resp.status_code == 404
    resp = Couch.get(tgt_db_name, doc2["_id"])
    assert resp.status_code == 404

    changes = get_db_changes(tgt_db_name, %{:since => tgt_info["update_seq"]})
    # quite unfortunately, there is no way on relying on ordering in a cluster
    # but we can assume a length of 2
    changes = for change <- changes["results"] do
      {change["_id"], change["deleted"]}
    end
    assert Enum.sort(changes) == [{doc1["_id"], true}, {doc2["_id"], true}]

    # Cancel the replication
    repl_body = %{:continuous => true, :cancel => true}
    resp = replicate(repl_src, repl_tgt, repl_body)
    assert resp["ok"]
    assert resp["_local_id"] == repl_id

    doc = %{"_id" => "foobar", "value": 666}
    [doc] = save_docs(src_db_name, [doc])

    wait_for_replication_stop(repl_id, 30000)

    resp = Couch.get("/#{tgt_db_name}/#{doc["_id"]}")
    assert resp.status_code == 404
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

  def get_db_changes(db_name, query \\ %{}) do
    resp = Couch.get("/#{db_name}/_changes", query: query)
    assert HTTPotion.Response.success?(resp), "#{inspect resp}"
    resp.body
  end

  def save_docs(db_name, docs) do
    query = %{w: 3}
    body = %{docs: docs}
    resp = Couch.post("/#{db_name}/_bulk_docs", query: query, body: body)
    assert HTTPotion.Response.success?(resp)
    for {doc, resp} <- Enum.zip(docs, resp.body) do
      Map.put(doc, "_rev", resp["rev"])
    end
  end

  def add_attachment(db_name, doc, att \\ []) do
    defaults = [
      name: <<"readme.txt">>,
      body: <<"some text">>,
      content_type: "text/plain"
    ]
    att = Keyword.merge(defaults, att) |> Enum.into(%{})
    uri = "/#{db_name}/#{URI.encode(doc["_id"])}/#{att[:name]}"
    headers = ["Content-Type": att[:content_type]]
    params = if doc["_rev"] do
      %{:w => 3, :rev => doc["_rev"]}
    else
      %{:w => 3}
    end
    resp = Couch.put(uri, headers: headers, query: params, body: att[:body])
    assert HTTPotion.Response.success?(resp)
    Map.put(doc, "_rev", resp.body["rev"])
  end

  def wait_for_seq(src_db_name, repl_id) do
    src_info = get_db_info(src_db_name)
    src_seq = src_info["update_seq"]
    wait_for_seq(src_seq, repl_id, 30000)
  end

  def wait_for_seq(src_seq, _, wait_left) when wait_left <= 0 do
    assert false, "Timeout waiting for replication sequence: #{src_seq}"
  end

  def wait_for_seq(src_seq, repl_id, wait_left) do
    task = get_task(repl_id, 0)
    Logger.debug "task: #{src_seq} #{inspect task}"
    if not is_map(task) or task["through_seq"] != src_seq do
      :timer.sleep(500)
      wait_for_seq(src_seq, repl_id, wait_left - 500)
    end
    task
  end

  def wait_for_replication_stop(repl_id, wait_left) when wait_left <= 0 do
    assert false, "Timeout waiting for replication task to stop: #{repl_id}"
  end

  def wait_for_replication_stop(repl_id, wait_left) do
    task = get_task(repl_id, 0)
    if is_map(task) do
      :timer.sleep(500)
      wait_for_replication_stop(repl_id, wait_left - 500)
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
    Enum.find(resp.body, :null, fn task ->
      task["replication_id"] == repl_id
    end)
  end

  def make_docs(ids) do
    for id <- ids, str_id = Integer.to_string(id) do
      %{"_id" => str_id, "integer" => id, "string" => str_id}
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
          Logger.error "#{inspect lhs} != #{inspect rhs}"
          {:halt, false}
        end
      else
        Logger.error "#{inspect lhs} != #{inspect rhs}"
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
end

defmodule BasicsTest do
  use CouchTestCase

  @moduletag :basics
  @moduletag kind: :single_node

  @moduledoc """
  Test CouchDB basics.
  This is a port of the basics.js suite
  """

  test "Session contains adm context" do
    user_ctx = Couch.get("/_session").body["userCtx"]
    assert user_ctx["name"] == "adm", "Should have adm user context"
    assert user_ctx["roles"] == ["_admin"], "Should have _admin role"
  end

  test "Welcome endpoint" do
    assert Couch.get("/").body["couchdb"] == "Welcome", "Should say welcome"
  end

  test "Ready endpoint" do
    resp = Couch.get("/_up")
    assert resp.status_code == 200
    assert resp.body["status"] == "ok"
  end

  @tag :with_db
  test "PUT on existing DB should return 412 instead of 500", context do
    db_name = context[:db_name]
    resp = Couch.put("/#{db_name}")
    assert resp.status_code == 412
    refute resp.body["ok"]
  end

  @tag :with_db_name
  test "Creating a new DB should return location header", context do
    db_name = context[:db_name]
    {:ok, resp} = create_db(db_name)
    msg = "Should return Location header for new db"
    assert String.ends_with?(resp.headers["location"], db_name), msg
    {:ok, _} = delete_db(db_name)
  end

  @tag :with_db_name
  test "Creating a new DB with slashes should return Location header (COUCHDB-411)",
       context do
    db_name = context[:db_name] <> "%2Fwith_slashes"
    {:ok, resp} = create_db(db_name)
    msg = "Should return Location header for new db"
    assert String.ends_with?(resp.headers["location"], db_name), msg
    {:ok, _} = delete_db(db_name)
  end

  test "Exceeding configured DB name size limit returns an error" do
    db_name = String.duplicate("x", 239)
    resp = Couch.put("/#{db_name}")
    assert resp.status_code == 400
    assert resp.body["error"] == "database_name_too_long"
  end

  @tag :with_db
  test "Created database has appropriate db info name", context do
    db_name = context[:db_name]

    assert Couch.get("/#{db_name}").body["db_name"] == db_name,
           "Get correct database name"
  end

  @tag :with_db
  test "Database should be in _all_dbs", context do
    assert context[:db_name] in Couch.get("/_all_dbs").body, "Db name in _all_dbs"
  end

  @tag :with_db
  test "Empty database should have zero docs", context do
    assert Couch.get("/#{context[:db_name]}").body["doc_count"] == 0,
           "Empty doc count in empty db"
  end

  @tag :with_db
  test "Create a document and save it to the database", context do
    resp = Couch.post("/#{context[:db_name]}", body: %{:_id => "0", :a => 1, :b => 1})
    assert resp.status_code in [201, 202], "Should be 201 created"
    assert resp.body["id"], "Id should be present"
    assert resp.body["rev"], "Rev should be present"

    resp2 = Couch.get("/#{context[:db_name]}/#{resp.body["id"]}")
    assert resp2.body["_id"] == resp.body["id"], "Ids should match"
    assert resp2.body["_rev"] == resp.body["rev"], "Revs should match"
  end

  @tag :with_db
  test "Revs info status is good", context do
    db_name = context[:db_name]
    {:ok, _} = create_doc(db_name, sample_doc_foo())
    resp = Couch.get("/#{db_name}/foo", query: %{:revs_info => true})
    assert hd(resp.body["_revs_info"])["status"] == "available", "Revs info is available"
  end

  @tag :with_db
  test "A document read with etag works", context do
    db_name = context[:db_name]
    {:ok, resp} = create_doc(db_name, sample_doc_foo())
    etag = ~s("#{resp.body["rev"]}")
    resp = Couch.get("/#{db_name}/foo", headers: ["If-None-Match": etag])
    assert resp.status_code == 304, "Should be 304 Not Modified"
    assert resp.headers[:"Content-Length"] == "0", "Should have zero content length"
    assert resp.body == "", "Should have an empty body"
  end

  @tag :with_db
  test "Make sure you can do a seq=true option", context do
    db_name = context[:db_name]
    {:ok, _} = create_doc(db_name, sample_doc_foo())
    resp = Couch.get("/#{db_name}/foo", query: %{:local_seq => true})
    assert is_binary(resp.body["_local_seq"]), "Local seq value is a binary"
  end

  @tag :with_db
  test "Can create several documents", context do
    db_name = context[:db_name]
    assert Couch.post("/#{db_name}", body: %{:_id => "1", :a => 2, :b => 4}).body["ok"]
    assert Couch.post("/#{db_name}", body: %{:_id => "2", :a => 3, :b => 9}).body["ok"]
    assert Couch.post("/#{db_name}", body: %{:_id => "3", :a => 4, :b => 16}).body["ok"]

    retry_until(fn ->
      Couch.get("/#{db_name}").body["doc_count"] == 3
    end)
  end

  @tag :pending
  @tag :with_db
  test "Regression test for COUCHDB-954", context do
    db_name = context[:db_name]
    doc = %{:_id => "COUCHDB-954", :a => 1}

    resp1 = Couch.post("/#{db_name}", body: doc)
    assert resp1.body["ok"]
    old_rev = resp1.body["rev"]

    doc = Map.put(doc, :_rev, old_rev)
    resp2 = Couch.post("/#{db_name}", body: doc)
    assert resp2.body["ok"]
    _new_rev = resp2.body["rev"]

    # TODO: enable chunked encoding
    # resp3 = Couch.get("/#{db_name}/COUCHDB-954", [query: %{:open_revs => "[#{old_rev}, #{new_rev}]"}])
    # assert length(resp3.body) == 2, "Should get two revisions back"
    # resp3 = Couch.get("/#{db_name}/COUCHDB-954", [query: %{:open_revs => "[#{old_rev}]", :latest => true}])
    # assert resp3.body["_rev"] == new_rev
  end

  @tag :with_db
  test "Simple map functions", context do
    db_name = context[:db_name]
    map_fun = "function(doc) { if (doc.a==4) { emit(null, doc.b); } }"
    red_fun = "function(keys, values) { return sum(values); }"
    map_doc = %{:views => %{:baz => %{:map => map_fun}}}
    red_doc = %{:views => %{:baz => %{:map => map_fun, :reduce => red_fun}}}

    # Bootstrap database and ddoc
    assert Couch.post("/#{db_name}", body: %{:_id => "0", :a => 1, :b => 1}).body["ok"]
    assert Couch.post("/#{db_name}", body: %{:_id => "1", :a => 2, :b => 4}).body["ok"]
    assert Couch.post("/#{db_name}", body: %{:_id => "2", :a => 3, :b => 9}).body["ok"]
    assert Couch.post("/#{db_name}", body: %{:_id => "3", :a => 4, :b => 16}).body["ok"]
    assert Couch.put("/#{db_name}/_design/foo", body: map_doc).body["ok"]
    assert Couch.put("/#{db_name}/_design/bar", body: red_doc, query: [w: 3]).body["ok"]
    assert Couch.get("/#{db_name}").body["doc_count"] == 6

    # Initial view query test
    resp = Couch.get("/#{db_name}/_design/foo/_view/baz")
    assert resp.body["total_rows"] == 1
    assert hd(resp.body["rows"])["value"] == 16

    # Modified doc and test for updated view results
    doc0 = Couch.get("/#{db_name}/0").body
    doc0 = Map.put(doc0, :a, 4)
    assert Couch.put("/#{db_name}/0", body: doc0).body["ok"]

    retry_until(fn ->
      Couch.get("/#{db_name}/_design/foo/_view/baz").body["total_rows"] == 2
    end)

    # Write 2 more docs and test for updated view results
    assert Couch.post("/#{db_name}", body: %{:a => 3, :b => 9}).body["ok"]
    assert Couch.post("/#{db_name}", body: %{:a => 4, :b => 16}).body["ok"]

    retry_until(fn ->
      Couch.get("/#{db_name}/_design/foo/_view/baz").body["total_rows"] == 3
    end)

    assert Couch.get("/#{db_name}").body["doc_count"] == 8

    # Disabling until we figure out reduce functions
    # # Test reduce function
    # resp = Couch.get("/#{db_name}/_design/bar/_view/baz")
    # assert hd(resp.body["rows"])["value"] == 33

    # Test reduce function
    resp = Couch.get("/#{db_name}/_design/bar/_view/baz", query: %{:reduce => false})
    assert resp.body["total_rows"] == 3

    # Delete doc and test for updated view results
    doc0 = Couch.get("/#{db_name}/0").body
    assert Couch.delete("/#{db_name}/0?rev=#{doc0["_rev"]}").body["ok"]

    # Disabling until we figure out reduce functions
    # retry_until(fn ->
    #  Couch.get("/#{db_name}/_design/foo/_view/baz").body["total_rows"] == 2
    # end)

    resp = Couch.get("/#{db_name}/_design/bar/_view/baz", query: %{:reduce => false})
    assert resp.body["total_rows"] == 2

    assert Couch.get("/#{db_name}").body["doc_count"] == 7
    assert Couch.get("/#{db_name}/0").status_code == 404

    # No longer true. Old revisions are not stored after
    # an update.
    # refute Couch.get("/#{db_name}/0?rev=#{doc0["_rev"]}").status_code == 404
  end

  @tag :with_db
  test "POST doc response has a Location header", context do
    db_name = context[:db_name]
    resp = Couch.post("/#{db_name}", body: %{:foo => :bar})
    assert resp.body["ok"]
    loc = resp.headers["Location"]
    assert loc, "should have a Location header"
    locs = Enum.reverse(String.split(loc, "/"))
    assert hd(locs) == resp.body["id"]
    assert hd(tl(locs)) == db_name
  end

  @tag :with_db
  test "POST doc with an _id field isn't overwritten by uuid", context do
    db_name = context[:db_name]
    resp = Couch.post("/#{db_name}", body: %{:_id => "oppossum", :yar => "matey"})
    assert resp.body["ok"]
    assert resp.body["id"] == "oppossum"
    assert Couch.get("/#{db_name}/oppossum").body["yar"] == "matey"
  end

  @tag :pending
  @tag :with_db
  test "PUT doc has a Location header", context do
    db_name = context[:db_name]
    resp = Couch.put("/#{db_name}/newdoc", body: %{:a => 1})
    assert String.ends_with?(resp.headers["location"], "/#{db_name}/newdoc")
    # TODO: make protocol check use defined protocol value
    assert String.starts_with?(resp.headers["location"], "http")
  end

  @tag :with_db
  test "DELETE'ing a non-existent doc should 404", context do
    db_name = context[:db_name]
    assert Couch.delete("/#{db_name}/doc-does-not-exist").status_code == 404
  end

  @tag :with_db
  test "Check for invalid document members", context do
    db_name = context[:db_name]

    bad_docs = [
      {:goldfish, %{:_zing => 4}},
      {:zebrafish, %{:_zoom => "hello"}},
      {:mudfish, %{:zane => "goldfish", :_fan => "something smells delicious"}},
      {:tastyfish, %{:_bing => %{"wha?" => "soda can"}}}
    ]

    Enum.each(bad_docs, fn {id, doc} ->
      resp = Couch.put("/#{db_name}/#{id}", body: doc)
      assert resp.status_code == 400
      assert resp.body["error"] == "doc_validation"

      resp = Couch.post("/#{db_name}", body: doc)
      assert resp.status_code == 400
      assert resp.body["error"] == "doc_validation"
    end)
  end

  @tag :with_db
  test "PUT error when body not an object", context do
    db_name = context[:db_name]
    resp = Couch.put("/#{db_name}/bar", body: "[]")
    assert resp.status_code == 400
    assert resp.body["error"] == "bad_request"
    assert resp.body["reason"] == "Document must be a JSON object"
  end

  @tag :with_db
  test "_bulk_docs POST error when body not an object", context do
    db_name = context[:db_name]
    resp = Couch.post("/#{db_name}/_bulk_docs", body: "[]")
    assert resp.status_code == 400
    assert resp.body["error"] == "bad_request"
    assert resp.body["reason"] == "Request body must be a JSON object"
  end

  @tag :with_db
  test "_all_docs POST error when multi-get is not a {'key': [...]} structure", context do
    db_name = context[:db_name]
    resp = Couch.post("/#{db_name}/_all_docs", body: "[]")
    assert resp.status_code == 400
    assert resp.body["error"] == "bad_request"
    assert resp.body["reason"] == "Request body must be a JSON object"

    resp = Couch.post("/#{db_name}/_all_docs", body: %{:keys => 1})
    assert resp.status_code == 400
    assert resp.body["error"] == "bad_request"
    assert resp.body["reason"] == "`keys` body member must be an array."
  end

  @tag :with_db
  test "oops, the doc id got lost in code nirwana", context do
    db_name = context[:db_name]
    resp = Couch.delete("/#{db_name}/?rev=foobarbaz")
    assert resp.status_code == 400, "should return a bad request"
    assert resp.body["error"] == "bad_request"

    assert resp.body["reason"] ==
             "You tried to DELETE a database with a ?=rev parameter. Did you mean to DELETE a document instead?"
  end

  @tag :pending
  @tag :with_db
  test "On restart, a request for creating an already existing db can not override",
       _context do
    # TODO
    assert true
  end

  @tag :with_db
  test "_all_docs/queries works", context do
    db_name = context[:db_name]

    resp = Couch.post("/#{db_name}/_all_docs/queries", body: %{:queries => []})
    assert resp.status_code == 200
    assert resp.body["results"] == []

    assert Couch.put("/#{db_name}/doc1", body: %{:a => 1}).body["ok"]

    body = %{
        :queries => [
            %{:limit => 1},
            %{:limit => 0}
        ]
    }
    resp = Couch.post("/#{db_name}/_all_docs/queries", body: body)
    assert resp.status_code == 200

    assert Map.has_key?(resp.body, "results")
    results = Enum.sort(resp.body["results"])
    assert length(results) == 2
    [res1, res2] = results

    assert res1 == %{"offset" => :null, "rows" => [], "total_rows" => 1}

    assert res2["offset"] == :null
    assert res2["total_rows"] == 1
    rows = res2["rows"]

    assert length(rows) == 1
    [row] = rows
    assert row["id"] == "doc1"
    assert row["key"] == "doc1"

    val = row["value"]
    assert Map.has_key?(val, "rev")
  end

  @tag :with_db
  test "_design_docs works", context do
    db_name = context[:db_name]
    body = %{:a => 1}

    resp = Couch.get("/#{db_name}/_design_docs")
    assert resp.status_code == 200
    assert resp.body == %{"offset" => :null, "rows" => [], "total_rows" => 0}

    assert Couch.put("/#{db_name}/doc1", body: body).body["ok"]

    # Make sure regular documents didn't get picked up
    resp = Couch.get("/#{db_name}/_design_docs")
    assert resp.status_code == 200
    assert resp.body == %{"offset" => :null, "rows" => [], "total_rows" => 0}

    # Add _design/doc1
    assert Couch.put("/#{db_name}/_design/doc1", body: body).body["ok"]
    resp = Couch.get("/#{db_name}/_design_docs")
    assert resp.status_code == 200
    assert resp.body["total_rows"] == 1
    [row] = resp.body["rows"]

    assert row["id"] == "_design/doc1"
    assert row["key"] == "_design/doc1"

    val = row["value"]
    assert Map.has_key?(val, "rev")

    # Add _design/doc5
    assert Couch.put("/#{db_name}/_design/doc5", body: body).body["ok"]
    resp = Couch.get("/#{db_name}/_design_docs")
    assert resp.status_code == 200
    [row1, row2] = resp.body["rows"]
    assert row1["id"] == "_design/doc1"
    assert row2["id"] == "_design/doc5"

    # descending=true
    resp = Couch.get("/#{db_name}/_design_docs?descending=true")
    assert resp.status_code == 200
    [row1, row2] = resp.body["rows"]
    assert row1["id"] == "_design/doc5"
    assert row2["id"] == "_design/doc1"

    # start_key=doc2
    resp = Couch.get("/#{db_name}/_design_docs?start_key=\"_design/doc2\"")
    assert resp.status_code == 200
    [row] = resp.body["rows"]
    assert row["id"] == "_design/doc5"

    # end_key=doc2
    resp = Couch.get("/#{db_name}/_design_docs?end_key=\"_design/doc2\"")
    assert resp.status_code == 200
    [row] = resp.body["rows"]
    assert row["id"] == "_design/doc1"

    # inclusive_end=false
    qstr = "start_key=\"_design/doc2\"&end_key=\"_design/doc5\"&inclusive_end=false"
    resp = Couch.get("/#{db_name}/_design_docs?" <> qstr)
    assert resp.status_code == 200
    assert resp.body == %{"offset" => :null, "rows" => [], "total_rows" => 2}

    # update_seq=true
    resp = Couch.get("/#{db_name}/_design_docs?update_seq=true")
    assert resp.status_code == 200
    assert Map.has_key?(resp.body, "update_seq")
  end

  @tag :with_db
  test "_local_docs works", context do
    db_name = context[:db_name]
    body = %{:a => 1}

    resp = Couch.get("/#{db_name}/_local_docs")
    assert resp.status_code == 200
    assert resp.body == %{"offset" => :null, "rows" => [], "total_rows" => 0}

    # Add _local/doc1
    assert Couch.put("/#{db_name}/_local/doc1", body: body).body["ok"]
    resp = Couch.get("/#{db_name}/_local_docs")
    assert resp.status_code == 200
    assert resp.body["total_rows"] == 1
    [row] = resp.body["rows"]

    assert row["id"] == "_local/doc1"
    assert row["key"] == "_local/doc1"

    val = row["value"]
    assert Map.has_key?(val, "rev")

    # Add _local/doc5
    # Use a body > 100Kb to tests local docs chunkifier
    body = %{:b => String.duplicate("b", 110_000)}
    assert Couch.put("/#{db_name}/_local/doc5", body: body).body["ok"]
    resp = Couch.get("/#{db_name}/_local_docs")
    assert resp.status_code == 200
    [row1, row2] = resp.body["rows"]
    assert row1["id"] == "_local/doc1"
    assert row2["id"] == "_local/doc5"

    # descending=true
    resp = Couch.get("/#{db_name}/_local_docs?descending=true")
    assert resp.status_code == 200
    [row1, row2] = resp.body["rows"]
    assert row1["id"] == "_local/doc5"
    assert row2["id"] == "_local/doc1"

    # start_key=doc2
    resp = Couch.get("/#{db_name}/_local_docs?start_key=\"_local/doc2\"")
    assert resp.status_code == 200
    [row] = resp.body["rows"]
    assert row["id"] == "_local/doc5"

    # end_key=doc2
    resp = Couch.get("/#{db_name}/_local_docs?end_key=\"_local/doc2\"")
    assert resp.status_code == 200
    [row] = resp.body["rows"]
    assert row["id"] == "_local/doc1"

    # inclusive_end=false
    qstr = "start_key=\"_local/doc2\"&end_key=\"_local/doc5\"&inclusive_end=false"
    resp = Couch.get("/#{db_name}/_local_docs?" <> qstr)
    assert resp.status_code == 200
    assert resp.body == %{"offset" => :null, "rows" => [], "total_rows" => 2}

    # update_seq=true
    resp = Couch.get("/#{db_name}/_local_docs?update_seq=true")
    assert resp.status_code == 200
    assert Map.has_key?(resp.body, "update_seq")
  end

  @tag :with_db
  test "Check _revs_limit", context do
    db_name = context[:db_name]

    resp = Couch.get("/#{db_name}/_revs_limit")
    assert resp.status_code == 200
    assert resp.body == 1000

    body = "999"
    resp = Couch.put("/#{db_name}/_revs_limit", body: "999")
    assert resp.status_code == 200
    assert resp.body["ok"] == true

    resp = Couch.get("/#{db_name}/_revs_limit")
    assert resp.status_code == 200
    assert resp.body == 999
  end
end

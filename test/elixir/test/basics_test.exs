defmodule BasicsTest do
  use CouchTestCase

  @moduletag :basics

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
  test "Limit and skip should work in _all_dbs", context do
    db = context[:db_name]
    db_count = length(Couch.get("/_all_dbs").body)
    assert db_count > 0
    assert Couch.get("/_all_dbs?limit=0").body == []
    assert length(Couch.get("/_all_dbs?limit=1").body) >= 1
    assert length(Couch.get("/_all_dbs?skip=1").body) == (db_count - 1)
    assert [db] == Couch.get("/_all_dbs?start_key=\"#{db}\"&limit=1").body
  end

  test "Database name with '+' should encode to '+'", _context do
    set_config({"chttpd", "decode_plus_to_space", "false"})

    random_number = :rand.uniform(16_000_000)
    db_name = "random+test+db+#{random_number}"
    resp = Couch.put("/#{db_name}")

    assert resp.status_code == 201
    assert resp.body["ok"] == true

    resp = Couch.get("/#{db_name}")

    assert resp.status_code == 200
    assert resp.body["db_name"] == db_name
  end

  test "Database name with '%2B' should encode to '+'", _context do
    set_config({"chttpd", "decode_plus_to_space", "true"})

    random_number = :rand.uniform(16_000_000)
    db_name = "random%2Btest%2Bdb2%2B#{random_number}"
    resp = Couch.put("/#{db_name}")

    assert resp.status_code == 201
    assert resp.body["ok"] == true

    resp = Couch.get("/#{db_name}")

    assert resp.status_code == 200
    assert resp.body["db_name"] == "random+test+db2+#{random_number}"
  end

  @tag :with_db
  test "'+' in document name should encode to '+'", context do
    set_config({"chttpd", "decode_plus_to_space", "false"})

    db_name = context[:db_name]
    doc_id = "test+doc"
    resp = Couch.put("/#{db_name}/#{doc_id}", body: %{})

    assert resp.status_code == 201
    assert resp.body["id"] == "test+doc"
  end

  @tag :with_db
  test "'+' in document name should encode to space", context do
    set_config({"chttpd", "decode_plus_to_space", "true"})

    db_name = context[:db_name]
    doc_id = "test+doc+2"
    resp = Couch.put("/#{db_name}/#{doc_id}", body: %{})

    assert resp.status_code == 201
    assert resp.body["id"] == "test doc 2"
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
    assert resp.body["_local_seq"] == 1, "Local seq value == 1"
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

    # Test reduce function
    resp = Couch.get("/#{db_name}/_design/bar/_view/baz")
    assert hd(resp.body["rows"])["value"] == 33

    # Delete doc and test for updated view results
    doc0 = Couch.get("/#{db_name}/0").body
    assert Couch.delete("/#{db_name}/0?rev=#{doc0["_rev"]}").body["ok"]

    retry_until(fn ->
      Couch.get("/#{db_name}/_design/foo/_view/baz").body["total_rows"] == 2
    end)

    assert Couch.get("/#{db_name}").body["doc_count"] == 7
    assert Couch.get("/#{db_name}/0").status_code == 404
    refute Couch.get("/#{db_name}/0?rev=#{doc0["_rev"]}").status_code == 404
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
  test "Default headers are returned for doc with open_revs=all", context do
    db_name = context[:db_name]
    post_response = Couch.post("/#{db_name}", body: %{:foo => :bar})
    id = post_response.body["id"]
    head_response = Couch.head("/#{db_name}/#{id}?open_revs=all")
    assert head_response.headers["X-Couch-Request-ID"]
    assert head_response.headers["X-CouchDB-Body-Time"]
  end

  @tag :with_db
  test "request ID can be specified at the client", _context do
    uuid = "E7498DE1-B661-42FA-943D-17F890143068"
    resp = Couch.get("/", headers: ["X-Couch-Request-ID": uuid])
    assert resp.headers["X-Couch-Request-ID"] == uuid
  end
end

defmodule BasicsTest do
  use CouchTestCase

  @moduledoc """
  Test CouchDB basics.
  This is a port of the basics.js suite
  """

  test "Session contains adm context" do
    userCtx = Couch.get("/_session").body["userCtx"]
    assert userCtx["name"] == "adm", "Should have adm user context"
    assert userCtx["roles"] == ["_admin"], "Should have _admin role"
  end

  test "Welcome endpoint" do
    assert Couch.get("/").body["couchdb"] == "Welcome", "Should say welcome"
  end

  @tag :with_db
  test "PUT on existing DB should return 412 instead of 500", context do
    db_name = context[:db_name]
    assert Couch.put("/#{db_name}").status_code == 412
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
  test "Creating a new DB with slashes should return Location header (COUCHDB-411)", context do
    db_name = context[:db_name] <> "%2Fwith_slashes"
    {:ok, resp} = create_db(db_name)
    msg = "Should return Location header for new db"
    assert String.ends_with?(resp.headers["location"], db_name), msg
    {:ok, _} = delete_db(db_name)
  end

  @tag :with_db
  test "Created database has appropriate db info name", context do
    db_name = context[:db_name]
    assert Couch.get("/#{db_name}").body["db_name"] == db_name, "Get correct database name"
  end

  @tag :with_db
  test "Database should be in _all_dbs", context do
    assert context[:db_name] in Couch.get("/_all_dbs").body, "Db name in _all_dbs"
  end

  @tag :with_db
  test "Empty database should have zero docs", context do
    assert Couch.get("/#{context[:db_name]}").body["doc_count"] == 0, "Empty doc count in empty db"
  end

  @tag :with_db
  test "Create a document and save it to the database", context do
    resp = Couch.post("/#{context[:db_name]}", [body: %{:_id => "0", :a => 1, :b => 1}])
    assert resp.status_code == 201, "Should be 201 created"
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
    resp = Couch.get("/#{db_name}/foo", [query: %{:revs_info => true}])
    assert hd(resp.body["_revs_info"])["status"] == "available", "Revs info is available"
  end

  @tag :with_db
  test "Make sure you can do a seq=true option", context do
    db_name = context[:db_name]
    {:ok, _} = create_doc(db_name, sample_doc_foo())
    resp = Couch.get("/#{db_name}/foo", [query: %{:local_seq => true}])
    assert resp.body["_local_seq"] == 1, "Local seq value == 1"
  end

  @tag :with_db
  test "Can create several documents", context do
    db_name = context[:db_name]
    assert Couch.post("/#{db_name}", [body: %{:_id => "1", :a => 2, :b => 4}]).body["ok"]
    assert Couch.post("/#{db_name}", [body: %{:_id => "2", :a => 3, :b => 9}])
    assert Couch.post("/#{db_name}", [body: %{:_id => "3", :a => 4, :b => 16}]).body["ok"]
    assert Couch.get("/#{db_name}").body["doc_count"] == 3
  end

  @tag :with_db
  test "Regression test for COUCHDB-954", context do
    db_name = context[:db_name]
    doc = %{:_id => "COUCHDB-954", :a => 1}

    resp1 = Couch.post("/#{db_name}", [body: doc])
    assert resp1.body["ok"]
    old_rev = resp1.body["rev"]

    doc = Map.put(doc, :_rev, old_rev)
    resp2 = Couch.post("/#{db_name}", [body: doc])
    assert resp2.body["ok"]
    new_rev = resp2.body["rev"]

    # TODO: enable chunked encoding
    #resp3 = Couch.get("/#{db_name}/COUCHDB-954", [query: %{:open_revs => "[#{old_rev}, #{new_rev}]"}])
    #assert length(resp3.body) == 2, "Should get two revisions back"
    #resp3 = Couch.get("/#{db_name}/COUCHDB-954", [query: %{:open_revs => "[#{old_rev}]", :latest => true}])
    #assert resp3.body["_rev"] == new_rev
  end


end

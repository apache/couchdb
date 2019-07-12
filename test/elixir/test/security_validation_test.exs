defmodule SecurityValidationTest do
  use CouchTestCase

  @moduletag :security
  @moduletag kind: :single_node

  @moduledoc """
  Test CouchDB Security Validations
  This is a port of the security_validation.js suite
  """

  @auth_headers %{
    jerry: [
      # jerry:mouse
      authorization: "Basic amVycnk6bW91c2U="
    ],
    tom: [
      # tom:cat
      authorization: "Basic dG9tOmNhdA=="
    ],
    spike_cat: [
      # spike:cat - which is wrong
      authorization: "Basic c3Bpa2U6Y2F0"
    ]
  }

  @ddoc %{
    _id: "_design/test",
    language: "javascript",
    validate_doc_update: ~s"""
      (function (newDoc, oldDoc, userCtx, secObj) {
        if (secObj.admin_override) {
          if (userCtx.roles.indexOf('_admin') != -1) {
            // user is admin, they can do anything
            return true;
          }
        }
        // docs should have an author field.
        if (!newDoc._deleted && !newDoc.author) {
          throw {forbidden:
              \"Documents must have an author field\"};
        }
        if (oldDoc && oldDoc.author != userCtx.name) {
            throw {unauthorized:
                \"You are '\" + userCtx.name + \"', not the author '\" + oldDoc.author + \"' of this document. You jerk.\"};
        }
      })
    """
  }

  setup_all do
    auth_db_name = random_db_name()
    {:ok, _} = create_db(auth_db_name)
    on_exit(fn -> delete_db(auth_db_name) end)

    configs = [
      {"chttpd_auth", "authentication_db", auth_db_name}
    ]

    Enum.each(configs, &set_config/1)

    # port of comment from security_validation.js
    # the special case handler does not exist (any longer) in clusters, so we have
    # to replicate the behavior using a "normal" DB even though tests might no more
    # run universally (why the "X-Couch-Test-Auth" header was introduced).
    # btw: this needs to be INSIDE configured server to propagate correctly ;-)
    # At least they'd run in the build, though
    users = [{"tom", "cat"}, {"jerry", "mouse"}, {"spike", "dog"}]

    Enum.each(users, fn {name, pass} ->
      doc = %{
        :_id => "org.couchdb.user:#{name}",
        :type => "user",
        :name => name,
        :roles => [],
        :password => pass
      }

      assert Couch.post("/#{auth_db_name}", body: doc).body["ok"]
    end)

    {:ok, [auth_db_name: auth_db_name]}
  end

  @tag :with_db_name
  test "Saving document using the wrong credentials", context do
    # spike:cat - which is wrong
    headers = @auth_headers[:spike_cat]
    resp = Couch.post("/#{context[:db_name]}", body: %{foo: 1}, headers: headers)
    assert resp.body["error"] == "unauthorized"
    assert resp.status_code == 401
  end

  test "Force basic login" do
    # spike:cat - which is wrong
    headers = @auth_headers[:spike_cat]
    resp = Couch.get("/_session", query: %{basic: true}, headers: headers)
    assert resp.status_code == 401
    assert resp.body["error"] == "unauthorized"
  end

  @tag :with_db
  test "Jerry can save a document normally", context do
    headers = @auth_headers[:jerry]
    assert Couch.get("/_session", headers: headers).body["userCtx"]["name"] == "jerry"

    doc = %{_id: "testdoc", foo: 1, author: "jerry"}
    assert Couch.post("/#{context[:db_name]}", body: doc).body["ok"]
  end

  @tag :with_db
  test "Non-admin user cannot save a ddoc", context do
    headers = @auth_headers[:jerry]
    resp = Couch.post("/#{context[:db_name]}", body: @ddoc, headers: headers)
    assert resp.status_code == 403
    assert resp.body["error"] == "forbidden"
  end

  @tag :with_db
  test "Ddoc writes with admin and replication contexts", context do
    db_name = context[:db_name]
    sec_obj = %{admins: %{names: ["jerry"]}}

    assert Couch.put("/#{db_name}/_security", body: sec_obj).body["ok"]
    assert Couch.post("/#{db_name}", body: @ddoc).body["ok"]

    new_rev = "2-642e20f96624a0aae6025b4dba0c6fb2"
    ddoc = @ddoc |> Map.put(:_rev, new_rev) |> Map.put(:foo, "bar")
    headers = @auth_headers[:tom]
    # attempt to save doc in replication context, eg ?new_edits=false
    resp =
      Couch.put(
        "/#{db_name}/#{ddoc[:_id]}",
        body: ddoc,
        headers: headers,
        query: %{new_edits: false}
      )

    assert resp.status_code == 403
    assert resp.body["error"] == "forbidden"
  end

  test "_session API" do
    headers = @auth_headers[:jerry]
    resp = Couch.get("/_session", headers: headers)
    assert resp.body["userCtx"]["name"] == "jerry"
    assert resp.body["userCtx"]["roles"] == []
  end

  @tag :with_db
  test "Author presence and user security", context do
    db_name = context[:db_name]
    sec_obj = %{admin_override: false, admins: %{names: ["jerry"]}}

    jerry = @auth_headers[:jerry]
    tom = @auth_headers[:tom]

    assert Couch.put("/#{db_name}/_security", body: sec_obj).body["ok"]
    assert Couch.post("/#{db_name}", body: @ddoc).body["ok"]

    retry_until(fn ->
      resp = Couch.put("/#{db_name}/test_doc", body: %{foo: 1}, headers: jerry)
      assert resp.status_code == 403
      assert resp.body["error"] == "forbidden"
      assert resp.body["reason"] == "Documents must have an author field"
    end)

    # Jerry can write the document
    assert Couch.put(
             "/#{db_name}/test_doc",
             body: %{foo: 1, author: "jerry"},
             headers: jerry
           ).body["ok"]

    test_doc = Couch.get("/#{db_name}/test_doc").body

    # Tom cannot write the document
    resp = Couch.post("/#{db_name}", body: %{foo: 1}, headers: tom)
    assert resp.status_code == 403
    assert resp.body["error"] == "forbidden"

    # Enable admin override for changing author values
    assert Couch.put("/#{db_name}/_security", body: %{sec_obj | admin_override: true}).body[
             "ok"
           ]

    # Change owner to Tom
    test_doc = Map.put(test_doc, "author", "tom")
    resp = Couch.put("/#{db_name}/test_doc", body: test_doc)
    assert resp.body["ok"]
    test_doc = Map.put(test_doc, "_rev", resp.body["rev"])

    # Now Tom can update the document
    test_doc = Map.put(test_doc, "foo", "asdf")
    resp = Couch.put("/#{db_name}/test_doc", body: test_doc, headers: tom)
    assert resp.body["ok"]
    test_doc = Map.put(test_doc, "_rev", resp.body["rev"])

    # Jerry can't delete it
    retry_until(fn ->
      opts = [headers: jerry]
      resp = Couch.delete("/#{db_name}/test_doc?rev=#{test_doc["_rev"]}", opts)
      resp.status_code == 401 and resp.body["error"] == "unauthorized"
    end)
  end
end

# TODO: port remainder of security_validation.js suite
# remaining bits reproduced below:
#
#      // try to do something lame
#      try {
#        db.setDbProperty("_security", ["foo"]);
#        T(false && "can't do this");
#      } catch(e) {}
#
#      // go back to normal
#      T(db.setDbProperty("_security", {admin_override : false}).ok);
#
#      // Now delete document
#      T(user2Db.deleteDoc(doc).ok);
#
#      // now test bulk docs
#      var docs = [{_id:"bahbah",author:"jerry",foo:"bar"},{_id:"fahfah",foo:"baz"}];
#
#      // Create the docs
#      var results = db.bulkSave(docs);
#
#      T(results[0].rev)
#      T(results[0].error == undefined)
#      T(results[1].rev === undefined)
#      T(results[1].error == "forbidden")
#
#      T(db.open("bahbah"));
#      T(db.open("fahfah") == null);
#
#
#      // now all or nothing with a failure - no more available on cluster
# /*      var docs = [
#           {_id:"booboo",author:"Damien Katz",foo:"bar"},{_id:"foofoo",foo:"baz"}
#         ];
#
#      // Create the docs
#      var results = db.bulkSave(docs, {all_or_nothing:true});
#
#      T(results.errors.length == 1);
#      T(results.errors[0].error == "forbidden");
#      T(db.open("booboo") == null);
#      T(db.open("foofoo") == null);
# */
#
#      // Now test replication
#      var AuthHeaders = {"Authorization": "Basic c3Bpa2U6ZG9n"}; // spike
#      adminDbA = new CouchDB("" + db_name + "_a", {"X-Couch-Full-Commit":"false"});
#      adminDbB = new CouchDB("" + db_name + "_b", {"X-Couch-Full-Commit":"false"});
#      var dbA = new CouchDB("" + db_name + "_a", AuthHeaders);
#      var dbB = new CouchDB("" + db_name + "_b", AuthHeaders);
#      // looping does not really add value as the scenario is the same anyway
#      // (there's nothing 2 be gained from it)
#      var A = CouchDB.protocol + CouchDB.host + "/" + db_name + "_a";
#      var B = CouchDB.protocol + CouchDB.host + "/" + db_name + "_b";
#
#      // (the databases never exist b4 - and we made sure they're deleted below)
#      //adminDbA.deleteDb();
#      adminDbA.createDb();
#      //adminDbB.deleteDb();
#      adminDbB.createDb();
#
#      // save and replicate a documents that will and will not pass our design
#      // doc validation function.
#      T(dbA.save({_id:"foo1",value:"a",author:"tom"}).ok);
#      T(dbA.save({_id:"foo2",value:"a",author:"spike"}).ok);
#      T(dbA.save({_id:"bad1",value:"a"}).ok);
#
#      T(CouchDB.replicate(A, B, {headers:AuthHeaders}).ok);
#      T(CouchDB.replicate(B, A, {headers:AuthHeaders}).ok);
#
#      T(dbA.open("foo1"));
#      T(dbB.open("foo1"));
#      T(dbA.open("foo2"));
#      T(dbB.open("foo2"));
#
#      // save the design doc to dbA
#      delete designDoc._rev; // clear rev from previous saves
#      T(adminDbA.save(designDoc).ok);
#
#      // no affect on already saved docs
#      T(dbA.open("bad1"));
#
#      // Update some docs on dbB. Since the design hasn't replicated, anything
#      // is allowed.
#
#      // this edit will fail validation on replication to dbA (no author)
#      T(dbB.save({_id:"bad2",value:"a"}).ok);
#
#      // this edit will fail security on replication to dbA (wrong author
#      //  replicating the change)
#      var foo1 = dbB.open("foo1");
#      foo1.value = "b";
#      T(dbB.save(foo1).ok);
#
#      // this is a legal edit
#      var foo2 = dbB.open("foo2");
#      foo2.value = "b";
#      T(dbB.save(foo2).ok);
#
#      var results = CouchDB.replicate({"url": B, "headers": AuthHeaders},
#         {"url": A, "headers": AuthHeaders}, {headers:AuthHeaders});
#      T(results.ok);
#      TEquals(1, results.history[0].docs_written);
#      TEquals(2, results.history[0].doc_write_failures);
#
#      // bad2 should not be on dbA
#      T(dbA.open("bad2") == null);
#
#      // The edit to foo1 should not have replicated.
#      T(dbA.open("foo1").value == "a");
#
#      // The edit to foo2 should have replicated.
#      T(dbA.open("foo2").value == "b");
#    });
#
#  // cleanup
#  db.deleteDb();
#  if(adminDbA){
#    adminDbA.deleteDb();
#  }
#  if(adminDbB){
#    adminDbB.deleteDb();
#  }
#  authDb.deleteDb();
#  // have to clean up authDb on the backside :(
#  var req = CouchDB.newXhr();
#  req.open("DELETE", "http://127.0.0.1:15986/" + authDb_name, false);
#  req.send("");
#  CouchDB.maybeThrowError(req);
# };

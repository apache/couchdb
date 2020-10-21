defmodule SecurityValidationTest do
  use CouchTestCase

  @moduletag :security

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
    ],
    spike: [
      # spike:dog
      authorization: "Basic c3Bpa2U6ZG9n"
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
      {"httpd", "authentication_handlers",
       "{couch_httpd_auth, cookie_authentication_handler}, {couch_httpd_auth, default_authentication_handler}"},
      {"couch_httpd_auth", "authentication_db", auth_db_name},
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
  test "try to set a wrong value for _security", context do
    db_name = context[:db_name]
    # try to do something lame
    resp = Couch.put("/#{db_name}/_security", body: ["foo"])
    assert resp.status_code == 400
    assert resp.body["error"] == "bad_request"
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

    # Admin cannot write the document (admin_override = false)
    test_doc = Map.put(test_doc, "foo", 3)
    resp = Couch.put("/#{db_name}/test_doc", body: test_doc)
    assert resp.status_code == 401
    assert resp.body["error"] == "unauthorized"

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

    # Admin can write the document (admin_override = true)
    test_doc = Map.put(test_doc, "foo", 4)
    resp = Couch.put("/#{db_name}/test_doc", body: test_doc)
    assert resp.body["ok"]

    # Disable admin override
    assert Couch.put("/#{db_name}/_security", body: %{sec_obj | admin_override: false}).body[
             "ok"
           ]

    docs = [%{_id: "bahbah", author: "jerry", foo: "bar"}, %{_id: "fahfah", foo: "baz"}]

    resp =
      Couch.post(
        "/#{db_name}/_bulk_docs",
        body: %{
          docs: docs
        },
        headers: jerry
      )

    assert Enum.at(resp.body, 0)["rev"]
    assert !Enum.at(resp.body, 0)["error"]
    assert !Enum.at(resp.body, 1)["rev"]
    assert Enum.at(resp.body, 1)["error"] == "forbidden"

    resp = Couch.get("/#{db_name}/bahbah")
    assert resp.status_code == 200

    resp = Couch.get("/#{db_name}/fahfah")
    assert resp.status_code == 404
  end

  test "Author presence and user security when replicated", _context do
    db_name = random_db_name()
    db_name_a = "#{db_name}_a"
    db_name_b = "#{db_name}_b"
    create_db(db_name_a)
    create_db(db_name_b)
    on_exit(fn -> delete_db(db_name_a) end)
    on_exit(fn -> delete_db(db_name_b) end)

    spike = @auth_headers[:spike]

    # save and replicate a documents that will and will not pass our design
    # doc validation function.
    {:ok, _} = create_doc(db_name_a, %{_id: "foo1", value: "a", author: "tom"})
    {:ok, _} = create_doc(db_name_a, %{_id: "foo2", value: "a", author: "spike"})
    {:ok, _} = create_doc(db_name_a, %{_id: "bad1", value: "a"})
    replicate(db_name_a, db_name_b, headers: spike)
    replicate(db_name_b, db_name_a, headers: spike)

    assert Couch.get("/#{db_name_a}/foo1").status_code == 200
    assert Couch.get("/#{db_name_b}/foo1").status_code == 200
    assert Couch.get("/#{db_name_a}/foo2").status_code == 200
    assert Couch.get("/#{db_name_b}/foo2").status_code == 200

    {:ok, _} = create_doc(db_name_a, @ddoc)

    # no affect on already saved docs
    assert Couch.get("/#{db_name_a}/bad1").status_code == 200

    # Update some docs on dbB. Since the design hasn't replicated, anything
    # is allowed.

    # this edit will fail validation on replication to dbA (no author)
    assert Couch.post(
             "/#{db_name_b}",
             body: %{id: "bad2", value: "a"},
             headers: spike
           ).body["ok"]

    # this edit will fail security on replication to dbA (wrong author
    # replicating the change)
    foo1 = Couch.get("/#{db_name_b}/foo1").body
    foo1 = Map.put(foo1, "value", "b")
    assert Couch.put("/#{db_name_b}/foo1", body: foo1, headers: spike).body["ok"]

    # this is a legal edit
    foo2 = Couch.get("/#{db_name_b}/foo2").body
    foo2 = Map.put(foo2, "value", "b")
    assert Couch.put("/#{db_name_b}/foo2", body: foo2, headers: spike).body["ok"]

    result = replicate(db_name_b, db_name_a, headers: spike)
    assert Enum.at(result["history"], 0)["docs_written"] == 1
    assert Enum.at(result["history"], 0)["doc_write_failures"] == 2

    # bad2 should not be on dbA
    assert Couch.get("/#{db_name_a}/bad2").status_code == 404

    # The edit to foo1 should not have replicated.
    resp = Couch.get("/#{db_name_a}/foo1")
    assert resp.body["value"] == "a"

    # The edit to foo2 should have replicated.
    resp = Couch.get("/#{db_name_a}/foo2")
    assert resp.body["value"] == "b"
  end
end

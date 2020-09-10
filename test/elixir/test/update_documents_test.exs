defmodule UpdateDocumentsTest do
  use CouchTestCase

  @moduletag kind: :single_node

  @ddoc %{
    _id: "_design/update",
    language: "javascript",
    updates: %{
      hello: """
        function(doc, req) {
        if (!doc) {
          if (req.id) {
            return [
            // Creates a new document with the PUT docid,
            { _id : req.id,
              reqs : [req] },
            // and returns an HTML response to the client.
            "<p>New World</p>"];
          };
          //
          return [null, "<p>Empty World</p>"];
        };
        // we can update the document inline
        doc.world = "hello";
        // we can record aspects of the request or use them in application logic.
        doc.reqs && doc.reqs.push(req);
        doc.edited_by = req.userCtx;
        return [doc, "<p>hello doc</p>"];
      }
      """,
      "in-place": """
      function(doc, req) {
        var field = req.query.field;
        var value = req.query.value;
        var message = "set "+field+" to "+value;
        doc[field] = value;
        return [doc, message];
      }
      """,
      "form-update": """
       function(doc, req) {
        for (var field in req.form) {
          doc[field] = req.form[field];
        }
        var message = "updated doc from form";
        return [doc, message];
      }
      """,
      "bump-counter": """
       function(doc, req) {
        if (!doc.counter) doc.counter = 0;
        doc.counter += 1;
        var message = "<h1>bumped it!</h1>";
        return [doc, message];
      }
      """,
      error: """
      function(doc, req) {
       superFail.badCrash;
      }
      """,
      "get-uuid": """
       function(doc, req) {
        return [null, req.uuid];
      }
      """,
      "code-n-bump": """
       function(doc,req) {
        if (!doc.counter) doc.counter = 0;
        doc.counter += 1;
        var message = "<h1>bumped it!</h1>";
        resp = {"code": 302, "body": message}
        return [doc, resp];
      }
      """,
      "resp-code": """
       function(doc,req) {
        resp = {"code": 302}
        return [null, resp];
      }
      """,
      "resp-code-and-json": """
       function(doc,req) {
        resp = {"code": 302, "json": {"ok": true}}
        return [{"_id": req["uuid"]}, resp];
      }
      """,
      binary: """
       function(doc, req) {
        var resp = {
          "headers" : {
            "Content-Type" : "application/octet-stream"
          },
          "base64" : "aGVsbG8gd29ybGQh" // "hello world!" encoded
        };
        return [doc, resp];
      }
      """,
      empty: """
       function(doc, req) {
        return [{}, 'oops'];
      }
      """
    }
  }

  @document %{word: "plankton", name: "Rusty"}

  @tag :with_db
  test "update error invalid path", context do
    db_name = context[:db_name]
    create_doc(db_name, @ddoc)

    resp = Couch.post("/#{db_name}/_design/update/_update/")
    assert resp.status_code == 404
    assert resp.body["reason"] == "Invalid path."
  end

  @tag :with_db
  test "update document", context do
    db_name = context[:db_name]
    create_doc(db_name, @ddoc)
    {:ok, resp} = create_doc(db_name, @document)
    docid = resp.body["id"]

    resp = Couch.put("/#{db_name}/_design/update/_update/hello/#{docid}")
    assert resp.status_code == 201
    assert resp.body == "<p>hello doc</p>"
    assert String.contains?(resp.headers["Content-Type"], "charset=utf-8")
    assert resp.headers["X-Couch-Id"] == docid

    resp = Couch.get("/#{db_name}/#{docid}")
    assert resp.status_code == 200
    assert resp.body["world"] == "hello"

    # Fix for COUCHDB-379
    assert String.starts_with?(resp.headers["Server"], "CouchDB")

    resp = Couch.put("/#{db_name}/_design/update/_update/hello")
    assert resp.status_code == 200
    assert resp.body == "<p>Empty World</p>"
  end

  @tag :with_db
  test "GET is not allowed", context do
    db_name = context[:db_name]
    create_doc(db_name, @ddoc)

    resp = Couch.get("/#{db_name}/_design/update/_update/hello")
    assert resp.body["error"] == "method_not_allowed"
  end

  @tag :with_db
  test "doc can be created", context do
    db_name = context[:db_name]
    create_doc(db_name, @ddoc)

    resp = Couch.get("/#{db_name}/nonExistingDoc")
    assert resp.status_code == 404

    resp = Couch.put("/#{db_name}/_design/update/_update/hello/nonExistingDoc")
    assert resp.status_code == 201
    assert resp.body == "<p>New World</p>"

    resp = Couch.get("/#{db_name}/nonExistingDoc")
    assert resp.status_code == 200
  end

  @tag :with_db
  test "in place update", context do
    db_name = context[:db_name]
    create_doc(db_name, @ddoc)

    {:ok, resp} = create_doc(db_name, @document)
    docid = resp.body["id"]

    resp =
      Couch.put(
        "/#{db_name}/_design/update/_update/in-place/#{docid}?field=title&value=test"
      )

    assert resp.status_code == 201
    assert resp.body == "set title to test"
    resp = Couch.get("/#{db_name}/#{docid}")
    assert resp.status_code == 200
    assert resp.body["title"] == "test"
  end

  @tag :with_db
  test "form update via application/x-www-form-urlencoded", context do
    db_name = context[:db_name]
    create_doc(db_name, @ddoc)

    {:ok, resp} = create_doc(db_name, @document)
    docid = resp.body["id"]

    resp =
      Couch.put(
        "/#{db_name}/_design/update/_update/form-update/#{docid}",
        headers: ["Content-Type": "application/x-www-form-urlencoded"],
        body: "formfoo=bar&formbar=foo"
      )

    assert resp.status_code == 201
    assert resp.body == "updated doc from form"

    resp = Couch.get("/#{db_name}/#{docid}")
    assert resp.status_code == 200
    assert resp.body["formfoo"] == "bar"
    assert resp.body["formbar"] == "foo"
  end

  @tag :with_db
  test "bump counter", context do
    db_name = context[:db_name]
    create_doc(db_name, @ddoc)

    {:ok, resp} = create_doc(db_name, @document)
    docid = resp.body["id"]

    resp =
      Couch.put("/#{db_name}/_design/update/_update/bump-counter/#{docid}",
        headers: ["X-Couch-Full-Commit": "true"]
      )

    assert resp.status_code == 201
    assert resp.body == "<h1>bumped it!</h1>"

    resp = Couch.get("/#{db_name}/#{docid}")
    assert resp.status_code == 200
    assert resp.body["counter"] == 1

    resp =
      Couch.put("/#{db_name}/_design/update/_update/bump-counter/#{docid}",
        headers: ["X-Couch-Full-Commit": "true"]
      )

    newrev = resp.headers["X-Couch-Update-NewRev"]

    resp = Couch.get("/#{db_name}/#{docid}")
    assert resp.status_code == 200
    assert resp.body["counter"] == 2
    assert resp.body["_rev"] == newrev
  end

  @tag :with_db
  test "Server provides UUID when POSTing without an ID in the URL", context do
    db_name = context[:db_name]
    create_doc(db_name, @ddoc)
    resp = Couch.put("/#{db_name}/_design/update/_update/get-uuid/")
    assert resp.status_code == 200
    assert String.length(resp.body) == 32
  end

  @tag :with_db
  test "COUCHDB-1229 - allow slashes in doc ids for update handlers", context do
    db_name = context[:db_name]
    create_doc(db_name, @ddoc)

    create_doc(db_name, %{_id: "with/slash", counter: 1})

    resp = Couch.put("/#{db_name}/_design/update/_update/bump-counter/with/slash")
    assert resp.status_code == 201
    assert resp.body == "<h1>bumped it!</h1>"

    resp = Couch.get("/#{db_name}/with%2Fslash")
    assert resp.status_code == 200
    assert resp.body["counter"] == 2
  end

  @tag :with_db
  test "COUCHDB-648 - the code in the JSON response should be honored", context do
    db_name = context[:db_name]
    create_doc(db_name, @ddoc)

    {:ok, resp} = create_doc(db_name, @document)
    docid = resp.body["id"]

    Couch.put("/#{db_name}/_design/update/_update/bump-counter/#{docid}")
    Couch.put("/#{db_name}/_design/update/_update/bump-counter/#{docid}")

    resp = Couch.put("/#{db_name}/_design/update/_update/code-n-bump/#{docid}")
    assert resp.status_code == 302
    assert resp.body == "<h1>bumped it!</h1>"

    resp = Couch.get("/#{db_name}/#{docid}")
    assert resp.status_code == 200
    assert resp.body["counter"] == 3

    resp = Couch.put("/#{db_name}/_design/update/_update/resp-code/")
    assert resp.status_code == 302

    resp = Couch.put("/#{db_name}/_design/update/_update/resp-code-and-json/")
    assert resp.status_code == 302
    assert resp.body["ok"] == true
  end

  @tag :with_db
  test "base64 response", context do
    db_name = context[:db_name]
    create_doc(db_name, @ddoc)

    {:ok, resp} = create_doc(db_name, @document)
    docid = resp.body["id"]

    resp =
      Couch.put("/#{db_name}/_design/update/_update/binary/#{docid}",
        body: "rubbish"
      )

    assert resp.status_code == 201
    assert resp.body == "hello world!"
    assert String.contains?(resp.headers["Content-Type"], "application/octet-stream")
  end

  @tag :with_db
  test "Insert doc with empty id", context do
    db_name = context[:db_name]
    create_doc(db_name, @ddoc)

    resp = Couch.put("/#{db_name}/_design/update/_update/empty/foo")
    assert resp.status_code == 400
    assert resp.body["reason"] == "Document id must not be empty"
  end
end

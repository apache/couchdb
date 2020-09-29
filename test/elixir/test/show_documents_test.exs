defmodule ShowDocumentsTest do
  use CouchTestCase

  @moduletag kind: :single_node

  @ddoc %{
    _id: "_design/template",
    language: "javascript",
    shows: %{
      hello: """
       function(doc, req) {
        if (doc) {
          return "Hello World";
        } else {
          if(req.id) {
            return "New World";
          } else {
            return "Empty World";
          }
        }
      }
      """,
      "just-name": """
        function(doc, req) {
        if (doc) {
          return {
            body : "Just " + doc.name
          };
        } else {
          return {
            body : "No such doc",
            code : 404
          };
        }
      }
      """,
      json: """
       function(doc, req) {
        return {
          json : doc
        }
      }
      """,
      "req-info": """
        function(doc, req) {
        return {
          json : req
        }
      }
      """,
      "show-deleted": """
       function(doc, req) {
        if(doc) {
          return doc._id;
        } else {
          return "No doc " + req.id;
        }
      }
      """,
      "render-error": """
        function(doc, req) {
        return noSuchVariable;
      }
      """,
      empty: """
      function(doc, req) {
        return "";
      }
      """,
      fail: """
       function(doc, req) {
        return doc._id;
      }
      """,
      "no-set-etag": """
       function(doc, req) {
        return {
          headers : {
            "Etag" : "skipped"
          },
          "body" : "something"
        }
      }
      """,
      "list-api": """
       function(doc, req) {
        start({"X-Couch-Test-Header": "Yeah"});
        send("Hey");
      }
      """,
      "list-api-provides": """
       function(doc, req) {
        provides("text", function(){
            send("foo, ");
            send("bar, ");
            send("baz!");
        })
      }
      """,
      "list-api-provides-and-return": """
       function(doc, req) {
        provides("text", function(){
            send("4, ");
            send("5, ");
            send("6, ");
            return "7!";
        })
        send("1, ");
        send("2, ");
        return "3, ";
      }
      """,
      "list-api-mix": """
       function(doc, req) {
        start({"X-Couch-Test-Header": "Yeah"});
        send("Hey ");
        return "Dude";
      }
      """,
      "list-api-mix-with-header": """
       function(doc, req) {
        start({"X-Couch-Test-Header": "Yeah"});
        send("Hey ");
        return {
          headers: {
            "X-Couch-Test-Header-Awesome": "Oh Yeah!"
          },
          body: "Dude"
        };
      }
      """,
      "accept-switch": """
       function(doc, req) {
        if (req.headers["Accept"].match(/image/)) {
          return {
            // a 16x16 px version of the CouchDB logo
            "base64" :
      ["iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAsV",
      "BMVEUAAAD////////////////////////5ur3rEBn////////////////wDBL/",
      "AADuBAe9EB3IEBz/7+//X1/qBQn2AgP/f3/ilpzsDxfpChDtDhXeCA76AQH/v7",
      "/84eLyWV/uc3bJPEf/Dw/uw8bRWmP1h4zxSlD6YGHuQ0f6g4XyQkXvCA36MDH6",
      "wMH/z8/yAwX64ODeh47BHiv/Ly/20dLQLTj98PDXWmP/Pz//39/wGyJ7Iy9JAA",
      "AADHRSTlMAbw8vf08/bz+Pv19jK/W3AAAAg0lEQVR4Xp3LRQ4DQRBD0QqTm4Y5",
      "zMxw/4OleiJlHeUtv2X6RbNO1Uqj9g0RMCuQO0vBIg4vMFeOpCWIWmDOw82fZx",
      "vaND1c8OG4vrdOqD8YwgpDYDxRgkSm5rwu0nQVBJuMg++pLXZyr5jnc1BaH4GT",
      "LvEliY253nA3pVhQqdPt0f/erJkMGMB8xucAAAAASUVORK5CYII="].join(''),
            headers : {
              "Content-Type" : "image/png",
              "Vary" : "Accept" // we set this for proxy caches
            }
          };
        } else {
          return {
            "body" : "accepting text requests",
            headers : {
              "Content-Type" : "text/html",
              "Vary" : "Accept"
            }
          };
        }
      }
      """,
      provides: """
       function(doc, req) {
        registerType("foo", "application/foo","application/x-foo");

        provides("html", function() {
          return "Ha ha, you said \\"" + doc.word + "\\".";
        });

        provides("foo", function() {
          return "foofoo";
        });
      }
      """,
      withSlash: """
       function(doc, req) {
        return { json: doc }
      }
      """,
      secObj: """
        function(doc, req) {
        return { json: req.secObj };
      }
      """
    }
  }

  setup_all do
    db_name = random_db_name()
    {:ok, _} = create_db(db_name)
    on_exit(fn -> delete_db(db_name) end)

    {:ok, _} = create_doc(db_name, @ddoc)

    create_doc(db_name, %{_id: "test-doc-id", word: "plankton", name: "Rusty"})

    {:ok, [db_name: db_name]}
  end

  test "show error", context do
    db_name = context[:db_name]

    resp = Couch.get("/#{db_name}/_design/template/_show/")
    assert resp.status_code == 404
    assert resp.body["reason"] == "Invalid path."
  end

  test "show with existing doc", context do
    db_name = context[:db_name]

    resp = Rawresp.get("/#{db_name}/_design/template/_show/hello/test-doc-id")
    assert resp.body == "Hello World"
    assert String.match?(resp.headers["Content-Type"], ~r/charset=utf-8/)

    # Fix for COUCHDB-379
    assert String.match?(resp.headers["Server"], ~r/^CouchDB/)
  end

  test "show without docid", context do
    db_name = context[:db_name]
    resp = Rawresp.get("/#{db_name}/_design/template/_show/hello")
    assert resp.body == "Empty World"

    resp = Rawresp.get("/#{db_name}/_design/template/_show/empty")
    assert resp.body == ""
  end

  test "show fail with non-existing docid", context do
    db_name = context[:db_name]
    resp = Couch.get("/#{db_name}/_design/template/_show/fail/nonExistingDoc")
    assert resp.status_code == 404
    assert resp.body["error"] == "not_found"
  end

  test "show with doc", context do
    db_name = context[:db_name]
    resp = Rawresp.get("/#{db_name}/_design/template/_show/just-name/test-doc-id")
    assert resp.body == "Just Rusty"
  end

  test "show with missing doc", context do
    db_name = context[:db_name]
    resp = Rawresp.get("/#{db_name}/_design/template/_show/just-name/missingdoc")
    assert resp.status_code == 404
    assert resp.body == "No such doc"
  end

  test "missing design doc", context do
    db_name = context[:db_name]
    resp = Couch.get("/#{db_name}/_design/missingddoc/_show/just-name/test-doc-id")
    assert resp.status_code == 404
    assert resp.body["error"] == "not_found"
  end

  test "show query parameters", context do
    db_name = context[:db_name]

    resp =
      Couch.get("/#{db_name}/_design/template/_show/req-info/test-doc-id?foo=bar",
        headers: [Accept: "text/html;text/plain;*/*", "X-Foo": "bar"]
      )

    assert resp.body["headers"]["X-Foo"] == "bar"
    assert resp.body["query"] == %{"foo" => "bar"}
    assert resp.body["method"] == "GET"
    assert Enum.at(resp.body["path"], 5) == "test-doc-id"
    assert resp.body["info"]["db_name"] == db_name
  end

  test "accept header switching - different mime has different etag", context do
    db_name = context[:db_name]

    resp =
      Couch.get("/#{db_name}/_design/template/_show/accept-switch/test-doc-id",
        headers: [Accept: "text/html;text/plain;*/*"]
      )

    assert String.match?(resp.headers["Content-Type"], ~r/text\/html/)
    assert resp.headers["Vary"] == "Accept"

    etag = resp.headers["etag"]

    resp =
      Rawresp.get("/#{db_name}/_design/template/_show/accept-switch/test-doc-id",
        headers: [Accept: "image/png;*/*"]
      )

    assert String.match?(resp.body, ~r/PNG/)
    assert resp.headers["Content-Type"] == "image/png"

    etag2 = resp.headers["etag"]

    assert etag != etag2
  end

  test "show with doc - etags", context do
    db_name = context[:db_name]

    doc = %{"_id" => "test-doc-id2", word: "plankton", name: "Rusty"}
    doc = save(db_name, doc)

    resp = Couch.get("/#{db_name}/_design/template/_show/just-name/test-doc-id2")

    etag = resp.headers["etag"]

    resp =
      Couch.get("/#{db_name}/_design/template/_show/just-name/test-doc-id2",
        headers: ["if-none-match": etag]
      )

    assert resp.status_code == 304

    doc = Map.put(doc, "name", "Crusty")
    save(db_name, doc)

    resp =
      Couch.get("/#{db_name}/_design/template/_show/just-name/test-doc-id2",
        headers: ["if-none-match": etag]
      )

    assert resp.status_code == 200
  end

  test "JS can't set etag", context do
    db_name = context[:db_name]

    resp = Couch.get("/#{db_name}/_design/template/_show/no-set-etag/test-doc-id")
    assert resp.headers["etag"] != "skipped"
  end

  test "the provides mime matcher", context do
    db_name = context[:db_name]

    resp =
      Rawresp.get("/#{db_name}/_design/template/_show/provides/test-doc-id",
        headers: [Accept: "text/html,application/atom+xml; q=0.9"]
      )

    assert String.match?(resp.headers["Content-Type"], ~r/text\/html/)
    assert String.match?(resp.headers["Content-Type"], ~r/charset=utf-8/)
    assert resp.body == "Ha ha, you said \"plankton\"."
  end

  test "registering types works", context do
    db_name = context[:db_name]

    resp =
      Rawresp.get("/#{db_name}/_design/template/_show/provides/test-doc-id",
        headers: [Accept: "application/x-foo"]
      )

    assert resp.headers["Content-Type"] == "application/x-foo"
    assert String.match?(resp.body, ~r/foofoo/)
  end

  test "the provides mime matcher without a match", context do
    db_name = context[:db_name]

    resp =
      Couch.get("/#{db_name}/_design/template/_show/provides/test-doc-id",
        headers: [Accept: "text/monkeys"]
      )

    assert resp.body["error"] == "not_acceptable"
  end

  test "id with slash", context do
    db_name = context[:db_name]

    doc3 = %{"_id" => "a/b/c", "a" => 1}
    save(db_name, doc3)
    resp = Couch.get("/#{db_name}/_design/template/_show/withSlash/a/b/c")
    assert resp.status_code == 200
  end

  test "show with non-existing docid", context do
    db_name = context[:db_name]

    resp = Rawresp.get("/#{db_name}/_design/template/_show/hello/nonExistingDoc")
    assert resp.body == "New World"
  end

  test "list() compatible API", context do
    db_name = context[:db_name]

    resp = Rawresp.get("/#{db_name}/_design/template/_show/list-api/foo")
    assert resp.body == "Hey"
    assert resp.headers["X-Couch-Test-Header"] == "Yeah"
  end

  test "list() compatible API with provides function", context do
    db_name = context[:db_name]

    resp =
      Rawresp.get("/#{db_name}/_design/template/_show/list-api-provides/foo?format=text")

    assert resp.body == "foo, bar, baz!"
  end

  test "should keep next result order: chunks + return value + provided chunks + provided return value",
       context do
    db_name = context[:db_name]

    resp =
      Rawresp.get(
        "/#{db_name}/_design/template/_show/list-api-provides-and-return/foo?format=text"
      )

    assert resp.body == "1, 2, 3, 4, 5, 6, 7!"

    resp = Rawresp.get("/#{db_name}/_design/template/_show/list-api-mix/foo")
    assert resp.body == "Hey Dude"
    assert resp.headers["X-Couch-Test-Header"] == "Yeah"

    resp = Rawresp.get("/#{db_name}/_design/template/_show/list-api-mix-with-header/foo")
    assert resp.body == "Hey Dude"
    assert resp.headers["X-Couch-Test-Header"] == "Yeah"
    assert resp.headers["X-Couch-Test-Header-Awesome"] == "Oh Yeah!"
  end

  test "deleted docs", context do
    db_name = context[:db_name]

    doc = save(db_name, %{"_id" => "testdoc", "foo" => 1})

    resp = Rawresp.get("/#{db_name}/_design/template/_show/show-deleted/testdoc")
    assert resp.body == "testdoc"

    Couch.delete("/#{db_name}/testdoc?rev=#{doc["_rev"]}")
    resp = Rawresp.get("/#{db_name}/_design/template/_show/show-deleted/testdoc")
    assert resp.body == "No doc testdoc"
  end

  @tag :with_db
  test "security object", context do
    db_name = context[:db_name]
    {:ok, _} = create_doc(db_name, @ddoc)
    {:ok, _} = create_doc(db_name, %{_id: "testdoc", foo: 1})

    Couch.put("/#{db_name}/_security", body: %{foo: true})

    retry_until(fn ->
      resp = Couch.get("/#{db_name}/_design/template/_show/secObj")
      assert resp.body["foo"]
    end)
  end
end

defmodule RewriteTest do
  use CouchTestCase

  @moduletag :js_engine

  @moduledoc """
  Test CouchDB rewrites
  This is a port of the rewrite.js suite
  """

  Enum.each(
    ["test_rewrite_suite_db", "test_rewrite_suite_db%2Fwith_slashes"],
    fn db_name ->
      @tag with_random_db: db_name
      @tag config: [
             {"httpd", "authentication_handlers",
              "{couch_httpd_auth, special_test_authentication_handler}"},
             {"chttpd", "WWW-Authenticate", "X-Couch-Test-Auth"}
           ]
      test "Test basic rewrites on #{db_name}", context do
        db_name = context[:db_name]

        ddoc = ~S"""
        {
          "_id": "_design/test",
          "language": "javascript",
          "_attachments": {
            "foo.txt": {
              "content_type":"text/plain",
              "data": "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
            }
          },
          "rewrites": [
            {
              "from": "foo",
              "to": "foo.txt"
            },
            {
              "from": "foo2",
              "to": "foo.txt",
              "method": "GET"
            },
            {
              "from": "hello/:id",
              "to": "_update/hello/:id",
              "method": "PUT"
            },
            {
              "from": "/welcome",
              "to": "_show/welcome"
            },
            {
              "from": "/welcome/:name",
              "to": "_show/welcome",
              "query": {
                "name": ":name"
              }
            },
            {
              "from": "/welcome2",
              "to": "_show/welcome",
              "query": {
                "name": "user"
              }
            },
            {
              "from": "/welcome3/:name",
              "to": "_update/welcome2/:name",
              "method": "PUT"
            },
            {
              "from": "/welcome3/:name",
              "to": "_show/welcome2/:name",
              "method": "GET"
            },
            {
              "from": "/welcome4/*",
              "to" : "_show/welcome3",
              "query": {
                "name": "*"
              }
            },
            {
              "from": "/welcome5/*",
              "to" : "_show/*",
              "query": {
                "name": "*"
              }
            },
            {
              "from": "basicView",
              "to": "_view/basicView"
            },
            {
              "from": "simpleForm/basicView",
              "to": "_list/simpleForm/basicView"
            },
            {
              "from": "simpleForm/basicViewFixed",
              "to": "_list/simpleForm/basicView",
              "query": {
                "startkey": 3,
                "endkey": 8
              }
            },
            {
              "from": "simpleForm/basicViewPath/:start/:end",
              "to": "_list/simpleForm/basicView",
              "query": {
                "startkey": ":start",
                "endkey": ":end"
              },
              "formats": {
                "start": "int",
                "end": "int"
              }
            },
            {
              "from": "simpleForm/complexView",
              "to": "_list/simpleForm/complexView",
              "query": {
                "key": [1, 2]
              }
            },
            {
              "from": "simpleForm/complexView2",
              "to": "_list/simpleForm/complexView",
              "query": {
                "key": ["test", {}]
              }
            },
            {
              "from": "simpleForm/complexView3",
              "to": "_list/simpleForm/complexView",
              "query": {
                "key": ["test", ["test", "essai"]]
              }
            },
            {
              "from": "simpleForm/complexView4",
              "to": "_list/simpleForm/complexView2",
              "query": {
                "key": {"c": 1}
              }
            },
            {
              "from": "simpleForm/complexView5/:a/:b",
              "to": "_list/simpleForm/complexView3",
              "query": {
                "key": [":a", ":b"]
              }
            },
            {
              "from": "simpleForm/complexView6",
              "to": "_list/simpleForm/complexView3",
              "query": {
                "key": [":a", ":b"]
              }
            },
            {
              "from": "simpleForm/complexView7/:a/:b",
              "to": "_view/complexView3",
              "query": {
                "key": [":a", ":b"],
                "include_docs": ":doc"
              },
              "format": {
                "doc": "bool"
              }

            },
            {
              "from": "/",
              "to": "_view/basicView"
            },
            {
              "from": "/db/*",
              "to": "../../*"
            }
          ],
          "lists": {
            "simpleForm": "function(head, req) {
              log(\"simpleForm\");
              send(\"<ul>\");
              var row, row_number = 0, prevKey, firstKey = null;
              while (row = getRow()) {
                row_number += 1;
                if (!firstKey) firstKey = row.key;
                prevKey = row.key;
                send(\"\\n<li>Key: \"+row.key
                     +\" Value: \"+row.value
                     +\" LineNo: \"+row_number+\"</li>\");
              }
              return \"</ul><p>FirstKey: \"+ firstKey + \" LastKey: \"+ prevKey+\"</p>\";
            }"
          },
          "shows": {
            "welcome": "(function(doc,req) {
              return \"Welcome \" + req.query[\"name\"];
            })",
            "welcome2": "(function(doc, req) {
              return \"Welcome \" + doc.name;
            })",
            "welcome3": "(function(doc,req) {
              return \"Welcome \" + req.query[\"name\"];
            })"
          },
          "updates": {
            "hello" : "(function(doc, req) {
              if (!doc) {
                if (req.id) {
                  return [{
                    _id : req.id
                  }, \"New World\"]
                }
                return [null, \"Empty World\"];
              }
              doc.world = \"hello\";
              doc.edited_by = req.userCtx;
              return [doc, \"hello doc\"];
            })",
            "welcome2": "(function(doc, req) {
              if (!doc) {
                if (req.id) {
                  return [{
                    _id: req.id,
                    name: req.id
                  }, \"New World\"]
                }
                return [null, \"Empty World\"];
              }
              return [doc, \"hello doc\"];
            })"
          },
          "views" : {
            "basicView" : {
              "map" : "(function(doc) {
                if (doc.integer) {
                  emit(doc.integer, doc.string);
                }

              })"
            },
            "complexView": {
              "map": "(function(doc) {
                if (doc.type == \"complex\") {
                  emit([doc.a, doc.b], doc.string);
                }
              })"
            },
            "complexView2": {
              "map": "(function(doc) {
                if (doc.type == \"complex\") {
                  emit(doc.a, doc.string);
                }
              })"
            },
            "complexView3": {
              "map": "(function(doc) {
                if (doc.type == \"complex\") {
                  emit(doc.b, doc.string);
                }
              })"
            }
          }
        }
        """

        ddoc = String.replace(ddoc, ~r/[\r\n]+/, "")

        docs1 = make_docs(0..9)

        docs2 = [
          %{"a" => 1, "b" => 1, "string" => "doc 1", "type" => "complex"},
          %{"a" => 1, "b" => 2, "string" => "doc 2", "type" => "complex"},
          %{"a" => "test", "b" => %{}, "string" => "doc 3", "type" => "complex"},
          %{
            "a" => "test",
            "b" => ["test", "essai"],
            "string" => "doc 4",
            "type" => "complex"
          },
          %{"a" => %{"c" => 1}, "b" => "", "string" => "doc 5", "type" => "complex"}
        ]

        assert Couch.put("/#{db_name}/_design/test", body: ddoc).body["ok"]

        assert Couch.post(
                 "/#{db_name}/_bulk_docs",
                 body: %{:docs => docs1},
                 query: %{w: 3}
               ).status_code in [201, 202]

        assert Couch.post(
                 "/#{db_name}/_bulk_docs",
                 body: %{:docs => docs2},
                 query: %{w: 3}
               ).status_code in [201, 202]

        # Test simple rewriting
        resp = Couch.get("/#{db_name}/_design/test/_rewrite/foo")
        assert resp.body == "This is a base64 encoded text"
        assert resp.headers["Content-Type"] == "text/plain"

        resp = Couch.get("/#{db_name}/_design/test/_rewrite/foo2")
        assert resp.body == "This is a base64 encoded text"
        assert resp.headers["Content-Type"] == "text/plain"

        # Test POST, hello update world
        resp =
          Couch.post("/#{db_name}", body: %{"word" => "plankton", "name" => "Rusty"}).body

        assert resp["ok"]
        doc_id = resp["id"]
        assert doc_id

        resp = Couch.put("/#{db_name}/_design/test/_rewrite/hello/#{doc_id}")
        assert resp.status_code in [201, 202]
        assert resp.body == "hello doc"
        assert String.match?(resp.headers["Content-Type"], ~r/charset=utf-8/)

        assert Couch.get("/#{db_name}/#{doc_id}").body["world"] == "hello"

        resp = Couch.get("/#{db_name}/_design/test/_rewrite/welcome?name=user")
        assert resp.body == "Welcome user"

        resp = Couch.get("/#{db_name}/_design/test/_rewrite/welcome/user")
        assert resp.body == "Welcome user"

        resp = Couch.get("/#{db_name}/_design/test/_rewrite/welcome2")
        assert resp.body == "Welcome user"

        resp = Couch.put("/#{db_name}/_design/test/_rewrite/welcome3/test")
        assert resp.status_code in [201, 202]
        assert resp.body == "New World"
        assert String.match?(resp.headers["Content-Type"], ~r/charset=utf-8/)

        resp = Couch.get("/#{db_name}/_design/test/_rewrite/welcome3/test")
        assert resp.body == "Welcome test"

        # TODO: port the two "bugged" tests from rewrite.js

        resp = Couch.get("/#{db_name}/_design/test/_rewrite/basicView")
        assert resp.status_code == 200
        assert resp.body["total_rows"] == 9

        resp = Couch.get("/#{db_name}/_design/test/_rewrite")
        assert resp.status_code == 200
        assert resp.body["total_rows"] == 9

        resp =
          Rawresp.get(
            "/#{db_name}/_design/test/_rewrite/simpleForm/basicView?startkey=3&endkey=8"
          )

        assert resp.status_code == 200
        assert not String.match?(resp.body, ~r/Key: 1/)
        assert String.match?(resp.body, ~r/FirstKey: 3/)
        assert String.match?(resp.body, ~r/LastKey: 8/)

        resp = Rawresp.get("/#{db_name}/_design/test/_rewrite/simpleForm/basicViewFixed")
        assert resp.status_code == 200
        assert not String.match?(resp.body, ~r/Key: 1/)
        assert String.match?(resp.body, ~r/FirstKey: 3/)
        assert String.match?(resp.body, ~r/LastKey: 8/)

        resp =
          Rawresp.get(
            "/#{db_name}/_design/test/_rewrite/simpleForm/basicViewFixed?startkey=4"
          )

        assert resp.status_code == 200
        assert not String.match?(resp.body, ~r/Key: 1/)
        assert String.match?(resp.body, ~r/FirstKey: 3/)
        assert String.match?(resp.body, ~r/LastKey: 8/)

        resp =
          Rawresp.get("/#{db_name}/_design/test/_rewrite/simpleForm/basicViewPath/3/8")

        assert resp.status_code == 200
        assert not String.match?(resp.body, ~r/Key: 1/)
        assert String.match?(resp.body, ~r/FirstKey: 3/)
        assert String.match?(resp.body, ~r/LastKey: 8/)

        resp = Rawresp.get("/#{db_name}/_design/test/_rewrite/simpleForm/complexView")
        assert resp.status_code == 200
        assert String.match?(resp.body, ~r/FirstKey: [1, 2]/)

        resp = Rawresp.get("/#{db_name}/_design/test/_rewrite/simpleForm/complexView2")
        assert resp.status_code == 200
        assert String.match?(resp.body, ~r/Value: doc 3/)

        resp = Rawresp.get("/#{db_name}/_design/test/_rewrite/simpleForm/complexView3")
        assert resp.status_code == 200
        assert String.match?(resp.body, ~r/Value: doc 4/)

        resp = Rawresp.get("/#{db_name}/_design/test/_rewrite/simpleForm/complexView4")
        assert resp.status_code == 200
        assert String.match?(resp.body, ~r/Value: doc 5/)

        resp =
          Rawresp.get(
            "/#{db_name}/_design/test/_rewrite/simpleForm/complexView5/test/essai"
          )

        assert resp.status_code == 200
        assert String.match?(resp.body, ~r/Value: doc 4/)

        resp =
          Rawresp.get(
            "/#{db_name}/_design/test/_rewrite/simpleForm/complexView6?a=test&b=essai"
          )

        assert resp.status_code == 200
        assert String.match?(resp.body, ~r/Value: doc 4/)

        resp =
          Rawresp.get(
            "/#{db_name}/_design/test/_rewrite/simpleForm/complexView7/test/essai?doc=true"
          )

        assert resp.status_code == 200
        result = resp.body |> IO.iodata_to_binary() |> :jiffy.decode([:return_maps])
        first_row = Enum.at(result["rows"], 0)
        assert Map.has_key?(first_row, "doc")

        # COUCHDB-2031 - path normalization versus qs params
        resp = Rawresp.get("/#{db_name}/_design/test/_rewrite/db/_design/test?meta=true")
        assert resp.status_code == 200
        result = resp.body |> IO.iodata_to_binary() |> :jiffy.decode([:return_maps])
        assert result["_id"] == "_design/test"
        assert Map.has_key?(result, "_revs_info")

        ddoc2 = %{
          _id: "_design/test2",
          rewrites: [
            %{
              from: "uuids",
              to: "../../../_uuids"
            }
          ]
        }

        create_doc(db_name, ddoc2)
        resp = Couch.get("/#{db_name}/_design/test2/_rewrite/uuids")
        assert resp.status_code == 500
        assert resp.body["error"] == "insecure_rewrite_rule"
      end

      @tag with_random_db: db_name
      @tag config: [
             {"chttpd", "secure_rewrites", "false"}
           ]
      test "path relative to server on #{db_name}", context do
        db_name = context[:db_name]

        ddoc = %{
          _id: "_design/test2",
          rewrites: [
            %{
              from: "uuids",
              to: "../../../_uuids"
            }
          ]
        }

        create_doc(db_name, ddoc)

        resp = Couch.get("/#{db_name}/_design/test2/_rewrite/uuids")
        assert resp.status_code == 200
        assert length(resp.body["uuids"]) == 1
      end

      @tag with_random_db: db_name
      @tag config: [
             {"chttpd", "rewrite_limit", "2"}
           ]
      test "loop detection on #{db_name}", context do
        db_name = context[:db_name]

        ddoc_loop = %{
          _id: "_design/loop",
          rewrites: [%{from: "loop", to: "_rewrite/loop"}]
        }

        create_doc(db_name, ddoc_loop)

        resp = Couch.get("/#{db_name}/_design/loop/_rewrite/loop")
        assert resp.status_code == 400
      end

      @tag with_random_db: db_name
      @tag config: [
             {"chttpd", "rewrite_limit", "2"},
             {"chttpd", "secure_rewrites", "false"}
           ]
      test "serial execution is not spuriously counted as loop on #{db_name}", context do
        db_name = context[:db_name]

        ddoc = %{
          _id: "_design/test",
          language: "javascript",
          _attachments: %{
            "foo.txt": %{
              content_type: "text/plain",
              data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
            }
          },
          rewrites: [
            %{
              from: "foo",
              to: "foo.txt"
            }
          ]
        }

        create_doc(db_name, ddoc)

        for _i <- 0..4 do
          resp = Couch.get("/#{db_name}/_design/test/_rewrite/foo")
          assert resp.status_code == 200
        end
      end
    end
  )
end

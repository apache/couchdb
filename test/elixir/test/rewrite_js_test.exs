defmodule RewriteJSTest do
  use CouchTestCase

  @moduletag :js_engine
  @moduletag kind: :single_node

  @moduledoc """
  Test CouchDB rewrites JS
  This is a port of the rewrite_js.js suite
  """

  @ddoc %{
    _id: "_design/test",
    language: "javascript",
    _attachments: %{
      "foo.txt": %{
        content_type: "text/plain",
        data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
      }
    },
    rewrites: """
      function(req) {
      prefix = req.path[4];
      if (prefix === 'foo') {
          return 'foo.txt';
      }
      if (prefix === 'foo2') {
          return {path: 'foo.txt', method: 'GET'};
      }
      if (prefix === 'hello') {
          if (req.method != 'PUT') {
              return
          }
          id = req.path[5];
          return {path: '_update/hello/' + id};
      }
      if (prefix === 'welcome') {
          if (req.path.length == 6){
              name = req.path[5];
              return {path: '_show/welcome', query: {'name': name}};
          }
          return '_show/welcome';
      }
      if (prefix === 'welcome2') {
          return {path: '_show/welcome', query: {'name': 'user'}};
      }
      if (prefix === 'welcome3') {
          name = req.path[5];
          if (req.method == 'PUT') {
              path = '_update/welcome2/' + name;
          } else if (req.method == 'GET') {
              path = '_show/welcome2/' + name;
          } else {
              return;
          }
          return path;
      }
      if (prefix === 'welcome4') {
          return {path: '_show/welcome3',  query: {name: req.path[5]}};
      }
      if (prefix === 'welcome5') {
          rest = req.path.slice(5).join('/');
          return {path: '_show/' + rest,  query: {name: rest}};
      }
      if (prefix === 'basicView') {
          rest = req.path.slice(5).join('/');
          return {path: '_view/basicView'};
      }
      if (req.path.slice(4).join('/') === 'simpleForm/basicView') {
          return {path: '_list/simpleForm/basicView'};
      }
      if (req.path.slice(4).join('/') === 'simpleForm/basicViewFixed') {
          return {path: '_list/simpleForm/basicView',
                  query: {startkey: '"3"', endkey: '"8"'}};
      }
      if (req.path.slice(4).join('/') === 'simpleForm/complexView') {
          return {path: '_list/simpleForm/complexView',
                  query: {key: JSON.stringify([1,2])}};
      }
      if (req.path.slice(4).join('/') === 'simpleForm/complexView2') {
          return {path: '_list/simpleForm/complexView',
                  query: {key: JSON.stringify(['test', {}])}};
      }
      if (req.path.slice(4).join('/') === 'simpleForm/complexView3') {
          return {path: '_list/simpleForm/complexView',
                  query: {key: JSON.stringify(['test', ['test', 'essai']])}};
      }
      if (req.path.slice(4).join('/') === 'simpleForm/complexView4') {
          return {path: '_list/simpleForm/complexView2',
                  query: {key: JSON.stringify({"c": 1})}};
      }
      if (req.path.slice(4).join('/') === 'simpleForm/sendBody1') {
          return {path:   '_list/simpleForm/complexView2',
                  method: 'POST',
                  query:  {limit: '1'},
                  headers:{'Content-type':'application/json'},
                  body:  JSON.stringify( {keys: [{"c": 1}]} )};
      }
      if (req.path.slice(4).join('/') === '/') {
          return {path: '_view/basicView'};
      }
      if (prefix === 'db') {
          return {path: '../../' + req.path.slice(5).join('/')};
      }
    }
    """,
    lists: %{
      simpleForm: """
      function(head, req) {
        send('<ul>');
        var row, row_number = 0, prevKey, firstKey = null;
        while (row = getRow()) {
          row_number += 1;
          if (!firstKey) firstKey = row.key;
          prevKey = row.key;
          send('\\n<li>Key: '+row.key
               +' Value: '+row.value
               +' LineNo: '+row_number+'</li>');
        }
        return '</ul><p>FirstKey: '+ firstKey + ' LastKey: '+ prevKey+'</p>';
      }
      """
    },
    shows: %{
      welcome: """
       function(doc,req) {
        return "Welcome " + req.query["name"];
      }
      """,
      welcome2: """
       function(doc, req) {
        return "Welcome " + doc.name;
      }
      """,
      welcome3: """
        function(doc,req) {
        return "Welcome " + req.query["name"];
      }
      """
    },
    updates: %{
      hello: """
       function(doc, req) {
        if (!doc) {
          if (req.id) {
            return [{
              _id : req.id
            }, "New World"]
          }
          return [null, "Empty World"];
        }
        doc.world = "hello";
        doc.edited_by = req.userCtx;
        return [doc, "hello doc"];
      }
      """,
      welcome2: """
       function(doc, req) {
        if (!doc) {
          if (req.id) {
            return [{
              _id: req.id,
              name: req.id
            }, "New World"]
          }
          return [null, "Empty World"];
        }
        return [doc, "hello doc"];
      }
      """
    },
    views: %{
      basicView: %{
        map: """
         function(doc) {
          if (doc.integer) {
            emit(doc.integer, doc.string);
          }
        }
        """
      },
      complexView: %{
        map: """
         function(doc) {
          if (doc.type == "complex") {
            emit([doc.a, doc.b], doc.string);
          }
        }
        """
      },
      complexView2: %{
        map: """
         function(doc) {
          if (doc.type == "complex") {
            emit(doc.a, doc.string);
          }
        }
        """
      },
      complexView3: %{
        map: """
         function(doc) {
          if (doc.type == "complex") {
            emit(doc.b, doc.string);
          }
        }
        """
      }
    }
  }

  Enum.each(
    ["test_rewrite_suite_db", "test_rewrite_suite_db%2Fwith_slashes"],
    fn db_name ->
      @tag with_random_db: db_name
      test "Test basic js rewrites on #{db_name}", context do
        db_name = context[:db_name]

        create_doc(db_name, @ddoc)

        docs1 = make_docs(0..9)
        bulk_save(db_name, docs1)

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

        bulk_save(db_name, docs2)

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

        resp = Couch.get("/#{db_name}/_design/test/_rewrite/welcome4/user")
        assert resp.body == "Welcome user"

        resp = Couch.get("/#{db_name}/_design/test/_rewrite/welcome5/welcome3")
        assert resp.body == "Welcome welcome3"

        resp = Couch.get("/#{db_name}/_design/test/_rewrite/basicView")
        assert resp.status_code == 200
        assert resp.body["total_rows"] == 9

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

        # COUCHDB-1612 - send body rewriting get to post
        resp = Rawresp.get("/#{db_name}/_design/test/_rewrite/simpleForm/sendBody1")
        assert resp.status_code == 200
        assert String.match?(resp.body, ~r/Value: doc 5 LineNo: 1/)

        resp = Couch.get("/#{db_name}/_design/test/_rewrite/db/_design/test?meta=true")
        assert resp.status_code == 200
        assert resp.body["_id"] == "_design/test"
        assert Map.has_key?(resp.body, "_revs_info")
      end

      @tag with_random_db: db_name
      test "early response on #{db_name}", context do
        db_name = context[:db_name]

        ddoc = %{
          _id: "_design/response",
          rewrites: """
           function(req){
            status = parseInt(req.query.status);
            return {code: status,
                    body: JSON.stringify({"status": status}),
                    headers: {'x-foo': 'bar', 'Content-Type': 'application/json'}};
          }
          """
        }

        create_doc(db_name, ddoc)

        resp = Couch.get("/#{db_name}/_design/response/_rewrite?status=200")
        assert resp.status_code == 200
        assert resp.headers["x-foo"] == "bar"
        assert resp.body["status"] == 200

        resp = Couch.get("/#{db_name}/_design/response/_rewrite?status=451")
        assert resp.status_code == 451
        assert resp.headers["Content-Type"] == "application/json"

        resp = Couch.get("/#{db_name}/_design/response/_rewrite?status=500")
        assert resp.status_code == 500
      end

      @tag with_random_db: db_name
      test "path relative to server on #{db_name}", context do
        db_name = context[:db_name]

        ddoc = %{
          _id: "_design/relative",
          rewrites: """
           function(req){
            return '../../../_uuids'
          }
          """
        }

        create_doc(db_name, ddoc)
        resp = Couch.get("/#{db_name}/_design/relative/_rewrite/uuids")
        assert resp.status_code == 200
        assert length(resp.body["uuids"]) == 1
      end

      @tag with_random_db: db_name
      test "loop on #{db_name}", context do
        db_name = context[:db_name]

        ddoc_loop = %{
          _id: "_design/loop",
          rewrites: """
          function(req) {
            return '_rewrite/loop';
          }
          """
        }

        create_doc(db_name, ddoc_loop)
        resp = Couch.get("/#{db_name}/_design/loop/_rewrite/loop")
        assert resp.status_code == 400
      end

      @tag with_random_db: db_name
      test "requests with body preserve the query string rewrite on #{db_name}",
           context do
        db_name = context[:db_name]

        ddoc_qs = %{
          _id: "_design/qs",
          rewrites:
            "function (r) { return {path: '../../_changes', query: {'filter': '_doc_ids'}};};"
        }

        create_doc(db_name, ddoc_qs)
        create_doc(db_name, %{_id: "qs1"})
        create_doc(db_name, %{_id: "qs2"})

        resp =
          Couch.post("/#{db_name}/_design/qs/_rewrite",
            body: %{doc_ids: ["qs2"]}
          )

        assert resp.status_code == 200
        assert length(resp.body["results"]) == 1
        assert Enum.at(resp.body["results"], 0)["id"] == "qs2"
      end
    end
  )
end

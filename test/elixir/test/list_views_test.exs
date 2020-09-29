defmodule ListViewsTest do
  use CouchTestCase

  @moduletag kind: :single_node

  @ddoc %{
    _id: "_design/lists",
    language: "javascript",
    views: %{
      basicView: %{
        map: """
          function(doc) {
          emit(doc.integer, doc.string);
        }
        """
      },
      withReduce: %{
        map: """
         function(doc) {
          emit(doc.integer, doc.string);
        }
        """,
        reduce: """
          function(keys, values, rereduce) {
          if (rereduce) {
            return sum(values);
          } else {
            return values.length;
          }
        }
        """
      }
    },
    lists: %{
      basicBasic: """
       function(head, req) {
        send("head");
        var row;
        while(row = getRow()) {
          send(row.key);
        };
        return "tail";
      }
      """,
      basicJSON: """
        function(head, req) {
        start({"headers":{"Content-Type" : "application/json"}});
        send('{"head":'+toJSON(head)+', ');
        send('"req":'+toJSON(req)+', ');
        send('"rows":[');
        var row, sep = '';
        while (row = getRow()) {
          send(sep + toJSON(row));
          sep = ', ';
        }
        return "]}";
      }
      """,
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
      """,
      acceptSwitch: """
        function(head, req) {
        // respondWith takes care of setting the proper headers
        provides("html", function() {
          send("HTML <ul>");

          var row, num = 0;
          while (row = getRow()) {
            num ++;
            send('\\n<li>Key: '
              +row.key+' Value: '+row.value
              +' LineNo: '+num+'</li>');
          }

          // tail
          return '</ul>';
        });
      }
      """,
      qsParams: """
        function(head, req) {
        return toJSON(req.query) + "\\n";
      }
      """,
      stopIter: """
        function(req) {
        send("head");
        var row, row_number = 0;
        while(row = getRow()) {
          if(row_number > 2) break;
          send(" " + row_number);
          row_number += 1;
        };
        return " tail";
      }
      """,
      stopIter2: """
        function(head, req) {
        provides("html", function() {
          send("head");
          var row, row_number = 0;
          while(row = getRow()) {
            if(row_number > 2) break;
            send(" " + row_number);
            row_number += 1;
          };
          return " tail";
        });
      }
      """,
      tooManyGetRows: """
       function() {
        send("head");
        var row;
        while(row = getRow()) {
          send(row.key);
        };
        getRow();
        getRow();
        getRow();
        row = getRow();
        return "after row: "+toJSON(row);
      }
      """,
      emptyList: """
        function() {
        return " ";
      }
      """,
      rowError: """
        function(head, req) {
        send("head");
        var row = getRow();
        send(fooBarBam); // intentional error
        return "tail";
      }
      """,
      docReference: """
        function(head, req) {
        send("head");
        var row = getRow();
        send(row.doc.integer);
        return "tail";
      }
      """,
      secObj: """
        function(head, req) {
        return toJSON(req.secObj);
      }
      """,
      setHeaderAfterGotRow: """
        function(head, req) {
        getRow();
        start({
          code: 400,
          headers: {
            "X-My-Header": "MyHeader"
          }
        });
        send("bad request");
      }
      """,
      allDocs: """
        function(head, req){
        start({'headers': {'Content-Type': 'application/json'}});
        var resp = head;
        var rows = [];
        while(row=getRow()){
          rows.push(row);
        }
        resp.rows = rows;
        return toJSON(resp);
      }
      """
    }
  }

  @view_only_design_doc %{
    _id: "_design/views",
    language: "javascript",
    views: %{
      basicView: %{
        map: """
         function(doc) {
          emit(-doc.integer, doc.string);
        }
        """
      }
    }
  }

  @erl_list_doc %{
    _id: "_design/erlang",
    language: "erlang",
    lists: %{
      simple: """
      fun(Head, {Req}) ->
        Send(<<"[">>),
        Fun = fun({Row}, Sep) ->
          Val = couch_util:get_value(<<"key">>, Row, 23),
          Send(list_to_binary(Sep ++ integer_to_list(Val))),
          {ok, ","}
        end,
        {ok, _} = FoldRows(Fun, ""),
        Send(<<"]">>)
      end.
      """
    }
  }

  setup_all do
    db_name = random_db_name()
    {:ok, _} = create_db(db_name)
    on_exit(fn -> delete_db(db_name) end)

    {:ok, _} = create_doc(db_name, @ddoc)
    bulk_save(db_name, make_docs(0..9))

    # Check setup
    resp = view(db_name, "lists/basicView")
    assert resp.body["total_rows"] == 10

    db_name_cross = "#{db_name}_cross"
    {:ok, _} = create_db(db_name_cross)
    on_exit(fn -> delete_db(db_name_cross) end)

    {:ok, _} = create_doc(db_name_cross, @ddoc)
    {:ok, _} = create_doc(db_name_cross, @view_only_design_doc)
    bulk_save(db_name_cross, make_docs(0..9))

    db_name_erlang = "#{db_name}_erlang"
    {:ok, _} = create_db(db_name_erlang)
    on_exit(fn -> delete_db(db_name_erlang) end)

    {:ok, _} = create_doc(db_name_erlang, @erl_list_doc)
    {:ok, _} = create_doc(db_name_erlang, @view_only_design_doc)
    bulk_save(db_name_erlang, make_docs(0..9))

    {:ok,
     [db_name: db_name, db_name_cross: db_name_cross, db_name_erlang: db_name_erlang]}
  end

  test "standard GET", context do
    db_name = context[:db_name]
    resp = Rawresp.get("/#{db_name}/_design/lists/_list/basicBasic/basicView")
    assert resp.status_code == 200
    assert String.match?(resp.body, ~r/head0123456789tail/)
  end

  test "standard OPTIONS", context do
    db_name = context[:db_name]
    resp = Rawresp.options("/#{db_name}/_design/lists/_list/basicBasic/basicView")
    assert resp.status_code == 200
    assert String.match?(resp.body, ~r/head0123456789tail/)
  end

  test "the richness of the arguments", context do
    db_name = context[:db_name]

    resp =
      Couch.get("/#{db_name}/_design/lists/_list/basicJSON/basicView?update_seq=true")

    assert resp.status_code == 200
    assert resp.body["head"]["total_rows"] == 10
    assert resp.body["head"]["offset"] == 0
    assert length(resp.body["rows"]) == 10
    assert Enum.at(resp.body["rows"], 0) == %{"id" => "0", "key" => 0, "value" => "0"}
    assert resp.body["req"]["info"]["db_name"] == db_name
    assert resp.body["req"]["method"] == "GET"

    assert resp.body["req"]["path"] == [
             db_name,
             "_design",
             "lists",
             "_list",
             "basicJSON",
             "basicView"
           ]

    assert Map.has_key?(resp.body["req"]["headers"], "Host") == true
    assert Map.has_key?(resp.body["req"]["headers"], "User-Agent") == true
    assert Map.has_key?(resp.body["req"], "cookie")

    assert resp.body["req"]["raw_path"] ==
             "/#{db_name}/_design/lists/_list/basicJSON/basicView?update_seq=true"
  end

  test "get with query params", context do
    db_name = context[:db_name]

    resp =
      Rawresp.get(
        "/#{db_name}/_design/lists/_list/simpleForm/basicView?startkey=3&endkey=8"
      )

    assert resp.status_code == 200
    assert not String.match?(resp.body, ~r/Key: 1/)
    assert String.match?(resp.body, ~r/FirstKey: 3/)
    assert String.match?(resp.body, ~r/LastKey: 8/)
  end

  test "with 0 rows", context do
    db_name = context[:db_name]

    resp = Rawresp.get("/#{db_name}/_design/lists/_list/simpleForm/basicView?startkey=30")

    assert resp.status_code == 200
    assert String.match?(resp.body, ~r/<\/ul>/)
  end

  test "too many Get Rows", context do
    db_name = context[:db_name]

    resp = Rawresp.get("/#{db_name}/_design/lists/_list/tooManyGetRows/basicView")

    assert resp.status_code == 200
    assert String.match?(resp.body, ~r/9after row: null/)
  end

  test "reduce with 0 rows", context do
    db_name = context[:db_name]

    resp =
      Rawresp.get("/#{db_name}/_design/lists/_list/simpleForm/withReduce?startkey=30")

    assert resp.status_code == 200
    assert String.match?(resp.body, ~r/LastKey: undefined/)
  end

  test "when there is a reduce present, but not used", context do
    db_name = context[:db_name]

    resp =
      Rawresp.get("/#{db_name}/_design/lists/_list/simpleForm/withReduce?reduce=false")

    assert resp.status_code == 200
    assert String.match?(resp.body, ~r/Key: 1/)
  end

  test "when there is a reduce present, and used", context do
    db_name = context[:db_name]

    resp = Rawresp.get("/#{db_name}/_design/lists/_list/simpleForm/withReduce?group=true")

    assert resp.status_code == 200
    assert String.match?(resp.body, ~r/Key: 1/)
  end

  test "empty list", context do
    db_name = context[:db_name]

    resp = Rawresp.get("/#{db_name}/_design/lists/_list/emptyList/basicView")
    assert String.match?(resp.body, ~r/^ $/)

    resp = Rawresp.get("/#{db_name}/_design/lists/_list/emptyList/withReduce?group=true")
    assert String.match?(resp.body, ~r/^ $/)
  end

  test "multi-key fetch with POST", context do
    db_name = context[:db_name]

    resp =
      Rawresp.post("/#{db_name}/_design/lists/_list/simpleForm/basicView",
        body: %{keys: [2, 4, 5, 7]}
      )

    assert resp.status_code == 200
    assert not String.match?(resp.body, ~r/Key: 1/)
    assert String.match?(resp.body, ~r/Key: 2/)
    assert String.match?(resp.body, ~r/FirstKey: 2/)
    assert String.match?(resp.body, ~r/LastKey: 7/)
  end

  test "multi-key fetch with GET", context do
    db_name = context[:db_name]

    resp =
      Rawresp.get("/#{db_name}/_design/lists/_list/simpleForm/basicView?keys=[2,4,5,7]")

    assert resp.status_code == 200
    assert not String.match?(resp.body, ~r/Key: 1/)
    assert String.match?(resp.body, ~r/Key: 2/)
    assert String.match?(resp.body, ~r/FirstKey: 2/)
    assert String.match?(resp.body, ~r/LastKey: 7/)
  end

  test "no multi-key fetch allowed when group=false", context do
    db_name = context[:db_name]

    resp =
      Rawresp.post("/#{db_name}/_design/lists/_list/simpleForm/withReduce?group=false",
        body: %{keys: [2, 4, 5, 7]}
      )

    assert resp.status_code == 400
    assert String.match?(resp.body, ~r/query_parse_error/)

    resp = Rawresp.get("/#{db_name}/_design/lists/_list/rowError/basicView")
    assert String.match?(resp.body, ~r/ReferenceError/)
  end

  test "with include_docs and a reference to the doc", context do
    db_name = context[:db_name]

    resp =
      Rawresp.get(
        "/#{db_name}/_design/lists/_list/docReference/basicView?include_docs=true"
      )

    assert String.match?(resp.body, ~r/head0tail/)
  end

  test "extra qs params", context do
    db_name = context[:db_name]
    resp = Rawresp.get("/#{db_name}/_design/lists/_list/qsParams/basicView?foo=blam")
    assert String.match?(resp.body, ~r/blam/)
  end

  test "stop iteration", context do
    db_name = context[:db_name]
    resp = Rawresp.get("/#{db_name}/_design/lists/_list/stopIter/basicView")
    assert String.match?(resp.body, ~r/^head 0 1 2 tail$/)

    resp =
      Rawresp.get("/#{db_name}/_design/lists/_list/stopIter2/basicView",
        headers: [Accept: "text/html"]
      )

    assert String.match?(resp.body, ~r/^head 0 1 2 tail$/)
  end

  test "abort iteration with reduce", context do
    db_name = context[:db_name]

    resp = Rawresp.get("/#{db_name}/_design/lists/_list/stopIter/withReduce?group=true")
    assert String.match?(resp.body, ~r/^head 0 1 2 tail$/)

    resp =
      Rawresp.get("/#{db_name}/_design/lists/_list/stopIter2/withReduce?group=true",
        headers: [Accept: "text/html"]
      )

    assert String.match?(resp.body, ~r/^head 0 1 2 tail$/)
  end

  test "with accept headers for HTML", context do
    db_name = context[:db_name]

    resp =
      Rawresp.get("/#{db_name}/_design/lists/_list/acceptSwitch/basicView",
        headers: [Accept: "text/html"]
      )

    assert resp.headers["Content-Type"] == "text/html; charset=utf-8"
    assert String.match?(resp.body, ~r/HTML/)
    assert String.match?(resp.body, ~r/Value/)
  end

  test "we can run lists and views from separate docs", context do
    db_name = context[:db_name_cross]

    resp =
      Rawresp.get(
        "/#{db_name}/_design/lists/_list/simpleForm/views/basicView?startkey=-3"
      )

    assert resp.status_code == 200
    assert not String.match?(resp.body, ~r/Key: -4/)
    assert String.match?(resp.body, ~r/FirstKey: -3/)
    assert String.match?(resp.body, ~r/LastKey: 0/)
  end

  test "we do multi-key requests on lists and views in separate docs", context do
    db_name = context[:db_name_cross]

    resp =
      Rawresp.post(
        "/#{db_name}/_design/lists/_list/simpleForm/views/basicView",
        body: %{keys: [-2, -4, -5, -7]}
      )

    assert resp.status_code == 200
    assert not String.match?(resp.body, ~r/Key: -3/)
    assert String.match?(resp.body, ~r/Key: -7/)
    assert String.match?(resp.body, ~r/FirstKey: -2/)
    assert String.match?(resp.body, ~r/LastKey: -7/)
  end

  test "secObj is available", context do
    db_name = context[:db_name]

    resp = Couch.get("/#{db_name}/_design/lists/_list/secObj/basicView")
    assert resp.status_code == 200
    assert is_map(resp.body)
  end

  test "multiple languages in design docs", context do
    db_name = context[:db_name_erlang]

    resp =
      Couch.get("/#{db_name}/_design/erlang/_list/simple/views/basicView?startkey=-3")

    assert resp.status_code == 200
    assert length(resp.body) == 4

    for i <- 0..3 do
      assert Enum.at(resp.body, i) + 3 == i
    end
  end

  @tag :with_db
  test "COUCHDB-1113", context do
    db_name = context[:db_name]

    ddoc = %{
      _id: "_design/test",
      views: %{
        me: %{
          map: "function(doc) { emit(null,null)}"
        }
      },
      lists: %{
        you: """
         function(head, req) {
          var row;
          while(row = getRow()) {
            send(row);
          }
        }
        """
      }
    }

    {:ok, _} = create_doc(db_name, ddoc)

    resp =
      Couch.get("/#{db_name}/_design/test/_list/you/me",
        headers: [
          "Content-Type": "application/x-www-form-urlencoded"
        ]
      )

    assert resp.status_code == 200
  end

  test "HTTP header response set after getRow() called in _list function", context do
    db_name = context[:db_name]

    resp = Rawresp.get("/#{db_name}/_design/lists/_list/setHeaderAfterGotRow/basicView")
    assert resp.status_code == 400
    assert resp.headers["X-My-Header"] == "MyHeader"
    assert String.match?(resp.body, ~r/^bad request$/)
  end

  test "handling _all_docs by _list functions. the result should be equal", context do
    db_name = context[:db_name]

    resp_list = Couch.get("/#{db_name}/_design/lists/_list/allDocs/_all_docs")
    assert resp_list.status_code == 200

    resp_alldocs = Couch.get("/#{db_name}/_all_docs")

    assert resp_list.body["total_rows"] == resp_alldocs.body["total_rows"]
    assert resp_list.body["offset"] == resp_alldocs.body["offset"]
    assert length(resp_list.body["rows"]) == length(resp_alldocs.body["rows"])
    assert resp_list.body["rows"] == resp_alldocs.body["rows"]
  end
end

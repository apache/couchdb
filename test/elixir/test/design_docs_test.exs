defmodule DesignDocsTest do
  use CouchTestCase

  @moduletag :design_docs
  @moduletag kind: :single_node

  @design_doc %{
    _id: "_design/test",
    language: "javascript",
    autoupdate: false,
    whatever: %{
      stringzone: "exports.string = 'plankton';",
      commonjs: %{
        whynot: """
        exports.test = require('../stringzone');
        exports.foo = require('whatever/stringzone');
        """,
        upper: """
        exports.testing = require('./whynot').test.string.toUpperCase()+
        module.id+require('./whynot').foo.string
        """,
        circular_one: "require('./circular_two'); exports.name = 'One';",
        circular_two: "require('./circular_one'); exports.name = 'Two';"
      },
      # paths relative to parent
      idtest1: %{
        a: %{
          b: %{d: "module.exports = require('../c/e').id;"},
          c: %{e: "exports.id = module.id;"}
        }
      },
      # multiple paths relative to parent
      idtest2: %{
        a: %{
          b: %{d: "module.exports = require('../../a/c/e').id;"},
          c: %{e: "exports.id = module.id;"}
        }
      },
      # paths relative to module
      idtest3: %{
        a: %{
          b: "module.exports = require('./c/d').id;",
          c: %{
            d: "module.exports = require('./e');",
            e: "exports.id = module.id;"
          }
        }
      },
      # paths relative to module and parent
      idtest4: %{
        a: %{
          b: "module.exports = require('../a/./c/d').id;",
          c: %{
            d: "module.exports = require('./e');",
            e: "exports.id = module.id;"
          }
        }
      },
      # paths relative to root
      idtest5: %{
        a: "module.exports = require('whatever/idtest5/b').id;",
        b: "exports.id = module.id;"
      }
    },
    views: %{
      all_docs_twice: %{
        map: """
        function(doc) {
          emit(doc.integer, null);
          emit(doc.integer, null);
        }
        """
      },
      no_docs: %{
        map: """
        function(doc) {}
        """
      },
      single_doc: %{
        map: """
        function(doc) {
          if (doc._id === "1") {
            emit(1, null);
          }
        }
        """
      },
      summate: %{
        map: """
        function(doc) {
          emit(doc.integer, doc.integer);
        }
        """,
        reduce: """
        function(keys, values) {
          return sum(values);
        }
        """
      },
      summate2: %{
        map: """
        function(doc) {
          emit(doc.integer, doc.integer);
        }
        """,
        reduce: """
        function(keys, values) {
          return sum(values);
        }
        """
      },
      huge_src_and_results: %{
        map: """
         function(doc) {
           if (doc._id === "1") {
             emit("#{String.duplicate("a", 16)}", null);
           }
         }
        """,
        reduce: """
        function(keys, values) {
          return "#{String.duplicate("a", 16)}";
        }
        """
      },
      lib: %{
        baz: "exports.baz = 'bam';",
        foo: %{
          foo: "exports.foo = 'bar';",
          boom: "exports.boom = 'ok';",
          zoom: "exports.zoom = 'yeah';"
        }
      },
      commonjs: %{
        map: """
        function(doc) {
          emit(null, require('views/lib/foo/boom').boom);
        }
        """
      }
    },
    shows: %{
      simple: """
      function() {
        return 'ok';
      }
      """,
      requirey: """
      function() {
        var lib = require('whatever/commonjs/upper');
        return lib.testing;
      }
      """,
      circular: """
      function() {
        var lib = require('whatever/commonjs/upper');
        return JSON.stringify(this);
      }
      """,
      circular_require: """
      function() {
        return require('whatever/commonjs/circular_one').name;
      }
      """,
      idtest1: """
      function() {
        return require('whatever/idtest1/a/b/d');
      }
      """,
      idtest2: """
      function() {
        return require('whatever/idtest2/a/b/d');
      }
      """,
      idtest3: """
      function() {
        return require('whatever/idtest3/a/b');
      }
      """,
      idtest4: """
      function() {
        return require('whatever/idtest4/a/b');
      }
      """,
      idtest5: """
      function() {
        return require('whatever/idtest5/a');
      }
      """
    }
  }

  setup_all do
    db_name = random_db_name()
    {:ok, _} = create_db(db_name)
    on_exit(fn -> delete_db(db_name) end)

    {:ok, _} = create_doc(db_name, @design_doc)
    {:ok, _} = create_doc(db_name, %{})
    {:ok, [db_name: db_name]}
  end

  test "consistent _rev for design docs", context do
    resp = Couch.get("/#{context[:db_name]}/_design/test")
    assert resp.status_code == 200
    first_db_rev = resp.body["_rev"]

    second_db_name = random_db_name()
    create_db(second_db_name)
    {:ok, resp2} = create_doc(second_db_name, @design_doc)
    assert first_db_rev == resp2.body["rev"]
  end

  @tag :pending # HTTP 410
  test "commonjs require", context do
    db_name = context[:db_name]
    resp = Couch.get("/#{db_name}/_design/test/_show/requirey")
    assert resp.status_code == 200
    assert resp.body == "PLANKTONwhatever/commonjs/upperplankton"

    resp = Couch.get("/#{db_name}/_design/test/_show/circular")
    assert resp.status_code == 200

    result =
      resp.body
      |> IO.iodata_to_binary()
      |> :jiffy.decode([:return_maps])

    assert result["language"] == "javascript"
  end

  @tag :pending # HTTP 410
  test "circular commonjs dependencies", context do
    db_name = context[:db_name]
    resp = Couch.get("/#{db_name}/_design/test/_show/circular_require")
    assert resp.status_code == 200
    assert resp.body == "One"
  end

  @tag :pending # HTTP 410
  test "module id values are as expected", context do
    db_name = context[:db_name]

    check_id_value(db_name, "idtest1", "whatever/idtest1/a/c/e")
    check_id_value(db_name, "idtest2", "whatever/idtest2/a/c/e")
    check_id_value(db_name, "idtest3", "whatever/idtest3/a/c/e")
    check_id_value(db_name, "idtest4", "whatever/idtest4/a/c/e")
    check_id_value(db_name, "idtest5", "whatever/idtest5/b")
  end

  defp check_id_value(db_name, id, expected) do
    resp = Couch.get("/#{db_name}/_design/test/_show/#{id}")
    assert resp.status_code == 200
    assert resp.body == expected
  end

  @tag :pending # No compact_running key
  @tag :with_db
  test "that we get correct design doc info back", context do
    db_name = context[:db_name]
    {:ok, _} = create_doc(db_name, @design_doc)

    resp = Couch.get("/#{db_name}/_design/test/_info")
    prev_view_sig = resp.body["view_index"]["signature"]
    prev_view_size = resp.body["view_index"]["sizes"]["file"]

    num_docs = 500
    bulk_save(db_name, make_docs(1..(num_docs + 1)))

    Couch.get("/#{db_name}/_design/test/_view/summate", query: [stale: "ok"])

    for _x <- 0..1 do
      resp = Couch.get("/#{db_name}/_design/test/_info")
      assert resp.body["name"] == "test"
      assert resp.body["view_index"]["sizes"]["file"] == prev_view_size
      assert resp.body["view_index"]["compact_running"] == false
      assert resp.body["view_index"]["signature"] == prev_view_sig
    end
  end

  test "commonjs in map functions", context do
    db_name = context[:db_name]

    resp = Couch.get("/#{db_name}/_design/test/_view/commonjs", query: [limit: 1])
    assert resp.status_code == 200
    assert Enum.at(resp.body["rows"], 0)["value"] == "ok"
  end

  test "_all_docs view returns correctly with keys", context do
    db_name = context[:db_name]

    resp =
      Couch.get("/#{db_name}/_all_docs",
        query: [startkey: :jiffy.encode("_design"), endkey: :jiffy.encode("_design0")]
      )

    assert length(resp.body["rows"]) == 1
  end

  @tag :with_db
  test "all_docs_twice", context do
    db_name = context[:db_name]
    {:ok, _} = create_doc(db_name, @design_doc)

    num_docs = 500
    bulk_save(db_name, make_docs(1..(2 * num_docs)))

    for _x <- 0..1 do
      test_all_docs_twice(db_name, num_docs)
    end
  end

  defp test_all_docs_twice(db_name, num_docs) do
    resp = Couch.get("/#{db_name}/_design/test/_view/all_docs_twice")
    assert resp.status_code == 200
    rows = resp.body["rows"]

    for x <- 0..num_docs do
      assert Map.get(Enum.at(rows, 2 * x), "key") == x + 1
      assert Map.get(Enum.at(rows, 2 * x + 1), "key") == x + 1
    end

    resp = Couch.get("/#{db_name}/_design/test/_view/no_docs")
    assert resp.body["total_rows"] == 0

    resp = Couch.get("/#{db_name}/_design/test/_view/single_doc")
    assert resp.body["total_rows"] == 1
  end

  @tag :with_db
  test "language not specified, Javascript is implied", context do
    db_name = context[:db_name]
    bulk_save(db_name, make_docs(1..2))

    design_doc_2 = %{
      _id: "_design/test2",
      views: %{
        single_doc: %{
          map: """
          function(doc) {
            if (doc._id === "1") {
              emit(1, null);
            }
          }
          """
        }
      }
    }

    {:ok, _} = create_doc(db_name, design_doc_2)

    resp = Couch.get("/#{db_name}/_design/test2/_view/single_doc")
    assert resp.status_code == 200
    assert length(resp.body["rows"]) == 1
  end

  @tag :with_db
  test "startkey and endkey", context do
    db_name = context[:db_name]
    {:ok, _} = create_doc(db_name, @design_doc)

    num_docs = 500
    bulk_save(db_name, make_docs(1..(2 * num_docs)))

    resp = Couch.get("/#{db_name}/_design/test/_view/summate")
    assert Enum.at(resp.body["rows"], 0)["value"] == summate(num_docs * 2)

    resp =
      Couch.get("/#{db_name}/_design/test/_view/summate",
        query: [startkey: 4, endkey: 4]
      )

    assert Enum.at(resp.body["rows"], 0)["value"] == 4

    resp =
      Couch.get("/#{db_name}/_design/test/_view/summate",
        query: [startkey: 4, endkey: 5]
      )

    assert Enum.at(resp.body["rows"], 0)["value"] == 9

    resp =
      Couch.get("/#{db_name}/_design/test/_view/summate",
        query: [startkey: 4, endkey: 6]
      )

    assert Enum.at(resp.body["rows"], 0)["value"] == 15

    # test start_key and end_key aliases
    resp =
      Couch.get("/#{db_name}/_design/test/_view/summate",
        query: [start_key: 4, end_key: 6]
      )

    assert Enum.at(resp.body["rows"], 0)["value"] == 15

    # Verify that a shared index (view def is an exact copy of "summate")
    # does not confuse the reduce stage
    resp =
      Couch.get("/#{db_name}/_design/test/_view/summate2",
        query: [startkey: 4, endkey: 6]
      )

    assert Enum.at(resp.body["rows"], 0)["value"] == 15

    for x <- 0..Integer.floor_div(num_docs, 60) do
      resp =
        Couch.get("/#{db_name}/_design/test/_view/summate",
          query: [startkey: x * 30, endkey: num_docs - x * 30]
        )

      assert Enum.at(resp.body["rows"], 0)["value"] ==
               summate(num_docs - x * 30) - summate(x * 30 - 1)
    end
  end

  defp summate(n) do
    (n + 1) * (n / 2)
  end

  @tag :with_db
  test "design doc deletion", context do
    db_name = context[:db_name]
    {:ok, resp} = create_doc(db_name, @design_doc)

    del_resp =
      Couch.delete("/#{db_name}/#{resp.body["id"]}", query: [rev: resp.body["rev"]])

    assert del_resp.status_code == 200

    resp = Couch.get("/#{db_name}/#{resp.body["id"]}")
    assert resp.status_code == 404

    resp = Couch.get("/#{db_name}/_design/test/_view/no_docs")
    assert resp.status_code == 404
  end

  @tag :with_db
  test "validate doc update", context do
    db_name = context[:db_name]

    # COUCHDB-1227 - if a design document is deleted, by adding a "_deleted"
    # field with the boolean value true, its validate_doc_update functions
    # should no longer have effect.

    ddoc = %{
      _id: "_design/test",
      language: "javascript",
      validate_doc_update: """
        function(newDoc, oldDoc, userCtx, secObj) {
         if (newDoc.value % 2 == 0) {
            throw({forbidden: "dont like even numbers"});
         }
         return true;
      }
      """
    }

    {:ok, resp_ddoc} = create_doc(db_name, ddoc)

    resp =
      Couch.post("/#{db_name}",
        body: %{_id: "doc1", value: 4}
      )

    assert resp.status_code == 403
    assert resp.body["reason"] == "dont like even numbers"

    ddoc_resp = Couch.get("/#{db_name}/#{resp_ddoc.body["id"]}")

    ddoc =
      ddoc_resp.body
      |> Map.put("_deleted", true)

    del_resp =
      Couch.post("/#{db_name}",
        body: ddoc
      )

    assert del_resp.status_code in [201, 202]

    {:ok, _} = create_doc(db_name, %{_id: "doc1", value: 4})
  end
end

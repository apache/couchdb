defmodule ViewMultiKeyDesignTest do
  use CouchTestCase
  
  @moduletag kind: :single_node

  @keys [10, 15, 30, 37, 50]

  @ddoc %{
    _id: "_design/test",
    language: "javascript",
    views: %{
      all_docs: %{
        map: "function(doc) { emit(doc.integer, doc.string) }"
      },
      multi_emit: %{
        map: "function(doc) {for(var i = 0 ; i < 3 ; i++) { emit(i, doc.integer) ; } }"
      },
      summate: %{
        map: "function (doc) {emit(doc.integer, doc.integer)};",
        reduce: "function (keys, values) { return sum(values); };"
      }
    }
  }

  setup_all do
    db_name = random_db_name()
    {:ok, _} = create_db(db_name)
    on_exit(fn -> delete_db(db_name) end)

    bulk_save(db_name, make_docs(0..99))
    {:ok, _} = create_doc(db_name, @ddoc)

    {:ok, [db_name: db_name]}
  end

  test "that missing keys work too", context do
    db_name = context[:db_name]
    keys = [101, 30, 15, 37, 50]
    resp = view(db_name, "test/summate", [group: true], keys)
    rows = resp.body["rows"]
    assert length(rows) == length(keys) - 1

    assert Enum.all?(rows, &Enum.member?(keys, &1["key"]))
    assert Enum.all?(rows, &(&1["key"] == &1["value"]))
  end

  test "keys in POST body", context do
    db_name = context[:db_name]
    resp = view(db_name, "test/all_docs", nil, @keys)
    rows = resp.body["rows"]
    assert length(rows) == length(@keys)
    assert Enum.all?(rows, &Enum.member?(@keys, &1["key"]))
    assert Enum.all?(rows, &(&1["key"] == String.to_integer(&1["value"])))
  end

  test "keys in GET parameters", context do
    db_name = context[:db_name]
    resp = view(db_name, "test/all_docs", keys: :jiffy.encode(@keys))
    rows = resp.body["rows"]
    assert length(rows) == length(@keys)
    assert Enum.all?(rows, &Enum.member?(@keys, &1["key"]))
    assert Enum.all?(rows, &(&1["key"] == String.to_integer(&1["value"])))
  end

  test "empty keys", context do
    db_name = context[:db_name]

    resp = view(db_name, "test/all_docs", keys: :jiffy.encode([]))
    assert resp.status_code == 200
    rows = resp.body["rows"]
    assert Enum.empty?(rows)
  end

  test "keys in POST body (group)", context do
    db_name = context[:db_name]
    resp = view(db_name, "test/summate", [group: true], @keys)
    rows = resp.body["rows"]
    assert length(rows) == length(@keys)
    assert Enum.all?(rows, &Enum.member?(@keys, &1["key"]))
    assert Enum.all?(rows, &(&1["key"] == &1["value"]))
  end

  test "keys in GET body (group)", context do
    db_name = context[:db_name]
    resp = view(db_name, "test/summate", group: true, keys: :jiffy.encode(@keys))
    rows = resp.body["rows"]
    assert length(rows) == length(@keys)
    assert Enum.all?(rows, &Enum.member?(@keys, &1["key"]))
    assert Enum.all?(rows, &(&1["key"] == &1["value"]))
  end

  test "POST - invalid parameter combinations get rejected ", context do
    db_name = context[:db_name]

    badargs = [[startkey: 0], [endkey: 0], [key: 0], [group_level: 2]]

    Enum.each(badargs, fn args ->
      resp =
        Couch.post("/#{db_name}/_design/test/_view/all_docs",
          query: args,
          body: %{"keys" => @keys}
        )

      assert resp.status_code == 400
      assert resp.body["error"] == "query_parse_error"
    end)

    resp =
      Couch.post("/#{db_name}/_design/test/_view/summate",
        query: nil,
        body: %{"keys" => @keys}
      )

    assert resp.status_code == 400
    assert resp.body["error"] == "query_parse_error"
  end

  test "GET - invalid parameter combinations get rejected ", context do
    db_name = context[:db_name]

    badargs = [
      [startkey: 0, keys: :jiffy.encode(@keys)],
      [endkey: 0, keys: :jiffy.encode(@keys)],
      [key: 0, keys: :jiffy.encode(@keys)],
      [group_level: 2, keys: :jiffy.encode(@keys)]
    ]

    Enum.each(badargs, fn args ->
      resp =
        Couch.get("/#{db_name}/_design/test/_view/all_docs",
          query: args
        )

      assert resp.status_code == 400
      assert resp.body["error"] == "query_parse_error"
    end)

    resp =
      Couch.get("/#{db_name}/_design/test/_view/summate",
        query: [keys: :jiffy.encode(@keys)],
        body: %{"keys" => @keys}
      )

    assert resp.status_code == 400
    assert resp.body["error"] == "query_parse_error"
  end

  test "that a map & reduce containing func support keys when reduce=false", context do
    db_name = context[:db_name]
    resp = view(db_name, "test/summate", [reduce: false], @keys)
    assert length(resp.body["rows"]) == 5

    resp = view(db_name, "test/summate", reduce: false, keys: :jiffy.encode(@keys))
    assert length(resp.body["rows"]) == 5
  end

  test "that limiting by startkey_docid and endkey_docid get applied", context do
    db_name = context[:db_name]

    exp_key = [0, 0, 0, 2, 2, 2]
    exp_val = [21, 22, 23, 21, 22, 23]

    resp =
      view(db_name, "test/multi_emit", [startkey_docid: 21, endkey_docid: 23], [0, 2])

    rows = resp.body["rows"]
    rows_key = Enum.map(rows, & &1["key"])
    assert rows_key == exp_key

    rows_value = Enum.map(rows, & &1["value"])
    assert rows_value == exp_val

    resp =
      view(db_name, "test/multi_emit",
        startkey_docid: 21,
        endkey_docid: 23,
        keys: :jiffy.encode([0, 2])
      )

    rows = resp.body["rows"]
    rows_key = Enum.map(rows, & &1["key"])
    assert rows_key == exp_key

    rows_value = Enum.map(rows, & &1["value"])
    assert rows_value == exp_val
  end

  test "limit works", context do
    db_name = context[:db_name]

    resp = view(db_name, "test/all_docs", [limit: 1], @keys)
    rows = resp.body["rows"]
    assert length(rows) == 1
    assert Enum.at(rows, 0)["key"] == 10

    resp = view(db_name, "test/all_docs", limit: 1, keys: :jiffy.encode(@keys))
    rows = resp.body["rows"]
    assert length(rows) == 1
    assert Enum.at(rows, 0)["key"] == 10
  end

  test "offset works", context do
    db_name = context[:db_name]

    resp = view(db_name, "test/multi_emit", [skip: 1], [0])
    rows = resp.body["rows"]
    assert length(rows) == 99

    resp = view(db_name, "test/multi_emit", skip: 1, keys: :jiffy.encode([0]))
    rows = resp.body["rows"]
    assert length(rows) == 99
  end

  test "dir works", context do
    db_name = context[:db_name]

    resp = view(db_name, "test/multi_emit", [descending: true], [1])
    rows = resp.body["rows"]
    assert length(rows) == 100

    resp = view(db_name, "test/multi_emit", descending: true, keys: :jiffy.encode([1]))
    rows = resp.body["rows"]
    assert length(rows) == 100
  end

  test "argument combinations", context do
    db_name = context[:db_name]

    resp = view(db_name, "test/multi_emit", [descending: true, skip: 3, limit: 2], [2])
    rows = resp.body["rows"]
    assert length(rows) == 2

    resp =
      view(db_name, "test/multi_emit",
        descending: true,
        skip: 3,
        limit: 2,
        keys: :jiffy.encode([2])
      )

    rows = resp.body["rows"]
    assert length(rows) == 2

    resp =
      view(db_name, "test/multi_emit", [skip: 0, limit: 1, startkey_docid: "13"], [0])

    rows = resp.body["rows"]
    assert length(rows) == 1
    assert Enum.at(rows, 0)["value"] == 13

    resp =
      view(db_name, "test/multi_emit", [skip: 2, limit: 3, startkey_docid: "13"], [0])

    rows = resp.body["rows"]
    assert length(rows) == 3

    resp =
      view(db_name, "test/multi_emit",
        skip: 2,
        limit: 3,
        startkey_docid: "13",
        keys: :jiffy.encode([0])
      )

    rows = resp.body["rows"]
    assert length(rows) == 3

    resp =
      view(
        db_name,
        "test/multi_emit",
        [skip: 1, limit: 5, startkey_docid: "25", endkey_docid: "27"],
        [1]
      )

    rows = resp.body["rows"]
    assert length(rows) == 2
    assert Enum.at(rows, 0)["value"] == 26 or assert(Enum.at(rows, 0)["value"] == 27)

    resp =
      view(db_name, "test/multi_emit",
        skip: 1,
        limit: 5,
        startkey_docid: "25",
        endkey_docid: "27",
        keys: :jiffy.encode([1])
      )

    rows = resp.body["rows"]
    assert length(rows) == 2
    assert Enum.at(rows, 0)["value"] == 26 or assert(Enum.at(rows, 0)["value"] == 27)

    resp =
      view(
        db_name,
        "test/multi_emit",
        [skip: 1, limit: 5, startkey_docid: "28", endkey_docid: "26", descending: true],
        [1]
      )

    rows = resp.body["rows"]
    assert length(rows) == 2
    assert Enum.at(rows, 0)["value"] == 26 or assert(Enum.at(rows, 0)["value"] == 27)

    resp =
      view(db_name, "test/multi_emit",
        skip: 1,
        limit: 5,
        startkey_docid: "28",
        endkey_docid: "26",
        descending: true,
        keys: :jiffy.encode([1])
      )

    rows = resp.body["rows"]
    assert length(rows) == 2
  end
end

defmodule ReduceTest do
  use CouchTestCase

  @moduletag :views
  @moduletag kind: :single_node

  @moduledoc """
  Test CouchDB view reduces
  This is a port of the reduce.js suite
  """

  def summate(n) do
    (n + 1) * n / 2
  end

  @tag :with_db
  test "Basic reduce functions", context do
    db_name = context[:db_name]
    view_url = "/#{db_name}/_design/foo/_view/bar"
    num_docs = 500

    map = ~s"""
    function (doc) {
      emit(doc.integer, doc.integer);
      emit(doc.integer, doc.integer);
    };
    """

    reduce = "function (keys, values) { return sum(values); };"
    red_doc = %{:views => %{:bar => %{:map => map, :reduce => reduce}}}

    assert Couch.put("/#{db_name}/_design/foo", body: red_doc).body["ok"]
    docs = make_docs(1..num_docs)

    assert Couch.post("/#{db_name}/_bulk_docs", body: %{:docs => docs}, query: %{w: 3}).status_code in
             [201, 202]

    rows = Couch.get(view_url).body["rows"]
    assert hd(rows)["value"] == 2 * summate(num_docs)

    query = %{:startkey => 4, :endkey => 4}
    rows = Couch.get(view_url, query: query).body["rows"]
    assert hd(rows)["value"] == 8

    query = %{:startkey => 4, :endkey => 5}
    rows = Couch.get(view_url, query: query).body["rows"]
    assert hd(rows)["value"] == 18

    query = %{:startkey => 4, :endkey => 6}
    rows = Couch.get(view_url, query: query).body["rows"]
    assert hd(rows)["value"] == 30

    query = %{:group => true, :limit => 3}
    rows = Couch.get(view_url, query: query).body["rows"]
    assert Enum.at(rows, 0)["value"] == 2
    assert Enum.at(rows, 1)["value"] == 4
    assert Enum.at(rows, 2)["value"] == 6

    half_num_docs = Integer.floor_div(num_docs, 2)
    max = Integer.floor_div(num_docs, 30) + 1

    for i <- 1..max, i * 30 + 1 < half_num_docs do
      i = i * 30 + 1
      query = %{:startkey => i, :endkey => num_docs - i}
      rows = Couch.get(view_url, query: query).body["rows"]
      assert hd(rows)["value"] == 2 * (summate(num_docs - i) - summate(i - 1))
    end
  end

  @tag :with_db
  test "More complex array key view row testing", context do
    db_name = context[:db_name]
    view_url = "/#{db_name}/_design/foo/_view/bar"
    map = "function (doc) { emit(doc.keys, 1); };"
    reduce = "function (keys, values) { return sum(values); };"
    red_doc = %{:views => %{bar: %{map: map, reduce: reduce}}}

    assert Couch.put("/#{db_name}/_design/foo", body: red_doc).body["ok"]

    for i <- 1..5 do
      for j <- 0..9 do
        docs = [
          %{keys: ["a"]},
          %{keys: ["a"]},
          %{keys: ["a", "b"]},
          %{keys: ["a", "b"]},
          %{keys: ["a", "b", "c"]},
          %{keys: ["a", "b", "d"]},
          %{keys: ["a", "c", "d"]},
          %{keys: ["d"]},
          %{keys: ["d", "a"]},
          %{keys: ["d", "b"]},
          %{keys: ["d", "c"]}
        ]

        assert Couch.post("/#{db_name}/_bulk_docs", body: %{docs: docs}, query: %{w: 3}).status_code in
                 [201, 202]

        total_docs = 1 + (i - 1) * 10 * 11 + (j + 1) * 11
        assert Couch.get("/#{db_name}").body["doc_count"] == total_docs
      end

      # test group by exact key match
      query = %{group: true}
      rows = Couch.get(view_url, query: query).body["rows"]
      assert Enum.at(rows, 0) == %{"key" => ["a"], "value" => 20 * i}
      assert Enum.at(rows, 0) == %{"key" => ["a"], "value" => 20 * i}
      assert Enum.at(rows, 0) == %{"key" => ["a"], "value" => 20 * i}
      assert Enum.at(rows, 1) == %{"key" => ["a", "b"], "value" => 20 * i}
      assert Enum.at(rows, 2) == %{"key" => ["a", "b", "c"], "value" => 10 * i}
      assert Enum.at(rows, 3) == %{"key" => ["a", "b", "d"], "value" => 10 * i}

      # test group reduce and limit params provide valid json
      query = %{group: true, limit: 2}
      rows = Couch.get(view_url, query: query).body["rows"]
      assert Enum.at(rows, 0) == %{"key" => ["a"], "value" => 20 * i}
      assert length(rows) == 2

      # test group by the first element in the key array
      query = %{group_level: 2}
      rows = Couch.get(view_url, query: query).body["rows"]
      assert Enum.at(rows, 0) == %{"key" => ["a"], "value" => 20 * i}
      assert Enum.at(rows, 1) == %{"key" => ["a", "b"], "value" => 40 * i}
      assert Enum.at(rows, 2) == %{"key" => ["a", "c"], "value" => 10 * i}
      assert Enum.at(rows, 3) == %{"key" => ["d"], "value" => 10 * i}
      assert Enum.at(rows, 4) == %{"key" => ["d", "a"], "value" => 10 * i}
      assert Enum.at(rows, 5) == %{"key" => ["d", "b"], "value" => 10 * i}
      assert Enum.at(rows, 6) == %{"key" => ["d", "c"], "value" => 10 * i}

      # test endkey with inclusive_end=true
      query = %{group_level: 2, endkey: ~s(["d"]), inclusive_end: true}
      rows = Couch.get(view_url, query: query).body["rows"]
      assert Enum.at(rows, 0) == %{"key" => ["a"], "value" => 20 * i}
      assert Enum.at(rows, 1) == %{"key" => ["a", "b"], "value" => 40 * i}
      assert Enum.at(rows, 2) == %{"key" => ["a", "c"], "value" => 10 * i}
      assert Enum.at(rows, 3) == %{"key" => ["d"], "value" => 10 * i}
      assert length(rows) == 4

      # test endkey with inclusive_end=false
      query = %{group_level: 2, endkey: ~s(["d"]), inclusive_end: false}
      rows = Couch.get(view_url, query: query).body["rows"]
      assert Enum.at(rows, 0) == %{"key" => ["a"], "value" => 20 * i}
      assert Enum.at(rows, 1) == %{"key" => ["a", "b"], "value" => 40 * i}
      assert Enum.at(rows, 2) == %{"key" => ["a", "c"], "value" => 10 * i}
      assert length(rows) == 3
    end
  end

  @tag :with_db
  test "More complex reductions that need to use the combine option", context do
    db_name = context[:db_name]
    view_url = "/#{db_name}/_design/foo/_view/bar"
    map = "function (doc) { emit(doc.val, doc.val); };"

    reduce = ~s"""
    function (keys, values, rereduce) {
      // This computes the standard deviation of the mapped results
      var stdDeviation=0.0;
      var count=0;
      var total=0.0;
      var sqrTotal=0.0;

      if (!rereduce) {
        // This is the reduce phase, we are reducing over emitted values from
        // the map functions.
        for(var i in values) {
          total = total + values[i];
          sqrTotal = sqrTotal + (values[i] * values[i]);
        }
        count = values.length;
      } else {
        // This is the rereduce phase, we are re-reducing previosuly
        // reduced values.
        for(var i in values) {
          count = count + values[i].count;
          total = total + values[i].total;
          sqrTotal = sqrTotal + values[i].sqrTotal;
        }
      }

      var variance =  (sqrTotal - ((total * total)/count)) / count;
      stdDeviation = Math.sqrt(variance);

      // the reduce result. It contains enough information to be rereduced
      // with other reduce results.
      return {"stdDeviation":stdDeviation,"count":count,
        "total":total,"sqrTotal":sqrTotal};
    }
    """

    red_doc = %{:views => %{:bar => %{:map => map, :reduce => reduce}}}
    assert Couch.put("/#{db_name}/_design/foo", body: red_doc).body["ok"]

    Enum.each(1..10, fn _ ->
      docs = for i <- 1..10, do: %{val: i * 10}

      assert Couch.post("/#{db_name}/_bulk_docs", body: %{:docs => docs}, query: %{w: 3}).status_code in
               [201, 202]
    end)

    rows = Couch.get(view_url).body["rows"]
    assert_in_delta hd(rows)["value"]["stdDeviation"], 28.722813232690143, 0.0000000001
  end

  @tag :with_db
  test "Reduce pagination", context do
    db_name = context[:db_name]
    view_url = "/#{db_name}/_design/foo/_view/bar"

    ddoc = %{
      _id: "_design/foo",
      language: "javascript",
      views: %{
        bar: %{
          reduce: "_count",
          map: ~s"""
            function(doc) {
               emit(doc.int, doc._id);
               emit(doc.int + 1, doc._id);
               emit(doc.int + 2, doc._id);
            }
          """
        }
      }
    }

    assert Couch.put("/#{db_name}/_design/foo", body: ddoc).body["ok"]
    docs = for i <- 0..1122, do: %{_id: Integer.to_string(i), int: i}

    assert Couch.post("/#{db_name}/_bulk_docs", body: %{:docs => docs}, query: %{w: 3}).status_code in
             [201, 202]


    rand_val = fn -> :rand.uniform(100_000_000) end

    # ?group=false tests
    query = %{startkey: 400, endkey: 402, foobar: rand_val.()}
    rows = Couch.get(view_url, query: query).body["rows"]
    assert hd(rows)["value"] == 9
    query = %{startkey: 402, endkey: 400, foobar: rand_val.(), descending: true}
    rows = Couch.get(view_url, query: query).body["rows"]
    assert hd(rows)["value"] == 9

    query = %{startkey: 400, endkey: 402, foobar: rand_val.(), inclusive_end: false}
    rows = Couch.get(view_url, query: query).body["rows"]
    assert hd(rows)["value"] == 6

    query = %{
      startkey: 402,
      endkey: 400,
      foobar: rand_val.(),
      inclusive_end: false,
      descending: true
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert hd(rows)["value"] == 6

    query = %{startkey: 400, endkey: 402, foobar: rand_val.(), endkey_docid: "400"}
    rows = Couch.get(view_url, query: query).body["rows"]
    assert hd(rows)["value"] == 7

    query = %{
      startkey: 400,
      endkey: 402,
      foobar: rand_val.(),
      endkey_docid: "400",
      inclusive_end: false
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert hd(rows)["value"] == 6

    query = %{startkey: 400, endkey: 402, foobar: rand_val.(), endkey_docid: "401"}
    rows = Couch.get(view_url, query: query).body["rows"]
    assert hd(rows)["value"] == 8

    query = %{
      startkey: 400,
      endkey: 402,
      foobar: rand_val.(),
      endkey_docid: "401",
      inclusive_end: false
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert hd(rows)["value"] == 7

    query = %{startkey: 400, endkey: 402, foobar: rand_val.(), endkey_docid: "402"}
    rows = Couch.get(view_url, query: query).body["rows"]
    assert hd(rows)["value"] == 9

    query = %{
      startkey: 400,
      endkey: 402,
      foobar: rand_val.(),
      endkey_docid: "402",
      inclusive_end: false
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert hd(rows)["value"] == 8

    query = %{
      startkey: 402,
      endkey: 400,
      foobar: rand_val.(),
      endkey_docid: "398",
      descending: true
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert hd(rows)["value"] == 9

    query = %{
      startkey: 402,
      endkey: 400,
      foobar: rand_val.(),
      endkey_docid: "398",
      descending: true,
      inclusive_end: false
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert hd(rows)["value"] == 8

    query = %{
      startkey: 402,
      endkey: 400,
      foobar: rand_val.(),
      endkey_docid: "399",
      descending: true
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert hd(rows)["value"] == 8

    query = %{
      startkey: 402,
      endkey: 400,
      foobar: rand_val.(),
      endkey_docid: "399",
      descending: true,
      inclusive_end: false
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert hd(rows)["value"] == 7

    query = %{
      startkey: 402,
      endkey: 400,
      foobar: rand_val.(),
      endkey_docid: "400",
      descending: true
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert hd(rows)["value"] == 7

    query = %{
      startkey: 402,
      endkey: 400,
      foobar: rand_val.(),
      endkey_docid: "400",
      descending: true,
      inclusive_end: false
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert hd(rows)["value"] == 6

    query = %{
      startkey: 402,
      endkey: 400,
      foobar: rand_val.(),
      startkey_docid: "400",
      descending: true
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert hd(rows)["value"] == 7

    query = %{
      startkey: 402,
      endkey: 400,
      foobar: rand_val.(),
      startkey_docid: "401",
      descending: true,
      inclusive_end: false
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert hd(rows)["value"] == 5

    # ?group=true tests
    query = %{:group => true, startkey: 400, endkey: 402, foobar: rand_val.()}
    rows = Couch.get(view_url, query: query).body["rows"]
    assert length(rows) == 3
    assert Enum.at(rows, 0)["key"] == 400
    assert Enum.at(rows, 0)["value"] == 3
    assert Enum.at(rows, 1)["key"] == 401
    assert Enum.at(rows, 1)["value"] == 3
    assert Enum.at(rows, 2)["key"] == 402
    assert Enum.at(rows, 2)["value"] == 3

    query = %{
      :group => true,
      startkey: 402,
      endkey: 400,
      foobar: rand_val.(),
      descending: true
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert length(rows) == 3
    assert Enum.at(rows, 0)["key"] == 402
    assert Enum.at(rows, 0)["value"] == 3
    assert Enum.at(rows, 1)["key"] == 401
    assert Enum.at(rows, 1)["value"] == 3
    assert Enum.at(rows, 2)["key"] == 400
    assert Enum.at(rows, 2)["value"] == 3

    query = %{
      :group => true,
      startkey: 400,
      endkey: 402,
      foobar: rand_val.(),
      inclusive_end: false
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert length(rows) == 2
    assert Enum.at(rows, 0)["key"] == 400
    assert Enum.at(rows, 0)["value"] == 3
    assert Enum.at(rows, 1)["key"] == 401
    assert Enum.at(rows, 1)["value"] == 3

    query = %{
      :group => true,
      startkey: 402,
      endkey: 400,
      foobar: rand_val.(),
      inclusive_end: false,
      descending: true
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert length(rows) == 2
    assert Enum.at(rows, 0)["key"] == 402
    assert Enum.at(rows, 0)["value"] == 3
    assert Enum.at(rows, 1)["key"] == 401
    assert Enum.at(rows, 1)["value"] == 3

    query = %{
      :group => true,
      startkey: 400,
      endkey: 402,
      foobar: rand_val.(),
      endkey_docid: "401"
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert length(rows) == 3
    assert Enum.at(rows, 0)["key"] == 400
    assert Enum.at(rows, 0)["value"] == 3
    assert Enum.at(rows, 1)["key"] == 401
    assert Enum.at(rows, 1)["value"] == 3
    assert Enum.at(rows, 2)["key"] == 402
    assert Enum.at(rows, 2)["value"] == 2

    query = %{
      :group => true,
      startkey: 400,
      endkey: 402,
      foobar: rand_val.(),
      endkey_docid: "400"
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert length(rows) == 3
    assert Enum.at(rows, 0)["key"] == 400
    assert Enum.at(rows, 0)["value"] == 3
    assert Enum.at(rows, 1)["key"] == 401
    assert Enum.at(rows, 1)["value"] == 3
    assert Enum.at(rows, 2)["key"] == 402
    assert Enum.at(rows, 2)["value"] == 1

    query = %{
      :group => true,
      startkey: 402,
      endkey: 400,
      foobar: rand_val.(),
      startkey_docid: "401",
      descending: true
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert length(rows) == 3
    assert Enum.at(rows, 0)["key"] == 402
    assert Enum.at(rows, 0)["value"] == 2
    assert Enum.at(rows, 1)["key"] == 401
    assert Enum.at(rows, 1)["value"] == 3
    assert Enum.at(rows, 2)["key"] == 400
    assert Enum.at(rows, 2)["value"] == 3

    query = %{
      :group => true,
      startkey: 402,
      endkey: 400,
      foobar: rand_val.(),
      startkey_docid: "400",
      descending: true
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert length(rows) == 3
    assert Enum.at(rows, 0)["key"] == 402
    assert Enum.at(rows, 0)["value"] == 1
    assert Enum.at(rows, 1)["key"] == 401
    assert Enum.at(rows, 1)["value"] == 3
    assert Enum.at(rows, 2)["key"] == 400
    assert Enum.at(rows, 2)["value"] == 3

    query = %{
      :group => true,
      startkey: 402,
      endkey: 400,
      foobar: rand_val.(),
      startkey_docid: "401",
      descending: true,
      inclusive_end: false
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert length(rows) == 2
    assert Enum.at(rows, 0)["key"] == 402
    assert Enum.at(rows, 0)["value"] == 2
    assert Enum.at(rows, 1)["key"] == 401
    assert Enum.at(rows, 1)["value"] == 3

    query = %{
      :group => true,
      startkey: 402,
      endkey: 400,
      foobar: rand_val.(),
      startkey_docid: "400",
      descending: true,
      inclusive_end: false
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert length(rows) == 2
    assert Enum.at(rows, 0)["key"] == 402
    assert Enum.at(rows, 0)["value"] == 1
    assert Enum.at(rows, 1)["key"] == 401
    assert Enum.at(rows, 1)["value"] == 3

    query = %{
      :group => true,
      startkey: 402,
      endkey: 400,
      foobar: rand_val.(),
      endkey_docid: "398",
      descending: true,
      inclusive_end: true
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert length(rows) == 3
    assert Enum.at(rows, 0)["key"] == 402
    assert Enum.at(rows, 0)["value"] == 3
    assert Enum.at(rows, 1)["key"] == 401
    assert Enum.at(rows, 1)["value"] == 3
    assert Enum.at(rows, 2)["key"] == 400
    assert Enum.at(rows, 2)["value"] == 3

    query = %{
      :group => true,
      startkey: 402,
      endkey: 400,
      foobar: rand_val.(),
      endkey_docid: "399",
      descending: true,
      inclusive_end: true
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert length(rows) == 3
    assert Enum.at(rows, 0)["key"] == 402
    assert Enum.at(rows, 0)["value"] == 3
    assert Enum.at(rows, 1)["key"] == 401
    assert Enum.at(rows, 1)["value"] == 3
    assert Enum.at(rows, 2)["key"] == 400
    assert Enum.at(rows, 2)["value"] == 2

    query = %{
      :group => true,
      startkey: 402,
      endkey: 400,
      foobar: rand_val.(),
      endkey_docid: "399",
      descending: true,
      inclusive_end: false
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert length(rows) == 3
    assert Enum.at(rows, 0)["key"] == 402
    assert Enum.at(rows, 0)["value"] == 3
    assert Enum.at(rows, 1)["key"] == 401
    assert Enum.at(rows, 1)["value"] == 3
    assert Enum.at(rows, 2)["key"] == 400
    assert Enum.at(rows, 2)["value"] == 1

    query = %{
      :group => true,
      startkey: 402,
      endkey: 400,
      foobar: rand_val.(),
      endkey_docid: "400",
      descending: true,
      inclusive_end: false
    }

    rows = Couch.get(view_url, query: query).body["rows"]
    assert length(rows) == 2
    assert Enum.at(rows, 0)["key"] == 402
    assert Enum.at(rows, 0)["value"] == 3
    assert Enum.at(rows, 1)["key"] == 401
    assert Enum.at(rows, 1)["value"] == 3
  end
end

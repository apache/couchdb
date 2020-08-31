defmodule ReduceBuiltinTest do
  use CouchTestCase

  @moduletag :views
  @moduletag kind: :single_node

  @moduledoc """
  Test CouchDB view builtin reduce functions
  This is a port of the reduce_builtin.js suite
  """

  def random_ddoc(db_name) do
    "/#{db_name}/_design/#{:erlang.monotonic_time()}"
  end

  def summate(n) do
    (n + 1) * n / 2
  end

  def sumsqr(n) do
    1..n |> Enum.reduce(0, fn i, acc -> acc + i * i end)
  end

  def check_approx_distinct(expected, estimated) do
    # see https://en.wikipedia.org/wiki/HyperLogLog
    err = 1.04 / :math.sqrt(:math.pow(2, 11 - 1))
    abs(expected - estimated) < expected * err
  end

  def query_rows(ddoc_url, builtin_fun, query \\ nil) do
    http_opts = if query, do: [query: query], else: []
    Couch.get("#{ddoc_url}/_view/builtin#{builtin_fun}", http_opts).body["rows"]
  end

  def query_value(ddoc_url, builtin_fun, query \\ nil) do
    hd(query_rows(ddoc_url, builtin_fun, query))["value"]
  end

  @tag :with_db
  test "Builtin reduce functions", context do
    db_name = context[:db_name]
    num_docs = 500

    docs = make_docs(1..num_docs)

    resp = Couch.post("/#{db_name}/_bulk_docs", body: %{:docs => docs}, query: %{w: 3})
    assert resp.status_code in [201, 202]

    ddoc_url = random_ddoc(db_name)

    map = ~s"""
    function (doc) {
      emit(doc.integer, doc.integer);
      emit(doc.integer, doc.integer);
    };
    """

    design_doc = %{
      :views => %{
        :builtin_sum => %{:map => map, :reduce => "_sum"},
        :builtin_count => %{:map => map, :reduce => "_count"},
        :builtin_stats => %{:map => map, :reduce => "_stats"},
        :builtin_approx_count_distinct => %{
          :map => map,
          :reduce => "_approx_count_distinct"
        }
      }
    }

    assert Couch.put(ddoc_url, body: design_doc).body["ok"]

    value = ddoc_url |> query_value("_sum")
    assert value == 2 * summate(num_docs)
    value = ddoc_url |> query_value("_count")
    assert value == 1000
    value = ddoc_url |> query_value("_stats")
    assert value["sum"] == 2 * summate(num_docs)
    assert value["count"] == 1000
    assert value["min"] == 1
    assert value["max"] == 500
    assert value["sumsqr"] == 2 * sumsqr(num_docs)
    value = ddoc_url |> query_value("_approx_count_distinct")
    assert check_approx_distinct(num_docs, value)

    value = ddoc_url |> query_value("_sum", %{startkey: 4, endkey: 4})
    assert value == 8
    value = ddoc_url |> query_value("_count", %{startkey: 4, endkey: 4})
    assert value == 2
    value = ddoc_url |> query_value("_approx_count_distinct", %{startkey: 4, endkey: 4})
    assert check_approx_distinct(1, value)

    value = ddoc_url |> query_value("_sum", %{startkey: 4, endkey: 5})
    assert value == 18
    value = ddoc_url |> query_value("_count", %{startkey: 4, endkey: 5})
    assert value == 4
    value = ddoc_url |> query_value("_approx_count_distinct", %{startkey: 4, endkey: 5})
    assert check_approx_distinct(2, value)

    value = ddoc_url |> query_value("_sum", %{startkey: 4, endkey: 6})
    assert value == 30
    value = ddoc_url |> query_value("_count", %{startkey: 4, endkey: 6})
    assert value == 6
    value = ddoc_url |> query_value("_approx_count_distinct", %{startkey: 4, endkey: 6})
    assert check_approx_distinct(3, value)

    assert [row0, row1, row2] = ddoc_url |> query_rows("_sum", %{group: true, limit: 3})
    assert row0["value"] == 2
    assert row1["value"] == 4
    assert row2["value"] == 6

    assert [row0, row1, row2] =
             ddoc_url |> query_rows("_approx_count_distinct", %{group: true, limit: 3})

    assert check_approx_distinct(1, row0["value"])
    assert check_approx_distinct(1, row1["value"])
    assert check_approx_distinct(1, row2["value"])

    1..div(500, 2)
    |> Enum.take_every(30)
    |> Enum.each(fn i ->
      value = ddoc_url |> query_value("_sum", %{startkey: i, endkey: num_docs - i})
      assert value == 2 * (summate(num_docs - i) - summate(i - 1))
    end)
  end

  @tag :with_db
  test "Builtin reduce functions with trailings", context do
    db_name = context[:db_name]
    num_docs = 500

    docs = make_docs(1..num_docs)

    resp = Couch.post("/#{db_name}/_bulk_docs", body: %{:docs => docs}, query: %{w: 3})
    assert resp.status_code in [201, 202]

    # test for trailing characters after builtin functions, desired behaviour
    # is to disregard any trailing characters
    # I think the behavior should be a prefix test, so that even "_statsorama"
    # or "_stats\nare\awesome" should work just as "_stats" does. - JChris
    ["\n", "orama", "\nare\nawesome", " ", "     \n  "]
    |> Enum.each(fn trailing ->
      ddoc_url = random_ddoc(db_name)

      map = ~s"""
      function (doc) {
        emit(doc.integer, doc.integer);
        emit(doc.integer, doc.integer);
      };
      """

      design_doc = %{
        :views => %{
          :builtin_sum => %{:map => map, :reduce => "_sum#{trailing}"},
          :builtin_count => %{:map => map, :reduce => "_count#{trailing}"},
          :builtin_stats => %{:map => map, :reduce => "_stats#{trailing}"},
          :builtin_approx_count_distinct => %{
            :map => map,
            :reduce => "_approx_count_distinct#{trailing}"
          }
        }
      }

      assert Couch.put(ddoc_url, body: design_doc).body["ok"]

      value = ddoc_url |> query_value("_sum")
      assert value == 2 * summate(num_docs)
      value = ddoc_url |> query_value("_count")
      assert value == 1000
      value = ddoc_url |> query_value("_stats")
      assert value["sum"] == 2 * summate(num_docs)
      assert value["count"] == 1000
      assert value["min"] == 1
      assert value["max"] == 500
      assert value["sumsqr"] == 2 * sumsqr(num_docs)
    end)
  end

  @tag :with_db
  test "Builtin count and sum reduce for key as array", context do
    db_name = context[:db_name]

    ddoc_url = random_ddoc(db_name)

    map_one = ~s"""
    function (doc) {
      emit(doc.keys, 1);
    };
    """

    map_ones_array = ~s"""
    function (doc) {
      emit(doc.keys, [1, 1]);
    };
    """

    design_doc = %{
      :views => %{
        :builtin_one_sum => %{:map => map_one, :reduce => "_sum"},
        :builtin_one_count => %{:map => map_one, :reduce => "_count"},
        :builtin_ones_array_sum => %{:map => map_ones_array, :reduce => "_sum"}
      }
    }

    assert Couch.put(ddoc_url, body: design_doc).body["ok"]

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

        resp = Couch.post("/#{db_name}/_bulk_docs", body: %{docs: docs}, query: %{w: 3})
        assert resp.status_code in [201, 202]

        total_docs = 1 + (i - 1) * 10 * 11 + (j + 1) * 11
        assert Couch.get("/#{db_name}").body["doc_count"] == total_docs
      end

      ["_sum", "_count"]
      |> Enum.each(fn builtin ->
        builtin = "_one#{builtin}"

        # group by exact key match
        rows = query_rows(ddoc_url, builtin, %{group: true})
        assert Enum.at(rows, 0) == %{"key" => ["a"], "value" => 20 * i}
        assert Enum.at(rows, 1) == %{"key" => ["a", "b"], "value" => 20 * i}
        assert Enum.at(rows, 2) == %{"key" => ["a", "b", "c"], "value" => 10 * i}
        assert Enum.at(rows, 3) == %{"key" => ["a", "b", "d"], "value" => 10 * i}

        # make sure group reduce and limit params provide valid json
        assert [row0, _] = query_rows(ddoc_url, builtin, %{group: true, limit: 2})
        assert row0 == %{"key" => ["a"], "value" => 20 * i}

        # group by the first element in the key array
        rows = query_rows(ddoc_url, builtin, %{group_level: 1})
        assert Enum.at(rows, 0) == %{"key" => ["a"], "value" => 70 * i}
        assert Enum.at(rows, 1) == %{"key" => ["d"], "value" => 40 * i}

        # group by the first 2 elements in the key array
        rows = query_rows(ddoc_url, builtin, %{group_level: 2})
        assert Enum.at(rows, 0) == %{"key" => ["a"], "value" => 20 * i}
        assert Enum.at(rows, 1) == %{"key" => ["a", "b"], "value" => 40 * i}
        assert Enum.at(rows, 2) == %{"key" => ["a", "c"], "value" => 10 * i}
        assert Enum.at(rows, 3) == %{"key" => ["d"], "value" => 10 * i}
        assert Enum.at(rows, 4) == %{"key" => ["d", "a"], "value" => 10 * i}
        assert Enum.at(rows, 5) == %{"key" => ["d", "b"], "value" => 10 * i}
        assert Enum.at(rows, 6) == %{"key" => ["d", "c"], "value" => 10 * i}
      end)

      rows = query_rows(ddoc_url, "_ones_array_sum", %{group: true})
      assert Enum.at(rows, 0) == %{"key" => ["a"], "value" => [20 * i, 20 * i]}
      assert Enum.at(rows, 1) == %{"key" => ["a", "b"], "value" => [20 * i, 20 * i]}
      assert Enum.at(rows, 2) == %{"key" => ["a", "b", "c"], "value" => [10 * i, 10 * i]}
      assert Enum.at(rows, 3) == %{"key" => ["a", "b", "d"], "value" => [10 * i, 10 * i]}

      assert [row0, _] = query_rows(ddoc_url, "_ones_array_sum", %{group: true, limit: 2})
      assert row0 == %{"key" => ["a"], "value" => [20 * i, 20 * i]}

      rows = query_rows(ddoc_url, "_ones_array_sum", %{group_level: 1})
      assert Enum.at(rows, 0) == %{"key" => ["a"], "value" => [70 * i, 70 * i]}
      assert Enum.at(rows, 1) == %{"key" => ["d"], "value" => [40 * i, 40 * i]}

      rows = query_rows(ddoc_url, "_ones_array_sum", %{group_level: 2})
      assert Enum.at(rows, 0) == %{"key" => ["a"], "value" => [20 * i, 20 * i]}
      assert Enum.at(rows, 1) == %{"key" => ["a", "b"], "value" => [40 * i, 40 * i]}
      assert Enum.at(rows, 2) == %{"key" => ["a", "c"], "value" => [10 * i, 10 * i]}
      assert Enum.at(rows, 3) == %{"key" => ["d"], "value" => [10 * i, 10 * i]}
      assert Enum.at(rows, 4) == %{"key" => ["d", "a"], "value" => [10 * i, 10 * i]}
      assert Enum.at(rows, 5) == %{"key" => ["d", "b"], "value" => [10 * i, 10 * i]}
      assert Enum.at(rows, 6) == %{"key" => ["d", "c"], "value" => [10 * i, 10 * i]}
    end
  end
end

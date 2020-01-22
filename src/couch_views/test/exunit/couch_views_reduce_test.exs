defmodule CouchViewsReduceTest do
  use Couch.Test.ExUnit.Case

  alias Couch.Test.Utils
  alias Couch.Test.Setup
  alias Couch.Test.Setup.Step

  setup_all do
    test_ctx =
      :test_util.start_couch([:couch_log, :fabric, :couch_js, :couch_views, :couch_jobs])

    on_exit(fn ->
      :test_util.stop_couch(test_ctx)
    end)
  end

  setup do
    db_name = Utils.random_name("db")

    admin_ctx =
      {:user_ctx,
       Utils.erlang_record(:user_ctx, "couch/include/couch_db.hrl", roles: ["_admin"])}

    {:ok, db} = :fabric2_db.create(db_name, [admin_ctx])

    docs = create_docs()
    ddoc = create_ddoc()

    {ok, _} = :fabric2_db.update_docs(db, [ddoc | docs])

    on_exit(fn ->
      :fabric2_db.delete(db_name, [admin_ctx])
    end)

    %{
      :db_name => db_name,
      :db => db,
      :ddoc => ddoc
    }
  end

  test "group_level=0 reduce startkey/endkey", context do
    args = %{
      reduce: true,
      group_level: 0,
      start_key: [2018, 3, 2],
      end_key: [2019, 5, 1]
    }

    correct = [
      {:row, [key: :null, value: 31]}
    ]

    run_query(context, args, "dates_sum", correct)
  end

  test "group_level=0 reduce", context do
    args = %{
      reduce: true,
      group_level: 0
    }

    correct = [
      {:row, [key: :null, value: 68]}
    ]

    run_query(context, args, "dates_sum", correct)
  end

  test "group_level=1 reduce", context do
    args = %{
      reduce: true,
      group_level: 1
    }

    correct = [
      {:row, [key: [2017], value: 31]},
      {:row, [key: [2018], value: 20]},
      {:row, [key: [2019], value: 17]}
    ]

    run_query(context, args, "dates_sum", correct)
  end

  test "group_level=1 reduce with startkey/endkey", context do
    args = %{
      reduce: true,
      group_level: 1,
      start_key: [2017, 4, 1],
      end_key: [2018, 3, 1]
    }

    correct = [
      {:row, [key: [2017], value: 22]},
      {:row, [key: [2018], value: 6]}
    ]

    run_query(context, args, "dates_sum", correct)
  end

  test "group_level=1 reduce with startkey/endkey take 2", context do
    args = %{
      reduce: true,
      group_level: 1,
      start_key: [2017, 4, 1],
      end_key: [2019, 3, 2]
    }

    correct = [
      {:row, [key: [2017], value: 22]},
      {:row, [key: [2018], value: 20]},
      {:row, [key: [2019], value: 4]}
    ]

    run_query(context, args, "dates_sum", correct)
  end

  test "group_level=1 reduce with startkey/endkey take 3", context do
    args = %{
      reduce: true,
      group_level: 1,
      start_key: [2017, 4, 1],
      end_key: [2019, 05, 1]
    }

    correct = [
      {:row, [key: [2017], value: 22]},
      {:row, [key: [2018], value: 20]},
      {:row, [key: [2019], value: 17]}
    ]

    run_query(context, args, "dates_sum", correct)
  end

  test "group_level=1 reduce with startkey", context do
    args = %{
      reduce: true,
      group_level: 1,
      start_key: [2017, 4, 1]
    }

    correct = [
      {:row, [key: [2017], value: 22]},
      {:row, [key: [2018], value: 20]},
      {:row, [key: [2019], value: 17]}
    ]

    run_query(context, args, "dates_sum", correct)
  end

  test "group_level=1 reduce with endkey", context do
    args = %{
      reduce: true,
      group_level: 1,
      end_key: [2018, 5, 2]
    }

    correct = [
      {:row, [key: [2017], value: 31]},
      {:row, [key: [2018], value: 20]}
    ]

    run_query(context, args, "dates_sum", correct)
  end

  test "group=true reduce with startkey/endkey", context do
    args = %{
      reduce: true,
      group: true,
      start_key: [2018, 5, 1],
      end_key: [2019, 04, 1]
    }

    correct = [
      {:row, [key: [2018, 5, 1], value: 7]},
      {:row, [key: [2019, 3, 1], value: 4]},
      {:row, [key: [2019, 4, 1], value: 6]}
    ]

    run_query(context, args, "dates_sum", correct)
  end

  test "mixed count reduce group_level=1", context do
    args = %{
      reduce: true,
      group_level: 1,
      limit: 6
    }

    correct = [
      {:row, [key: 1, value: 2]},
      {:row, [key: 2, value: 2]},
      {:row, [key: 3, value: 2]},
      {:row, [key: [1], value: 3]},
      {:row, [key: [2], value: 2]},
      {:row, [key: [3], value: 3]}
    ]

    run_query(context, args, "count", correct)
  end

  test "mixed count reduce group_level=2", context do
    args = %{
      :reduce => true,
      :group_level => 2,
      :limit => 9
    }

    correct = [
      {:row, [key: 1, value: 2]},
      {:row, [key: 2, value: 2]},
      {:row, [key: 3, value: 2]},
      {:row, [key: [1, 1], value: 2]},
      {:row, [key: [1, 2], value: 1]},
      {:row, [key: [2, 1], value: 1]},
      {:row, [key: [2, 3], value: 1]},
      {:row, [key: [3, 1], value: 2]},
      {:row, [key: [3, 4], value: 1]}
    ]

    run_query(context, args, "count", correct)
  end

  test "mixed _count reduce group=2 reduce with startkey/endkey", context do
    args = %{
      reduce: true,
      group_level: 2,
      start_key: 3,
      end_key: [3, 1]
    }

    correct = [
      {:row, [key: 3, value: 2]},
      {:row, [key: [1, 1], value: 2]},
      {:row, [key: [1, 2], value: 1]},
      {:row, [key: [2, 1], value: 1]},
      {:row, [key: [2, 3], value: 1]},
      {:row, [key: [3, 1], value: 1]}
    ]

    run_query(context, args, "count", correct)
  end

  test "mixed _count reduce group=2 reduce with startkey/endkey direction = rev",
       context do
    args = %{
      reduce: true,
      group_level: 2,
      start_key: [3, 1],
      end_key: [1, 1],
      direction: :rev
    }

    correct = [
      {:row, [key: [3, 1], value: 1]},
      {:row, [key: [2, 3], value: 1]},
      {:row, [key: [2, 1], value: 1]},
      {:row, [key: [1, 2], value: 1]},
      {:row, [key: [1, 1], value: 2]}
    ]

    run_query(context, args, "count", correct)

    args1 = %{
      reduce: true,
      group_level: 2,
      start_key: [3, 1],
      direction: :rev
    }

    correct1 = [
      {:row, [key: [3, 1], value: 1]},
      {:row, [key: [2, 3], value: 1]},
      {:row, [key: [2, 1], value: 1]},
      {:row, [key: [1, 2], value: 1]},
      {:row, [key: [1, 1], value: 2]},
      {:row, [key: 3, value: 2]},
      {:row, [key: 2, value: 2]},
      {:row, [key: 1, value: 2]}
    ]

    run_query(context, args1, "count", correct1)

    args2 = %{
      reduce: true,
      group_level: 2,
      end_key: [1, 1],
      direction: :rev
    }

    correct2 = [
      {:row, [key: [3, 4], value: 1]},
      {:row, [key: [3, 1], value: 2]},
      {:row, [key: [2, 3], value: 1]},
      {:row, [key: [2, 1], value: 1]},
      {:row, [key: [1, 2], value: 1]},
      {:row, [key: [1, 1], value: 2]}
    ]

    run_query(context, args2, "count", correct2)
  end

  test "mixed _count reduce group=2 reduce with skip", context do
    args = %{
      reduce: true,
      group_level: 2,
      start_key: 3,
      skip: 2,
      end_key: [3, 1]
    }

    correct = [
      {:row, [key: [1, 2], value: 1]},
      {:row, [key: [2, 1], value: 1]},
      {:row, [key: [2, 3], value: 1]},
      {:row, [key: [3, 1], value: 1]}
    ]

    run_query(context, args, "count", correct)
  end

  test "mixed _count reduce group=2 reduce inclusive_end = false", context do
    args = %{
      reduce: true,
      group_level: 2,
      start_key: [1, 1],
      end_key: [3, 1],
      inclusive_end: false
    }

    correct = [
      {:row, [key: [1, 1], value: 2]},
      {:row, [key: [1, 2], value: 1]},
      {:row, [key: [2, 1], value: 1]},
      {:row, [key: [2, 3], value: 1]}
    ]

    run_query(context, args, "count", correct)

    args1 = %{
      reduce: true,
      group_level: 2,
      start_key: [3, 1],
      end_key: [1, 1],
      direction: :rev,
      inclusive_end: false
    }

    correct1 = [
      {:row, [key: [3, 1], value: 1]},
      {:row, [key: [2, 3], value: 1]},
      {:row, [key: [2, 1], value: 1]},
      {:row, [key: [1, 2], value: 1]},
      {:row, [key: [1, 1], value: 1]}
    ]

    run_query(context, args1, "count", correct1)
  end

  test "strings count reduce group_level=1", context do
    args = %{
      reduce: true,
      group_level: 1,
      start_key: "4"
    }

    correct = [
      {:row, [key: "5", value: 1]},
      {:row, [key: "6", value: 1]},
      {:row, [key: "7", value: 1]},
      {:row, [key: "8", value: 2]},
      {:row, [key: "9", value: 1]}
    ]

    run_query(context, args, "count_strings", correct)
  end

  test "_stats reduce works", context do
    args = %{
      reduce: true,
      group_level: 1
    }

    correct = [
      {:row,
       [
         key: [2017],
         value: %{"sum" => 31, "count" => 4, "min" => 6, "max" => 9, "sumsqr" => 247}
       ]},
      {:row,
       [
         key: [2018],
         value: %{"sum" => 20, "count" => 4, "min" => 3, "max" => 7, "sumsqr" => 110}
       ]},
      {:row,
       [
         key: [2019],
         value: %{"sum" => 17, "count" => 3, "min" => 4, "max" => 7, "sumsqr" => 101}
       ]}
    ]

    run_query(context, args, "stats", correct)
  end

  test "_approx_count_distinct reduce works", context do
    args = %{
      reduce: true,
      group_level: 1
    }

    correct = [
      {:row, [key: [2017], value: 4]},
      {:row, [key: [2018], value: 3]},
      {:row, [key: [2019], value: 3]}
    ]

    run_query(context, args, "distinct", correct)
  end

  @tag :skip
  test "generate random keys to test skiplist creation", context do
    kvs =
      for i <- 1..11 do
        year = Enum.random(2019..2020)
        month = Enum.random(1..3)
        day = Enum.random(1..3)
        val = Enum.random(1..3)
        {[year, month, day], val}
      end

    db = context[:db]
    sig = <<"sig">>
    view_id = <<123>>

    :fabric2_fdb.transactional(db, fn txdb ->
      :couch_views_reduce_fdb.create_reduce_indexes(txdb, sig, [view_id])

      Enum.each(kvs, fn kv ->
        :couch_views_reduce_fdb.update_reduce_idx(
          txdb,
          sig,
          view_id,
          <<"_sum">>,
          "",
          [],
          [kv]
        )
      end)
    end)
  end

  defp run_query(context, args, view, correct_resp) do
    db = context[:db]
    ddoc = context[:ddoc]

    {:ok, res1} = :couch_views.query(db, ddoc, view, &__MODULE__.default_cb/2, [], args)
    assert(res1 == correct_resp, "failed for skiplist")

    args = Map.put(args, :use_skiplist, false)
    {:ok, res2} = :couch_views.query(db, ddoc, view, &__MODULE__.default_cb/2, [], args)
    assert(res2 == correct_resp, "failed for fold level 0")
  end

  def default_cb(:complete, acc) do
    {:ok, Enum.reverse(acc)}
  end

  def default_cb({:final, info}, []) do
    {:ok, [info]}
  end

  def default_cb({:final, _}, acc) do
    {:ok, acc}
  end

  def default_cb({:meta, _}, acc) do
    {:ok, acc}
  end

  def default_cb(:ok, :ddoc_updated) do
    {:ok, :ddoc_updated}
  end

  def default_cb(row, acc) do
    {:ok, [row | acc]}
  end

  defp create_docs() do
    dates = [
      {[2017, 3, 1], 9},
      {[2017, 4, 1], 7},
      # out of order check
      {[2019, 3, 1], 4},
      {[2017, 4, 15], 6},
      {[2018, 4, 1], 3},
      {[2017, 5, 1], 9},
      {[2018, 3, 1], 6},
      # duplicate check
      {[2018, 4, 1], 4},
      {[2018, 5, 1], 7},
      {[2019, 4, 1], 6},
      {[2019, 5, 1], 7}
    ]

    for i <- 1..11 do
      group =
        if rem(i, 3) == 0 do
          "first"
        else
          "second"
        end

      {date_key, date_val} = Enum.at(dates, i - 1)

      val =
        if i == 4 do
          8
        else
          i
        end

      :couch_doc.from_json_obj(
        {[
           {"_id", "doc-id-#{i}"},
           {"value", i},
           {"some", "field"},
           {"group", group},
           {"date", date_key},
           {"date_val", date_val},
           {"random_val", val}
         ]}
      )
    end
  end

  defp create_ddoc() do
    views = %{
      "_id" => "_design/bar",
      "views" => %{
        "dates_sum" => %{
          "map" => """

            function(doc) {
              emit(doc.date, doc.date_val);
            }
          """,
          "reduce" => "_sum"
        },
        "count_strings" => %{
          "map" => """
            function(doc) {
              emit(doc.random_val.toString(), 1);
            }
          """,
          "reduce" => "_count"
        },
        "count" => %{
          "map" => """
            function(doc) {
              if (doc.value > 3) {
                return;
              }
              emit(doc.value, doc.value);
              emit(doc.value, doc.value);
              emit([doc.value, 1], doc.value);
              emit([doc.value, doc.value + 1, doc.group.length], doc.value);

              if (doc.value === 3) {
                emit([1, 1, 5], 1);
                emit([doc.value, 1, 5], 1);
              }
            }
          """,
          "reduce" => "_count"
        },
        "distinct" => %{
          "map" => """
            function(doc) {
                emit(doc.date, doc.date_val);
            }
          """,
          "reduce" => "_approx_count_distinct"
        },
        "stats" => %{
          "map" => """
            function(doc) {
                emit(doc.date, doc.date_val);
            }
          """,
          "reduce" => "_stats"
        },
        "no_reduce" => %{
          "map" => """
            function (doc) {
              emit(doc._id, doc.value);
            }
          """
        }
      }
    }

    :couch_doc.from_json_obj(:jiffy.decode(:jiffy.encode(views)))
  end
end

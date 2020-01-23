defmodule MangoIndexerTest do
  use Couch.Test.ExUnit.Case

  alias Couch.Test.Utils
  alias Couch.Test.Setup
  alias Couch.Test.Setup.Step

  setup_all do
    test_ctx = :test_util.start_couch([:couch_log, :fabric, :couch_js, :couch_jobs])

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

    ddocs = create_ddocs()
    idx_ddocs = create_indexes(db)
    docs = create_docs()

    IO.inspect idx_ddocs
    {ok, _} = :fabric2_db.update_docs(db, ddocs ++ idx_ddocs)
    {ok, _} = :fabric2_db.update_docs(db, docs)

    on_exit(fn ->
      :fabric2_db.delete(db_name, [admin_ctx])
    end)

    %{
      db_name: db_name,
      db: db,
      ddoc: ddocs,
      idx: idx_ddocs
    }
  end

  test "create design doc through _index", context do
    db = context[:db]
  end

  defp create_indexes(db) do
    opts = [
      {:def, {[{"fields", ["group", "value"]}]}},
      {:type, "json"},
      {:name, "idx_01"},
      {:ddoc, :auto_name},
      {:w, 3},
      {:partitioned, :db_default}
    ]

    {:ok, idx} = :mango_idx.new(db, opts)
    db_opts = [{:user_ctx, db["user_ctx"]}, :deleted, :ejson_body]
    {:ok, ddoc} = :mango_util.load_ddoc(db, :mango_idx.ddoc(idx), db_opts)
    {:ok ,new_ddoc} = :mango_idx.add(ddoc, idx)
    [new_ddoc]
  end

  #    Create 1 design doc that should be filtered out and ignored
  defp create_ddocs() do
    views = %{
      "_id" => "_design/bar",
      "views" => %{
        "dates_sum" => %{
          "map" => """
                function(doc) {
                    if (doc.date) {
                        emit(doc.date, doc.date_val);
                    }
                }
          """
        }
      }
    }

    ddoc1 = :couch_doc.from_json_obj(:jiffy.decode(:jiffy.encode(views)))
    []
  end

  defp create_docs() do
    for i <- 1..1 do
      group =
        if rem(i, 3) == 0 do
          "first"
        else
          "second"
        end

      :couch_doc.from_json_obj(
        {[
          {"_id", "doc-id-#{i}"},
          {"value", i},
          {"val_str", Integer.to_string(i, 8)},
          {"some", "field"},
          {"group", group}
        ]}
      )
    end
  end
end

defmodule MangoIndexerTest do
    use Couch.Test.ExUnit.Case

    alias Couch.Test.Utils
    alias Couch.Test.Setup
    alias Couch.Test.Setup.Step

    setup_all do
        test_ctx =
          :test_util.start_couch([:couch_log, :fabric, :couch_js, :couch_jobs])

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

    test "create design doc through _index", context do
        db = context[:db]
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
        :couch_doc.from_json_obj(:jiffy.decode(:jiffy.encode(views)))
    end

    defp create_docs() do
        []
    end
end
defmodule ViewConflictsTest do
  use CouchTestCase

  @moduletag kind: :single_node

  setup_all do
    db_name_a = random_db_name()
    db_name_b = random_db_name()

    {:ok, _} = create_db(db_name_a)
    {:ok, _} = create_db(db_name_b)

    on_exit(fn -> delete_db(db_name_a) end)
    on_exit(fn -> delete_db(db_name_b) end)
    {:ok, [db_name_a: db_name_a, db_name_b: db_name_b]}
  end

  test "view conflict", context do
    db_name_a = context[:db_name_a]
    db_name_b = context[:db_name_b]

    create_doc(db_name_a, %{_id: "foo", bar: 42})
    replicate(db_name_a, db_name_b)

    resp = Couch.get("/#{db_name_b}/foo")

    docb =
      resp.body
      |> Map.put("bar", 43)

    docb = save(db_name_b, docb)

    resp = Couch.get("/#{db_name_a}/foo")

    doca =
      resp.body
      |> Map.put("bar", 41)

    doca = save(db_name_a, doca)

    replicate(db_name_a, db_name_b)

    resp = Couch.get("/#{db_name_b}/foo", query: [conflicts: true])
    doc = resp.body
    assert length(resp.body["_conflicts"]) == 1

    conflict_rev = Enum.at(resp.body["_conflicts"], 0)

    case doc["bar"] do
      41 -> assert conflict_rev == docb["_rev"]
      43 -> assert conflict_rev == doca["_rev"]
      _ -> assert false
    end

    map_fun = """
    function(doc) {
      if (doc._conflicts) {
        emit(doc._id, doc._conflicts);
      }
    }
    """

    results = query(db_name_b, map_fun)

    rev =
      results
      |> Map.get("rows")
      |> Enum.at(0)
      |> Map.get("value")
      |> Enum.at(0)

    assert conflict_rev == rev
  end
end

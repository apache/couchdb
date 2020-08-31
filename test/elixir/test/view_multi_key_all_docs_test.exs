defmodule ViewMultiKeyAllDocsTest do
  use CouchTestCase

  @moduletag kind: :single_node

  @keys ["10", "15", "30", "37", "50"]

  setup_all do
    db_name = random_db_name()
    {:ok, _} = create_db(db_name)
    on_exit(fn -> delete_db(db_name) end)

    bulk_save(db_name, make_docs(0..99))

    {:ok, [db_name: db_name]}
  end

  test "keys in POST body", context do
    db_name = context[:db_name]

    resp = all_docs(db_name, nil, @keys)
    assert resp.status_code == 200
    rows = resp.body["rows"]
    assert length(rows) == length(@keys)

    rows_id = Enum.map(rows, & &1["id"])
    assert rows_id == @keys
  end

  test "keys in GET parameters", context do
    db_name = context[:db_name]
    resp = all_docs(db_name, keys: :jiffy.encode(@keys))
    assert resp.status_code == 200
    rows = resp.body["rows"]
    assert length(rows) == length(@keys)
    rows_id = Enum.map(rows, & &1["id"])
    assert rows_id == @keys
  end

  test "keys in POST body (limit)", context do
    db_name = context[:db_name]

    resp = all_docs(db_name, [limit: 1], @keys)
    assert resp.status_code == 200
    rows = resp.body["rows"]
    assert length(rows) == 1
    assert Enum.at(rows, 0)["id"] == Enum.at(@keys, 0)
  end

  test "keys in GET parameters (limit)", context do
    db_name = context[:db_name]
    resp = all_docs(db_name, limit: 1, keys: :jiffy.encode(@keys))
    assert resp.status_code == 200
    rows = resp.body["rows"]
    assert length(rows) == 1
    assert Enum.at(rows, 0)["id"] == Enum.at(@keys, 0)
  end

  test "keys in POST body (skip)", context do
    db_name = context[:db_name]

    resp = all_docs(db_name, [skip: 2], @keys)
    assert resp.status_code == 200
    rows = resp.body["rows"]
    assert length(rows) == 3

    rows_id = Enum.map(rows, & &1["id"])
    assert rows_id == Enum.drop(@keys, 2)
  end

  test "keys in GET parameters (skip)", context do
    db_name = context[:db_name]
    resp = all_docs(db_name, skip: 2, keys: :jiffy.encode(@keys))
    assert resp.status_code == 200
    rows = resp.body["rows"]
    assert length(rows) == 3
    rows_id = Enum.map(rows, & &1["id"])
    assert rows_id == Enum.drop(@keys, 2)
  end

  test "keys in POST body (descending)", context do
    db_name = context[:db_name]

    resp = all_docs(db_name, [descending: true], @keys)
    assert resp.status_code == 200
    rows = resp.body["rows"]
    assert length(rows) == length(@keys)

    rows_id = Enum.map(rows, & &1["id"])
    assert rows_id == Enum.reverse(@keys)
  end

  test "keys in GET parameters (descending)", context do
    db_name = context[:db_name]
    resp = all_docs(db_name, descending: true, keys: :jiffy.encode(@keys))
    assert resp.status_code == 200
    rows = resp.body["rows"]
    assert length(rows) == length(@keys)
    rows_id = Enum.map(rows, & &1["id"])
    assert rows_id == Enum.reverse(@keys)
  end

  test "keys in POST body (descending, skip, limit)", context do
    db_name = context[:db_name]

    resp = all_docs(db_name, [descending: "true", skip: 3, limit: 1], @keys)
    assert resp.status_code == 200
    rows = resp.body["rows"]
    assert length(rows) == 1

    key =
      @keys
      |> Enum.reverse()
      |> Enum.drop(3)
      |> Enum.at(0)

    assert Enum.at(rows, 0)["id"] == key
  end

  test "keys in GET parameters (descending, skip, limit)", context do
    db_name = context[:db_name]

    resp =
      all_docs(db_name, descending: "true", skip: 3, limit: 1, keys: :jiffy.encode(@keys))

    assert resp.status_code == 200
    rows = resp.body["rows"]
    assert length(rows) == 1

    key =
      @keys
      |> Enum.reverse()
      |> Enum.drop(3)
      |> Enum.at(0)

    assert Enum.at(rows, 0)["id"] == key
  end

  test "POST - get invalid rows when the key doesn't exist", context do
    db_name = context[:db_name]

    resp = all_docs(db_name, nil, ["1211", "i_dont_exist", "0"])
    assert resp.status_code == 200
    rows = resp.body["rows"]
    assert length(rows) == 3
    assert Enum.at(rows, 0)["error"] == "not_found"
    assert not Map.has_key?(Enum.at(rows, 0), "id")
    assert Enum.at(rows, 1)["error"] == "not_found"
    assert not Map.has_key?(Enum.at(rows, 1), "id")
    assert Enum.at(rows, 2)["id"] == Enum.at(rows, 2)["key"]
    assert Enum.at(rows, 2)["key"] == "0"
  end

  test "GET - get invalid rows when the key doesn't exist", context do
    db_name = context[:db_name]

    resp = all_docs(db_name, keys: :jiffy.encode(["1211", "i_dont_exist", "0"]))
    assert resp.status_code == 200
    rows = resp.body["rows"]
    assert length(rows) == 3
    assert Enum.at(rows, 0)["error"] == "not_found"
    assert not Map.has_key?(Enum.at(rows, 0), "id")
    assert Enum.at(rows, 1)["error"] == "not_found"
    assert not Map.has_key?(Enum.at(rows, 1), "id")
    assert Enum.at(rows, 2)["id"] == Enum.at(rows, 2)["key"]
    assert Enum.at(rows, 2)["key"] == "0"
  end

  test "empty keys", context do
    db_name = context[:db_name]

    resp = all_docs(db_name, keys: :jiffy.encode([]))
    assert resp.status_code == 200
    rows = resp.body["rows"]
    assert Enum.empty?(rows)
  end

  defp all_docs(db_name, options, keys \\ nil) do
    resp =
      case keys do
        nil ->
          Couch.get("/#{db_name}/_all_docs", query: options)

        _ ->
          Couch.post("/#{db_name}/_all_docs",
            query: options,
            body: %{"keys" => keys}
          )
      end

    resp
  end
end

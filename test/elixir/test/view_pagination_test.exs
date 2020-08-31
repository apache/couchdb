defmodule ViewPaginationTest do
  use CouchTestCase

  @moduletag :view_pagination
  @moduletag kind: :single_node

  @moduledoc """
  Integration tests for pagination.
  This is a port of the view_pagination.js test suite.
  """

  @tag :with_db
  test "basic view pagination", context do
    db_name = context[:db_name]

    docs = make_docs(0..99)
    bulk_save(db_name, docs)

    query_function = "function(doc) { emit(doc.integer, null); }"

    0..99
    |> Enum.filter(fn number -> rem(number, 10) === 0 end)
    |> Enum.each(fn i ->
      query_options = %{"startkey" => i, "startkey_docid" => i, limit: 10}
      result = query(db_name, query_function, nil, query_options)
      assert result["total_rows"] === length(docs)
      assert length(result["rows"]) === 10
      assert result["offset"] === i
      Enum.each(0..9, &assert(Enum.at(result["rows"], &1)["key"] === &1 + i))
    end)
  end

  @tag :with_db
  test "aliases start_key and start_key_doc_id should work", context do
    db_name = context[:db_name]

    docs = make_docs(0..99)
    bulk_save(db_name, docs)

    query_function = "function(doc) { emit(doc.integer, null); }"

    0..99
    |> Enum.filter(fn number -> rem(number, 10) === 0 end)
    |> Enum.each(fn i ->
      query_options = %{"start_key" => i, "start_key_docid" => i, limit: 10}
      result = query(db_name, query_function, nil, query_options)
      assert result["total_rows"] === length(docs)
      assert length(result["rows"]) === 10
      assert result["offset"] === i
      Enum.each(0..9, &assert(Enum.at(result["rows"], &1)["key"] === &1 + i))
    end)
  end

  @tag :with_db
  test "descending view pagination", context do
    db_name = context[:db_name]

    docs = make_docs(0..99)
    bulk_save(db_name, docs)

    query_function = "function(doc) { emit(doc.integer, null); }"

    100..0
    |> Enum.filter(fn number -> rem(number, 10) === 0 end)
    |> Enum.map(&(&1 - 1))
    |> Enum.filter(&(&1 > 0))
    |> Enum.each(fn i ->
      query_options = %{
        "startkey" => i,
        "startkey_docid" => i,
        limit: 10,
        descending: true
      }

      result = query(db_name, query_function, nil, query_options)
      assert result["total_rows"] === length(docs)
      assert length(result["rows"]) === 10
      assert result["offset"] === length(docs) - i - 1
      Enum.each(0..9, &assert(Enum.at(result["rows"], &1)["key"] === i - &1))
    end)
  end

  @tag :with_db
  test "descending=false parameter should just be ignored", context do
    db_name = context[:db_name]

    docs = make_docs(0..99)
    bulk_save(db_name, docs)

    query_function = "function(doc) { emit(doc.integer, null); }"

    0..99
    |> Enum.filter(fn number -> rem(number, 10) === 0 end)
    |> Enum.each(fn i ->
      query_options = %{
        "start_key" => i,
        "start_key_docid" => i,
        limit: 10,
        descending: false
      }

      result = query(db_name, query_function, nil, query_options)
      assert result["total_rows"] === length(docs)
      assert length(result["rows"]) === 10
      assert result["offset"] === i
      Enum.each(0..9, &assert(Enum.at(result["rows"], &1)["key"] === &1 + i))
    end)
  end

  @tag :with_db
  test "endkey document id", context do
    db_name = context[:db_name]

    docs = make_docs(0..99)
    bulk_save(db_name, docs)

    query_function = "function(doc) { emit(null, null); }"

    query_options = %{
      "startkey" => :null,
      "startkey_docid" => 1,
      "endkey" => :null,
      "endkey_docid" => 40,
    }

    result = query(db_name, query_function, nil, query_options)
    test_end_key_doc_id(result, docs)
  end

  @tag :with_db
  test "endkey document id, but with end_key_doc_id alias", context do
    db_name = context[:db_name]

    docs = make_docs(0..99)
    bulk_save(db_name, docs)

    query_function = "function(doc) { emit(null, null); }"

    query_options = %{
      "start_key" => :null,
      "start_key_doc_id" => 1,
      "end_key" => :null,
      "end_key_doc_id" => 40,
    }

    result = query(db_name, query_function, nil, query_options)
    test_end_key_doc_id(result, docs)
  end

  defp test_end_key_doc_id(query_result, docs) do
    assert length(query_result["rows"]) === 35
    assert query_result["total_rows"] === length(docs)
    assert query_result["offset"] === 1
    assert Enum.at(query_result["rows"], 0)["id"] === "1"
    assert Enum.at(query_result["rows"], 1)["id"] === "10"
    assert Enum.at(query_result["rows"], 2)["id"] === "11"
    assert Enum.at(query_result["rows"], 3)["id"] === "12"
    assert Enum.at(query_result["rows"], 4)["id"] === "13"
    assert Enum.at(query_result["rows"], 5)["id"] === "14"
    assert Enum.at(query_result["rows"], 6)["id"] === "15"
    assert Enum.at(query_result["rows"], 7)["id"] === "16"
    assert Enum.at(query_result["rows"], 8)["id"] === "17"
    assert Enum.at(query_result["rows"], 9)["id"] === "18"
    assert Enum.at(query_result["rows"], 10)["id"] === "19"
    assert Enum.at(query_result["rows"], 11)["id"] === "2"
    assert Enum.at(query_result["rows"], 12)["id"] === "20"
    assert Enum.at(query_result["rows"], 13)["id"] === "21"
    assert Enum.at(query_result["rows"], 14)["id"] === "22"
    assert Enum.at(query_result["rows"], 15)["id"] === "23"
    assert Enum.at(query_result["rows"], 16)["id"] === "24"
    assert Enum.at(query_result["rows"], 17)["id"] === "25"
    assert Enum.at(query_result["rows"], 18)["id"] === "26"
    assert Enum.at(query_result["rows"], 19)["id"] === "27"
    assert Enum.at(query_result["rows"], 20)["id"] === "28"
    assert Enum.at(query_result["rows"], 21)["id"] === "29"
    assert Enum.at(query_result["rows"], 22)["id"] === "3"
    assert Enum.at(query_result["rows"], 23)["id"] === "30"
    assert Enum.at(query_result["rows"], 24)["id"] === "31"
    assert Enum.at(query_result["rows"], 25)["id"] === "32"
    assert Enum.at(query_result["rows"], 26)["id"] === "33"
    assert Enum.at(query_result["rows"], 27)["id"] === "34"
    assert Enum.at(query_result["rows"], 28)["id"] === "35"
    assert Enum.at(query_result["rows"], 29)["id"] === "36"
    assert Enum.at(query_result["rows"], 30)["id"] === "37"
    assert Enum.at(query_result["rows"], 31)["id"] === "38"
    assert Enum.at(query_result["rows"], 32)["id"] === "39"
    assert Enum.at(query_result["rows"], 33)["id"] === "4"
    assert Enum.at(query_result["rows"], 34)["id"] === "40"
  end
end

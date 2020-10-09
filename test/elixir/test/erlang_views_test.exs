defmodule ErlangViewsTest do
  use CouchTestCase

  @moduletag :erlang_views
  @moduletag kind: :single_node

  @moduledoc """
  basic 'smoke tests' of erlang views.
  This is a port of the erlang_views.js test suite.
  """

  @doc1 %{:_id => "1", :integer => 1, :string => "str1", :array => [1, 2, 3]}

  @erlang_map_fun """
   fun({Doc}) ->
      K = couch_util:get_value(<<"integer">>, Doc, null),
      V = couch_util:get_value(<<"string">>, Doc, null),
      Emit(K, V)
  end.
  """

  @erlang_reduce_fun """
  fun (_, Values, false) -> length(Values);
     (_, Values, true) -> lists:sum(Values)
  end.
  """

  @erlang_map_fun_2 """
  fun({Doc}) ->
   Words = couch_util:get_value(<<"words">>, Doc),
   lists:foreach(fun({Word}) ->
     WordString = couch_util:get_value(<<"word">>, Word),
     Count = couch_util:get_value(<<"count">>, Word),
     Emit(WordString , Count)
    end, Words)
  end.
  """

  @erlang_reduce_fun_2 """
  fun(Keys, Values, RR) -> length(Values) end.
  """

  @word_list ["foo", "bar", "abc", "def", "baz", "xxyz"]

  @tag :with_db
  test "Erlang map function", context do
    db_name = context[:db_name]
    create_doc(db_name, @doc1)

    results =
      query(
        db_name,
        @erlang_map_fun,
        nil,
        nil,
        nil,
        "erlang"
      )

    assert results["total_rows"] == 1
    assert List.first(results["rows"])["key"] == 1
    assert List.first(results["rows"])["value"] == "str1"
  end

  @tag :with_db
  test "Erlang reduce function", context do
    db_name = context[:db_name]
    create_doc(db_name, @doc1)
    doc2 = @doc1 |> Map.replace!(:_id, "2") |> Map.replace!(:string, "str2")
    create_doc(db_name, doc2)

    results =
      query(
        db_name,
        @erlang_map_fun,
        @erlang_reduce_fun,
        nil,
        nil,
        "erlang"
      )

    assert List.first(results["rows"])["value"] == 2
  end

  @tag :with_db
  test "Erlang reduce function larger dataset", context do
    db_name = context[:db_name]
    bulk_save(db_name, create_large_dataset(250))

    results =
      query(
        db_name,
        @erlang_map_fun_2,
        @erlang_reduce_fun_2,
        nil,
        nil,
        "erlang"
      )

    assert Map.get(List.first(results["rows"]), "key", :null) == :null
    assert List.first(results["rows"])["value"] > 0
  end

  defp create_large_dataset(size) do
    doc_words =
      for j <- 0..100 do
        %{word: get_word(j), count: j}
      end

    template_doc = %{words: doc_words}

    make_docs(0..size, template_doc)
  end

  defp get_word(idx) do
    Enum.at(@word_list, rem(idx, length(@word_list)))
  end
end

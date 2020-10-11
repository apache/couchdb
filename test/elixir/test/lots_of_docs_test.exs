defmodule LotsOfDocsTest do
  use CouchTestCase

  @moduletag :lots_of_docs
  @moduletag kind: :performance
  
  @docs_range 0..499

  @moduledoc """
  Test saving a semi-large quanitity of documents and do some view queries.
  This is a port of the lots_of_docs.js suite
  """

  @tag :with_db
  test "lots of docs with _all_docs", context do
    db_name = context[:db_name]

    @docs_range
    |> create_docs()
    |> Enum.chunk_every(100)
    |> Enum.each(fn docs -> bulk_post(docs, db_name) end)

    %{"rows" => rows, "total_rows" => total_rows} =
      Couch.get("/#{db_name}/_all_docs").body

    assert total_rows === Enum.count(@docs_range)
    assert total_rows === Enum.count(rows)

    @docs_range
    |> Enum.map(fn i -> Integer.to_string(i) end)
    |> Enum.sort()
    |> Enum.with_index()
    |> Enum.each(fn {value, index} ->
      assert Map.fetch!(Enum.at(rows, index), "key") === value
    end)

    retry_until(fn ->
      %{"rows" => desc_rows, "total_rows" => desc_total_rows} =
        Couch.get(
          "/#{db_name}/_all_docs",
          query: %{:descending => true}
        ).body

      assert desc_total_rows === Enum.count(@docs_range)
      assert desc_total_rows === Enum.count(desc_rows)

      @docs_range
      |> Enum.map(fn i -> Integer.to_string(i) end)
      |> Enum.sort()
      |> Enum.reverse()
      |> Enum.with_index()
      |> Enum.each(fn {value, index} ->
        assert Map.fetch!(Enum.at(desc_rows, index), "key") === value
      end)
    end)
  end

  @tag :skip_on_jenkins
  @tag :with_db
  test "lots of docs with a regular view", context do
    db_name = context[:db_name]

    @docs_range
    |> create_docs()
    |> Enum.chunk_every(100)
    |> Enum.each(fn docs -> bulk_post(docs, db_name) end)

    %{"rows" => rows, "total_rows" => total_rows} = query_view(db_name)
    assert total_rows === Enum.count(rows)
    assert total_rows === Enum.count(@docs_range)

    Enum.each(@docs_range, fn i ->
      assert Map.fetch!(Enum.at(rows, i), "key") === i
    end)

    retry_until(fn ->
      %{"rows" => desc_rows, "total_rows" => desc_total_rows} =
        query_view(db_name, "descending")

      assert desc_total_rows === Enum.count(desc_rows)
      assert desc_total_rows === Enum.count(@docs_range)

      @docs_range
      |> Enum.reverse()
      |> Enum.with_index()
      |> Enum.each(fn {value, index} ->
        assert Map.fetch!(Enum.at(desc_rows, index), "key") === value
      end)
    end)
  end

  defp query_view(db_name, sorting \\ "ascending") do
    descending = if(sorting === "descending", do: true, else: false)
    map_fun = "function(doc) { emit(doc.integer, null); }"
    map_doc = %{:views => %{:view => %{:map => map_fun}}}
    %{"rev" => rev} = Couch.put("/#{db_name}/_design/tempddoc", body: map_doc).body

    response =
      Couch.get(
        "/#{db_name}/_design/tempddoc/_view/view",
        query: %{:descending => descending}
      ).body

    Couch.delete("/#{db_name}/_design/tempddoc?rev=#{rev}")
    response
  end

  defp bulk_post(docs, db) do
    resp = Couch.post("/#{db}/_bulk_docs", query: [w: 3], body: %{docs: docs})

    assert resp.status_code in [201, 202] and length(resp.body) == length(docs), """
    Expected 201 and the same number of response rows as in request, but got
    #{pretty_inspect(resp)}
    """

    resp
  end
end

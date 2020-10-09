defmodule UTF8Test do
  use CouchTestCase

  @moduletag :utf8
  @moduletag kind: :single_node

  @moduledoc """
  Test CouchDB UTF8 support
  This is a port of the utf8.js test suite
  """

  @tag :with_db
  test "UTF8 support", context do
    db_name = context[:db_name]
    texts = [
      "1. Ascii: hello",
      "2. Russian: На берегу пустынных волн",
      "3. Math: ∮ E⋅da = Q,  n → ∞, ∑ f(i) = ∏ g(i),",
      "4. Geek: STARGΛ̊TE SG-1",
      "5. Braille: ⡌⠁⠧⠑ ⠼⠁⠒  ⡍⠜⠇⠑⠹⠰⠎ ⡣⠕⠌",
      "6. null \u0000 byte",
    ]
    
    texts
    |> Enum.with_index()
    |> Enum.each(fn {string, index} ->
      status = Couch.post("/#{db_name}", query: [w: 3], body: %{"_id" => Integer.to_string(index), "text" => string}).status_code
      assert status in [201, 202]
    end)

    texts
    |> Enum.with_index()
    |> Enum.each(fn {_, index} ->
      resp = Couch.get("/#{db_name}/#{index}")
      %{"_id" => id, "text" => text} = resp.body
      assert resp.status_code == 200
      assert Enum.at(texts, String.to_integer(id)) === text
    end)

    design_doc = %{
      :_id => "_design/temp_utf8_support",
      :language => "javascript",
      :views => %{
        :view => %{
          :map => "function(doc) { emit(null, doc.text) }"
        }
      }
    }

    design_resp =
      Couch.put(
        "/#{db_name}/_design/temp_utf8_support",
        body: design_doc,
        query: %{w: 3}
      )

    assert design_resp.status_code in [201, 202]

    %{"rows" => values} = Couch.get("/#{db_name}/_design/temp_utf8_support/_view/view").body
    values
    |> Enum.with_index()
    |> Enum.each(fn {%{"value" => value}, index} ->
      assert Enum.at(texts, index) === value
    end)
  end
end

defmodule CoffeeTest do
  use CouchTestCase

  @moduletag :coffee
  @moduletag kind: :single_node

  @moduledoc """
  Test basic coffeescript functionality.
  This is a port of the coffee.js test suite.
  """

  @tag :with_db
  test "CoffeeScript basic functionality", context do
    db_name = context[:db_name]

    docs = [
      %{:_id => "a", :foo => 100},
      %{:foo => 1},
      %{:foo => 1},
      %{:foo => 2},
      %{:foo => 2},
      %{:bar => 1},
      %{:bar => 1},
      %{:bar => 2},
      %{:bar => 2}
    ]

    resp = Couch.post("/#{db_name}/_bulk_docs", body: %{docs: docs})

    design_doc = %{
      :_id => "_design/coffee",
      :language => "coffeescript",
      :views => %{
        :myview => %{
          :map => "(doc) -> if doc.foo\n  emit(doc.foo, 1)",
          :reduce =>
            "(keys, values, rereduce) ->\n  sum = 0\n  for x in values\n    sum = sum + x\n  sum"
        }
      },
      :shows => %{
        :myshow => "(doc) ->\n  \"Foo #\{doc.foo}\""
      },
      :lists => %{
        :mylist =>
          "(head, req) ->\n  while row = getRow()\n    send(toJSON({\"resp\": \"Foo #\{row.value}\"}))\n  return"
      },
      :filters => %{
        :filter => "(doc) ->\n  doc.foo"
      }
    }

    design_resp = Couch.put("/#{db_name}/_design/coffee", body: design_doc)
    assert design_resp.status_code === 201

    assert resp.status_code === 201 and length(resp.body) === length(docs)

    retry_until(fn ->
      %{"rows" => values} = Couch.get("/#{db_name}/_design/coffee/_view/myview").body
      assert 5 === hd(values)["value"]
    end)

    assert Couch.get("/#{db_name}/_design/coffee/_show/myshow/a").body === "Foo 100"

    %{"resp" => list_output} =
      Couch.get("/#{db_name}/_design/coffee/_list/mylist/myview").body

    assert list_output === "Foo 5"

    %{"results" => changes_results} =
      Couch.get("/#{db_name}/_changes", query: %{"filter" => "coffee/filter"}).body

    assert length(changes_results) === 5
  end
end

defmodule MultipleRowsTest do
  use CouchTestCase

  @moduletag :multiple_rows
  @moduletag kind: :single_node

  @north_carolina_cities ["Charlotte", "Raleigh"]
  @massachussets_cities ["Boston", "Lowell", "Worcester", "Cambridge", "Springfield"]
  @florida_cities ["Miami", "Tampa", "Orlando", "Springfield"]

  @moduledoc """
  Test checking multiple rows
  This is a port of the multiple_rows.js suite
  """

  @tag :with_db
  test "multiple rows", context do
    db_name = context[:db_name]

    resp1 =
      Couch.put(
        "/#{db_name}/NC",
        body: %{:_id => "NC", :cities => @north_carolina_cities}
      ).body

    resp2 =
      Couch.put(
        "/#{db_name}/MA",
        body: %{
          :_id => "MA",
          :cities => @massachussets_cities
        }
      ).body

    resp3 =
      Couch.put("/#{db_name}/FL", body: %{:_id => "FL", :cities => @florida_cities}).body

    assert resp1["ok"]
    assert resp2["ok"]
    assert resp3["ok"]

    %{"rows" => rows, "total_rows" => total_rows} = query_list_cities_and_state(db_name)

    assert Enum.at(rows, 0)["key"] == "Boston, MA"
    assert Enum.at(rows, 1)["key"] == "Cambridge, MA"
    assert Enum.at(rows, 2)["key"] == "Charlotte, NC"
    assert Enum.at(rows, 3)["key"] == "Lowell, MA"
    assert Enum.at(rows, 4)["key"] == "Miami, FL"
    assert Enum.at(rows, 5)["key"] == "Orlando, FL"
    assert Enum.at(rows, 6)["key"] == "Raleigh, NC"
    assert Enum.at(rows, 7)["key"] == "Springfield, FL"
    assert Enum.at(rows, 8)["key"] == "Springfield, MA"
    assert Enum.at(rows, 9)["key"] == "Tampa, FL"
    assert Enum.at(rows, 10)["key"] == "Worcester, MA"

    assert total_rows === 11

    new_insert_resp =
      Couch.put(
        "/#{db_name}/NC",
        body: %{
          :id => "NC",
          :cities => List.insert_at(@north_carolina_cities, -1, "Wilmington"),
          :_rev => resp1["rev"]
        }
      ).body

    assert new_insert_resp["ok"]

    %{"rows" => rows, "total_rows" => total_rows} = query_list_cities_and_state(db_name)

    assert Enum.at(rows, 0)["key"] == "Boston, MA"
    assert Enum.at(rows, 1)["key"] == "Cambridge, MA"
    assert Enum.at(rows, 2)["key"] == "Charlotte, NC"
    assert Enum.at(rows, 3)["key"] == "Lowell, MA"
    assert Enum.at(rows, 4)["key"] == "Miami, FL"
    assert Enum.at(rows, 5)["key"] == "Orlando, FL"
    assert Enum.at(rows, 6)["key"] == "Raleigh, NC"
    assert Enum.at(rows, 7)["key"] == "Springfield, FL"
    assert Enum.at(rows, 8)["key"] == "Springfield, MA"
    assert Enum.at(rows, 9)["key"] == "Tampa, FL"
    assert Enum.at(rows, 10)["key"] == "Wilmington, NC"
    assert Enum.at(rows, 11)["key"] == "Worcester, MA"

    assert total_rows === 12

    delete_resp = Couch.delete("/#{db_name}/MA", query: %{:rev => resp2["rev"]}).body
    assert delete_resp["ok"]

    %{"rows" => rows, "total_rows" => total_rows} = query_list_cities_and_state(db_name)

    assert Enum.at(rows, 0)["key"] == "Charlotte, NC"
    assert Enum.at(rows, 1)["key"] == "Miami, FL"
    assert Enum.at(rows, 2)["key"] == "Orlando, FL"
    assert Enum.at(rows, 3)["key"] == "Raleigh, NC"
    assert Enum.at(rows, 4)["key"] == "Springfield, FL"
    assert Enum.at(rows, 5)["key"] == "Tampa, FL"
    assert Enum.at(rows, 6)["key"] == "Wilmington, NC"

    assert total_rows === 7
  end

  def query_list_cities_and_state(db_name) do
    design_doc = %{
      :_id => "_design/list_cities_and_state",
      :language => "javascript",
      :views => %{
        :view => %{
          :map => """
            function(doc) {
              for (var i = 0; i < doc.cities.length; i++)
                emit(doc.cities[i] + \", \" + doc._id, null);
            }
          """
        }
      }
    }

    design_resp =
      Couch.put(
        "/#{db_name}/_design/list_cities_and_state",
        body: design_doc,
        query: %{w: 3}
      )

    assert design_resp.status_code in [201, 202]

    %{:body => result} = Couch.get("/#{db_name}/_design/list_cities_and_state/_view/view")

    Couch.delete(
      "/#{db_name}/_design/list_cities_and_state",
      query: %{rev: design_resp.body["rev"]}
    )

    result
  end
end

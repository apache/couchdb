# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

defmodule IdCollationTest do
  use CouchTestCase

  @moduledoc """
  Doc _id comparisons in selectors should be by byte order like _all_docs. Other
  string fields are compared by ICU collation.
  """

  @db_name "id-collation"

  # As compared with byte order. With ICU it would be [a1, B0, b9, m5, Z1, zed]
  @ids ["B0", "Z1", "a1", "b9", "m5", "zed"]

  setup do
    MangoDatabase.recreate(@db_name)
    docs = Enum.map(@ids, fn id -> %{"_id" => id, "a" => %{"_id" => id}} end)
    MangoDatabase.save_docs(@db_name, docs)
    :ok
  end

  defp find(selector) do
    {:ok, docs} = MangoDatabase.find(@db_name, selector, fields: ["_id"])
    Enum.map(docs, fn doc -> doc["_id"] end)
  end

  defp all_docs(query) do
    resp = Couch.get("/#{@db_name}/_all_docs", query: query)
    assert resp.status_code == 200
    Enum.map(resp.body["rows"], fn row -> row["id"] end)
  end

  test "_id range find is the same as _all_docs" do
    {:ok, explain} =
      MangoDatabase.find(@db_name, %{"_id" => %{"$gte" => "Z"}}, explain: true)

    assert explain["index"]["name"] == "_all_docs"

    gte_z = find(%{"_id" => %{"$gte" => "Z"}})
    assert gte_z == all_docs(%{:startkey => "\"Z\""})
    assert gte_z == ["Z1", "a1", "b9", "m5", "zed"]

    lt_a = find(%{"_id" => %{"$lt" => "a"}})
    assert lt_a == all_docs(%{:endkey => "\"a\"", :inclusive_end => false})
    assert lt_a == ["B0", "Z1"]

    assert find(%{"_id" => %{"$gt" => "B0", "$lte" => "b9"}}) == ["Z1", "a1", "b9"]
  end

  test "_id range in _changes selector" do
    resp =
      Couch.post("/#{@db_name}/_changes?filter=_selector",
        body: %{"selector" => %{"_id" => %{"$gte" => "Z"}}},
        headers: ["Content-Type": "application/json"]
      )

    assert resp.status_code == 200

    ids = Enum.sort(Enum.map(resp.body["results"], fn row -> row["id"] end))
    assert ids == ["Z1", "a1", "b9", "m5", "zed"]
  end

  test "icu collation for nested _id fields" do
    # a._id instead of the top level _id
    assert find(%{"a._id" => %{"$gte" => "Z"}}) == ["Z1", "zed"]
  end
end

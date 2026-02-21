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

defmodule PaginateJsonDocs do
  use CouchTestCase

  @db_name "paginate-json-docs"
  @docs [
    %{"_id" => "100", "name" => "Jimi", "location" => "AUS", "user_id" => 1, "same" => "value"},
    %{"_id" => "200", "name" => "Eddie", "location" => "BRA", "user_id" => 2, "same" => "value"},
    %{"_id" => "300", "name" => "Harry", "location" => "CAN", "user_id" => 3, "same" => "value"},
    %{"_id" => "400", "name" => "Eddie", "location" => "DEN", "user_id" => 4, "same" => "value"},
    %{"_id" => "500", "name" => "Jones", "location" => "ETH", "user_id" => 5, "same" => "value"},
    %{
      "_id" => "600",
      "name" => "Winnifried",
      "location" => "FRA",
      "user_id" => 6,
      "same" => "value",
    },
    %{"_id" => "700", "name" => "Marilyn", "location" => "GHA", "user_id" => 7, "same" => "value"},
    %{"_id" => "800", "name" => "Sandra", "location" => "ZAR", "user_id" => 8, "same" => "value"},
  ]

  setup do
    MangoDatabase.recreate(@db_name)
    MangoDatabase.save_docs(@db_name, @docs)
    :ok
  end

  test "all docs paginate to end" do
    selector = %{"_id" => %{"$gt" => 0}}
    # Page 1
    {:ok, resp} = MangoDatabase.find(@db_name, selector, fields: ["_id"], limit: 5, return_raw: true)
    bookmark = resp["bookmark"]
    docs = resp["docs"]
    assert Enum.at(docs, 0)["_id"] == "100"
    assert length(docs) == 5

    # Page 2
    {:ok, resp} = MangoDatabase.find(@db_name,
      selector, fields: ["_id"], bookmark: bookmark, limit: 5, return_raw: true
    )
    bookmark = resp["bookmark"]
    docs = resp["docs"]
    assert Enum.at(docs, 0)["_id"] == "600"
    assert length(docs) == 3

    # Page 3
    {:ok, resp} = MangoDatabase.find(@db_name, selector, bookmark: bookmark, limit: 5, return_raw: true)
    bookmark = resp["bookmark"]
    docs = resp["docs"]
    assert Enum.empty?(docs)
  end

  test "return previous bookmark for empty" do
    selector = %{"_id" => %{"$gt" => 0}}
    # Page 1
    {:ok, resp} = MangoDatabase.find(@db_name, selector, fields: ["_id"], return_raw: true)
    bookmark1 = resp["bookmark"]
    docs = resp["docs"]
    assert length(docs) == 8

    {:ok, resp} = MangoDatabase.find(@db_name,
      selector, fields: ["_id"], return_raw: true, bookmark: bookmark1
    )
    bookmark2 = resp["bookmark"]
    docs = resp["docs"]
    assert Enum.empty?(docs)

    {:ok, resp} = MangoDatabase.find(@db_name,
      selector, fields: ["_id"], return_raw: true, bookmark: bookmark2
    )
    bookmark3 = resp["bookmark"]
    docs = resp["docs"]
    assert bookmark3 == bookmark2
    assert Enum.empty?(docs)
  end

  test "all docs with skip" do
    selector = %{"_id" => %{"$gt" => 0}}
    # Page 1
    {:ok, resp} = MangoDatabase.find(@db_name, selector, fields: ["_id"], skip: 2, limit: 5, return_raw: true)
    bookmark = resp["bookmark"]
    docs = resp["docs"]
    assert Enum.at(docs, 0)["_id"] == "300"
    assert length(docs) == 5

    # Page 2
    {:ok, resp} = MangoDatabase.find(@db_name,
      selector, fields: ["_id"], bookmark: bookmark, limit: 5, return_raw: true
    )
    bookmark = resp["bookmark"]
    docs = resp["docs"]
    assert Enum.at(docs, 0)["_id"] == "800"
    assert length(docs) == 1
    {:ok, resp} = MangoDatabase.find(@db_name, selector, bookmark: bookmark, limit: 5, return_raw: true)
    bookmark = resp["bookmark"]
    docs = resp["docs"]
    assert Enum.empty?(docs)
  end

  test "all docs reverse" do
    selector = %{"_id" => %{"$gt" => 0}}
    {:ok, resp} = MangoDatabase.find(@db_name,
      selector, fields: ["_id"], sort: [%{"_id" => "desc"}], limit: 5, return_raw: true
    )
    docs = resp["docs"]
    bookmark1 = resp["bookmark"]
    assert length(docs) == 5
    assert Enum.at(docs, 0)["_id"] == "800"

    {:ok, resp} = MangoDatabase.find(@db_name,
      selector,
      fields: ["_id"],
      sort: [%{"_id" => "desc"}],
      limit: 5,
      return_raw: true,
      bookmark: bookmark1
    )
    docs = resp["docs"]
    bookmark2 = resp["bookmark"]
    assert length(docs) == 3
    assert Enum.at(docs, 0)["_id"] == "300"

    {:ok, resp} = MangoDatabase.find(@db_name,
      selector,
      fields: ["_id"],
      sort: [%{"_id" => "desc"}],
      limit: 5,
      return_raw: true,
      bookmark: bookmark2
    )
    docs = resp["docs"]
    assert Enum.empty?(docs)
  end

  test "bad bookmark" do
    {:error, response} = MangoDatabase.find(@db_name, %{"_id" => %{"$gt" => 0}}, bookmark: "bad-bookmark")
    assert response.body["error"] == "invalid_bookmark"
    assert response.body["reason"] == "Invalid bookmark value: \"bad-bookmark\""
    assert response.status_code == 400
  end

  test "throws error on text bookmark" do
    bookmark = (
      "g2wAAAABaANkABFub2RlMUBjb3VjaGRiLm5ldGwAAAACYQBiP____2poAkY_8AAAAAAAAGEHag"
    )
    {:error, response} = MangoDatabase.find(@db_name, %{"_id" => %{"$gt" => 0}}, bookmark: bookmark)
    assert response.body["error"] == "invalid_bookmark"
    assert response.status_code == 400
  end

  test "index pagination" do
    MangoDatabase.create_index(@db_name, ["location"])
    selector = %{"location" => %{"$gt" => "A"}}
    {:ok, resp} = MangoDatabase.find(@db_name, selector, fields: ["_id"], limit: 5, return_raw: true)
    docs = resp["docs"]
    bookmark1 = resp["bookmark"]
    assert length(docs) == 5
    assert Enum.at(docs, 0)["_id"] == "100"

    {:ok, resp} = MangoDatabase.find(@db_name,
      selector, fields: ["_id"], limit: 5, return_raw: true, bookmark: bookmark1
    )
    docs = resp["docs"]
    bookmark2 = resp["bookmark"]
    assert length(docs) == 3
    assert Enum.at(docs, 0)["_id"] == "600"

    {:ok, resp} = MangoDatabase.find(@db_name,
      selector, fields: ["_id"], limit: 5, return_raw: true, bookmark: bookmark2
    )
    docs = resp["docs"]
    assert Enum.empty?(docs)
  end

  test "index pagination two keys" do
    MangoDatabase.create_index(@db_name, ["location", "user_id"])
    selector = %{"location" => %{"$gt" => "A"}, "user_id" => %{"$gte" => 1}}
    {:ok, resp} = MangoDatabase.find(@db_name, selector, fields: ["_id"], limit: 5, return_raw: true)
    docs = resp["docs"]
    bookmark1 = resp["bookmark"]
    assert length(docs) == 5
    assert Enum.at(docs, 0)["_id"] == "100"

    {:ok, resp} = MangoDatabase.find(@db_name,
      selector, fields: ["_id"], limit: 5, return_raw: true, bookmark: bookmark1
    )
    docs = resp["docs"]
    bookmark2 = resp["bookmark"]
    assert length(docs) == 3
    assert Enum.at(docs, 0)["_id"] == "600"

    {:ok, resp} = MangoDatabase.find(@db_name,
      selector, fields: ["_id"], limit: 5, return_raw: true, bookmark: bookmark2
    )
    docs = resp["docs"]
    assert Enum.empty?(docs)
  end

  test "index pagination reverse" do
    MangoDatabase.create_index(@db_name, ["location", "user_id"])
    selector = %{"location" => %{"$gt" => "A"}, "user_id" => %{"$gte" => 1}}
    sort = [%{"location" => "desc"}, %{"user_id" => "desc"}]
    {:ok, resp} = MangoDatabase.find(@db_name,
      selector, fields: ["_id"], sort: sort, limit: 5, return_raw: true
    )
    docs = resp["docs"]
    bookmark1 = resp["bookmark"]
    assert length(docs) == 5
    assert Enum.at(docs, 0)["_id"] == "800"

    {:ok, resp} = MangoDatabase.find(@db_name,
      selector,
      fields: ["_id"],
      limit: 5,
      sort: sort,
      return_raw: true,
      bookmark: bookmark1
    )
    docs = resp["docs"]
    bookmark2 = resp["bookmark"]
    assert length(docs) == 3
    assert Enum.at(docs, 0)["_id"] == "300"

    {:ok, resp} = MangoDatabase.find(@db_name,
      selector,
      fields: ["_id"],
      limit: 5,
      sort: sort,
      return_raw: true,
      bookmark: bookmark2
    )
    docs = resp["docs"]
    assert Enum.empty?(docs)
  end

  test "index pagination same emitted key" do
    MangoDatabase.create_index(@db_name, ["same"])
    selector = %{"same" => %{"$gt" => ""}}
    {:ok, resp} = MangoDatabase.find(@db_name, selector, fields: ["_id"], limit: 5, return_raw: true)
    docs = resp["docs"]
    bookmark1 = resp["bookmark"]
    assert length(docs) == 5
    assert Enum.at(docs, 0)["_id"] == "100"

    {:ok, resp} = MangoDatabase.find(@db_name,
      selector, fields: ["_id"], limit: 5, return_raw: true, bookmark: bookmark1
    )
    docs = resp["docs"]
    bookmark2 = resp["bookmark"]
    assert length(docs) == 3
    assert Enum.at(docs, 0)["_id"] == "600"

    {:ok, resp} = MangoDatabase.find(@db_name,
      selector, fields: ["_id"], limit: 5, return_raw: true, bookmark: bookmark2
    )
    docs = resp["docs"]
    assert Enum.empty?(docs)
  end
end

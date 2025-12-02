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

defmodule ChooseCorrectIndexForDocs do
  use CouchTestCase

  @db_name "choose-correct-index"
  @docs [
    %{"_id" => "_design/my-design-doc"},
    %{
      "_id" => "54af50626de419f5109c962f",
      "user_id" => 0,
      "age" => 10,
      "name" => "Jimi",
      "location" => "UK",
      "number" => 4,
    },
    %{
      "_id" => "54af50622071121b25402dc3",
      "user_id" => 1,
      "age" => 12,
      "name" => "Eddie",
      "location" => "ZAR",
      "number" => 2,
    },
    %{
      "_id" => "54af50622071121b25402dc6",
      "user_id" => 1,
      "age" => 6,
      "name" => "Harry",
      "location" => "US",
      "number" => 8,
    },
    %{
      "_id" => "54af50622071121b25402dc9",
      "name" => "Eddie",
      "occupation" => "engineer",
      "number" => 7,
    },
  ]
  @docs2 [%{"a" => 1, "b" => 1, "c" => 1}, %{"a" => 1000, "d" => 1000, "e" => 1000}]

  setup do
    MangoDatabase.recreate(@db_name)
    MangoDatabase.save_docs(@db_name, @docs)
    :ok
  end

  test "choose index with one field in index" do
    MangoDatabase.create_index(@db_name, ["name", "age", "user_id"], ddoc: "aaa")
    MangoDatabase.create_index(@db_name, ["name"], ddoc: "zzz")
    {:ok, explain} = MangoDatabase.find(@db_name, %{"name" => "Eddie"}, explain: true)
    assert explain["index"]["ddoc"] == "_design/zzz"
  end

  test "choose index with two" do
    MangoDatabase.create_index(@db_name, ["name", "age", "user_id"], ddoc: "aaa")
    MangoDatabase.create_index(@db_name, ["name", "age"], ddoc: "bbb")
    MangoDatabase.create_index(@db_name, ["name"], ddoc: "zzz")
    {:ok, explain} = MangoDatabase.find(@db_name, %{"name" => "Eddie", "age" => %{"$gte" => 12}}, explain: true)
    assert explain["index"]["ddoc"] == "_design/bbb"
  end

  test "choose index alphabetically" do
    MangoDatabase.create_index(@db_name, ["name"], ddoc: "aaa")
    MangoDatabase.create_index(@db_name, ["name"], ddoc: "bbb")
    MangoDatabase.create_index(@db_name, ["name"], ddoc: "zzz")
    {:ok, explain} = MangoDatabase.find(@db_name, %{"name" => "Eddie", "age" => %{"$gte" => 12}}, explain: true)
    assert explain["index"]["ddoc"] == "_design/aaa"
  end

  test "choose index most accurate" do
    MangoDatabase.create_index(@db_name, ["name", "age", "user_id"], ddoc: "aaa")
    MangoDatabase.create_index(@db_name, ["name", "age"], ddoc: "bbb")
    MangoDatabase.create_index(@db_name, ["name"], ddoc: "zzz")
    {:ok, explain} = MangoDatabase.find(@db_name, %{"name" => "Eddie", "age" => %{"$gte" => 12}}, explain: true)
    assert explain["index"]["ddoc"] == "_design/bbb"
  end

  test "choose index most accurate in memory selector" do
    MangoDatabase.create_index(@db_name, ["name", "location", "user_id"], ddoc: "aaa")
    MangoDatabase.create_index(@db_name, ["name", "age", "user_id"], ddoc: "bbb")
    MangoDatabase.create_index(@db_name, ["name"], ddoc: "zzz")
    {:ok, explain} = MangoDatabase.find(@db_name, %{"name" => "Eddie", "number" => %{"$lte" => 12}}, explain: true)
    assert explain["index"]["ddoc"] == "_design/zzz"
  end

  test "warn on full db scan" do
    selector = %{"not_indexed" => "foo"}
    {:ok, explain} = MangoDatabase.find(@db_name, selector, explain: true, return_raw: true)
    assert explain["index"]["type"] == "special"
    {:ok, resp} = MangoDatabase.find(@db_name, selector, return_raw: true)
    assert resp["warning"] == "No matching index found, create an index to optimize query time."
  end

  test "chooses idxA" do
    MangoDatabase.save_docs(@db_name, @docs2)
    MangoDatabase.create_index(@db_name, ["a", "b", "c"])
    MangoDatabase.create_index(@db_name, ["a", "d", "e"])
    {:ok, explain} = MangoDatabase.find(@db_name,
      %{"a" => %{"$gt" => 0}, "b" => %{"$gt" => 0}, "c" => %{"$gt" => 0}}, explain: true
    )
    assert explain["index"]["def"]["fields"] == [%{"a" => "asc"}, %{"b" => "asc"}, %{"c" => "asc"}]
  end

  test "can query with range on secondary column" do
    MangoDatabase.create_index(@db_name, ["age", "name"], ddoc: "bbb")
    selector = %{"age" => 10, "name" => %{"$gte" => 0}}
    {:ok, docs} = MangoDatabase.find(@db_name, selector)
    assert length(docs) == 1
    {:ok, explain} = MangoDatabase.find(@db_name, selector, explain: true)
    assert explain["index"]["ddoc"] == "_design/bbb"
    assert explain["mrargs"]["end_key"] == [10, "<MAX>"]
  end

  # all documents contain an _id and _rev field they
  # should not be used to restrict indexes based on the
  # fields required by the selector
  test "choose index with id" do
    MangoDatabase.create_index(@db_name, ["name", "_id"], ddoc: "aaa")
    {:ok, explain} = MangoDatabase.find(@db_name, %{"name" => "Eddie"}, explain: true)
    assert explain["index"]["ddoc"] == "_design/aaa"
  end

  test "choose index with rev" do
    MangoDatabase.create_index(@db_name, ["name", "_rev"], ddoc: "aaa")
    {:ok, explain} = MangoDatabase.find(@db_name, %{"name" => "Eddie"}, explain: true)
    assert explain["index"]["ddoc"] == "_design/aaa"
  end
end

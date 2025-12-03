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

defmodule SortTests do
  use CouchTestCase

  @db_name "sort-docs"

  setup do
    UserDocs.setup(@db_name, "text")
  end

  test "number sort" do
    q = %{"age" => %{"$gt" => 0}}
    {:ok, docs} = MangoDatabase.find(@db_name, q, sort: ["age:number"])

    assert length(docs) == 15
    assert Enum.at(docs, 0)["age"] == 22
  end

  test "number sort desc" do
    q = %{"age" => %{"$gt" => 0}}
    sort = [%{"age" => "desc"}]
    {:ok, docs} = MangoDatabase.find(@db_name, q, sort: sort)

    assert length(docs) == 15
    assert Enum.at(docs, 0)["age"] == 79

    q = %{"manager" => true}
    sort = [%{"age:number" => "desc"}]
    {:ok, docs} = MangoDatabase.find(@db_name, q, sort: sort)

    assert length(docs) == 10
    assert Enum.at(docs, 0)["age"] == 79
  end

  test "string sort" do
    q = %{"email" => %{"$gt" => nil}}
    {:ok, docs} = MangoDatabase.find(@db_name, q, sort: ["email:string"])

    assert length(docs) == 15
    assert Enum.at(docs, 0)["email"] == "abbottwatson@talkola.com"
  end

  test "notype sort" do
    q = %{"email" => %{"$gt" => nil}}
    {:error, resp} = MangoDatabase.find(@db_name, q, sort: ["email"])

    assert resp.status_code == 400
    assert resp.body["error"] == "text_sort_error"
    assert resp.body["reason"] == "Unspecified or ambiguous sort type. Try appending :number or :string to the sort field. email"
  end

  test "array sort" do
    q = %{"favorites" => %{"$exists" => true}}
    {:ok, docs} = MangoDatabase.find(@db_name, q, sort: ["favorites.[]:string"])

    assert length(docs) == 15
    assert Enum.at(docs, 0)["user_id"] == 8
  end

  test "multi sort" do
    q = %{"name" => %{"$exists" => true}}
    {:ok, docs} = MangoDatabase.find(@db_name, q, sort: ["name.last:string", "age:number"])

    assert length(docs) == 15
    assert Enum.at(docs, 0)["name"] ==  %{"first" => "Shelly", "last" => "Ewing"}
    assert Enum.at(docs, 1)["age"] == 22
  end

  test "guess type sort" do
    q = %{
      "$or" => [
        %{"age" => %{"$gt" => 0}},
        %{"email" => %{"$gt" => nil}}
      ]
    }
    {:ok, docs} = MangoDatabase.find(@db_name, q, sort: ["age"])

    assert length(docs) == 15
    assert Enum.at(docs, 0)["age"] == 22
  end

  test "guess dup type sort" do
    q = %{
      "$and" => [
        %{"age" => %{"$gt" => 0}},
        %{"email" =>  %{"$gt" => nil}},
        %{"age" => %{"$lte" => 100}},
      ]
    }
    {:ok, docs} = MangoDatabase.find(@db_name, q, sort: ["age"])
    assert length(docs) == 15
    assert Enum.at(docs, 0)["age"] == 22
  end

  test "guess ambiguous type sort" do
    q = %{
      "$or" => [
        %{"age" => %{"$gt" => 0}},
        %{"email" => %{"$gt" => nil}},
        %{"age" => "34"}
      ]
    }
    {:error, resp} = MangoDatabase.find(@db_name, q, sort: ["age"])

    assert resp.status_code == 400
  end

  test "guess multi sort" do
    q = %{
      "$or" => [
        %{"age" => %{"$gt" => 0}},
        %{"email" =>  %{"$gt" => nil}},
        %{"name.last" => "Harvey"}
      ]
    }
    {:ok, docs} = MangoDatabase.find(@db_name, q, sort: ["name.last", "age"])

    assert length(docs) == 15
    assert Enum.at(docs, 0)["name"] ==  %{"first" => "Shelly", "last" => "Ewing"}
    assert Enum.at(docs, 1)["age"] == 22
  end

  test "guess mix sort" do
    q = %{
      "$or" => [
        %{"age" => %{"$gt" => 0}},
        %{"email" =>  %{"$gt" => nil}},
        %{"name.last" => "Harvey"}
      ]
    }
    {:ok, docs} = MangoDatabase.find(@db_name, q, sort: ["name.last", "age"])

    assert length(docs) == 15
    assert Enum.at(docs, 0)["name"] ==  %{"first" => "Shelly", "last" => "Ewing"}
    assert Enum.at(docs, 1)["age"] == 22
  end
end
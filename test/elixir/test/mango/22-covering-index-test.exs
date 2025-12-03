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

defmodule CoveringIndexTest do
  use CouchTestCase
  defmacro describe(db) do
    quote do
      test "index covers query 1 field index id" do
        covered?(unquote(db), %{"age" => %{"$gte" => 32}}, ["_id"], "age")
      end

      test "index covers query 2 field index id" do
        covered?(
          unquote(db), %{"company" => "Lyria", "manager" => true}, ["_id"], "company_and_manager"
        )
      end

      test "index covers query 2 field index extract field" do
        covered?(
          unquote(db),
          %{"company" => %{"$exists" => true}, "manager" => true},
          ["company"],
          "company_and_manager"
        )
      end

      test "index covers query 2 field index extract field force index" do
        covered?(
          unquote(db),
          %{"company" => %{"$exists" => true}, "manager" => true},
          ["company"],
          "company_and_manager",
          use_index: "company_and_manager"
      )
      end

      test "index covers query elemMatch" do
        covered?(
          unquote(db), %{"favorites" => %{"$elemMatch" => %{"$eq" => "Erlang"}}}, ["favorites"], "favorites"
        )
      end

      test "index covers query composite field_id" do
        covered?(
          unquote(db), %{"name" => %{"first" => "Stephanie", "last" => "Kirkland"}}, ["_id"], "name"
        )
      end

      test "index does not cover query empty selector" do
        not_covered?(unquote(db), %{}, ["_id"])
      end

      test "index does not cover query field not in index" do
        not_covered?(unquote(db), %{"age" => %{"$gte" => 32}}, ["name"])
      end

      test "index does not cover query all fields" do
        not_covered?(unquote(db), %{"age" => %{"$gte" => 32}}, [])
      end

      test "index does not cover query partial selector id" do
        not_covered?(unquote(db), %{"location.state" => "Nevada"}, ["_id"])
      end

      test "index does not cover query partial selector" do
        not_covered?(unquote(db), %{"name.last" => "Hernandez"}, ["name.first"])
      end

      test "index does not cover selector with more fields" do
        not_covered?(
          unquote(db),
          %{
            "$and" => [
              %{"age" => %{"$ne" => 23}},
              %{"twitter" => %{"$not" => %{"$regex" => "^@.*[0-9]+$"}}},
              %{"location.address.number" => %{"$gt" => 4288}},
              %{"location.city" => %{"$ne" => "Pico Rivera"}},
            ]
          },
          ["twitter"],
          use_index: "twitter"
        )
      end
    end
  end
end

defmodule RegularCoveringIndexTest do
  use CouchTestCase
  require CoveringIndexTest

  @db_name "regular-covering-index"

  setup do
    UserDocs.setup(@db_name)
  end

  def covered?(db, selector, fields, index, opts \\ []) do
    use_index = Keyword.get(opts, :use_index, nil)
    {:ok, resp} = MangoDatabase.find(db, selector, fields: fields, use_index: use_index, explain: true)

    assert resp["index"]["type"] == "json"
    assert resp["index"]["name"] == index
    assert resp["mrargs"]["include_docs"] == false
    assert resp["covering"] == true
  end

  def not_covered?(db, selector, fields, opts \\ []) do
    use_index = Keyword.get(opts, :use_index, nil)
    {:ok, resp} = MangoDatabase.find(db, selector, fields: fields, use_index: use_index, explain: true)

    assert resp["mrargs"]["include_docs"] == true
    assert resp["covering"] == false
  end

  CoveringIndexTest.describe(@db_name)

  test "covering index provides correct answer 2 field index" do
    {:ok, docs} = MangoDatabase.find(
      @db_name,
      %{"company" => %{"$exists" => true}, "manager" => true},
      sort: [%{"company" => "asc"}],
      fields: ["company"],
      use_index: "company_and_manager"
    )
    expected = [
      %{"company" => "Affluex"},
      %{"company" => "Globoil"},
      %{"company" => "Lyria"},
      %{"company" => "Manglo"},
      %{"company" => "Myopium"},
      %{"company" => "Niquent"},
      %{"company" => "Oulu"},
      %{"company" => "Prosely"},
      %{"company" => "Tasmania"},
      %{"company" => "Zialactic"},
    ]
    assert docs == expected
  end

  test "covering index provides correct answer id" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$gte" => 32}}, fields: ["_id"])
    expected = [
      %{"_id" => "659d0430-b1f4-413a-a6b7-9ea1ef071325"},
      %{"_id" => "48ca0455-8bd0-473f-9ae2-459e42e3edd1"},
      %{"_id" => "e900001d-bc48-48a6-9b1a-ac9a1f5d1a03"},
      %{"_id" => "b31dad3f-ae8b-4f86-8327-dfe8770beb27"},
      %{"_id" => "71562648-6acb-42bc-a182-df6b1f005b09"},
      %{"_id" => "c78c529f-0b07-4947-90a6-d6b7ca81da62"},
      %{"_id" => "8e1c90c0-ac18-4832-8081-40d14325bde0"},
      %{"_id" => "6c0afcf1-e57e-421d-a03d-0c0717ebf843"},
      %{"_id" => "5b61abc1-a3d3-4092-b9d7-ced90e675536"},
      %{"_id" => "a33d5457-741a-4dce-a217-3eab28b24e3e"},
      %{"_id" => "b06aadcf-cd0f-4ca6-9f7e-2c993e48d4c4"},
      %{"_id" => "b1e70402-8add-4068-af8f-b4f3d0feb049"},
      %{"_id" => "0461444c-e60a-457d-a4bb-b8d811853f21"},
    ]
    assert docs == expected
  end
end

defmodule PartitionedCoveringIndexTest do
  use CouchTestCase
  require CoveringIndexTest

  @db_name "partitioned-covering-index"

  setup do
    UserDocs.setup(@db_name, "view", true)
  end

  def covered?(db, selector, fields, index, opts \\ []) do
    use_index = Keyword.get(opts, :use_index, nil)
    {:ok, resp} = MangoDatabase.find(db, selector, fields: fields, use_index: use_index, explain: true, partition: "0")

    assert resp["index"]["type"] == "json"
    assert resp["index"]["name"] == index
    assert resp["mrargs"]["include_docs"] == false
    assert resp["covering"] == true
  end

  def not_covered?(db, selector, fields, opts \\ []) do
    use_index = Keyword.get(opts, :use_index, nil)
    {:ok, resp} = MangoDatabase.find(db, selector, fields: fields, use_index: use_index, explain: true, partition: "0")

    assert resp["mrargs"]["include_docs"] == true
    assert resp["covering"] == false
  end

  CoveringIndexTest.describe(@db_name)

  test "covering index provides correct answer 2 field index" do
    {:ok, docs} = MangoDatabase.find(
      @db_name,
      %{"company" => %{"$exists" => true}, "manager" => true},
      sort: [%{"company" => "asc"}],
      fields: ["company"],
      use_index: "company_and_manager",
      partition: "0"
    )
    expected = [
      %{"company" => "Manglo"},
      %{"company" => "Oulu"},
      %{"company" => "Prosely"},
      %{"company" => "Tasmania"},
    ]
    assert docs == expected
  end

  test "covering index provides correct answer id" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$gte" => 32}}, fields: ["_id"], partition: "0")

    expected = [
      %{"_id" => "0:0461444c-e60a-457d-a4bb-b8d811853f21"},
      %{"_id" => "0:5b61abc1-a3d3-4092-b9d7-ced90e675536"},
      %{"_id" => "0:71562648-6acb-42bc-a182-df6b1f005b09"},
      %{"_id" => "0:b31dad3f-ae8b-4f86-8327-dfe8770beb27"},
    ]
    assert Enum.sort_by(docs, fn x -> x["_id"] end) == expected
  end
end

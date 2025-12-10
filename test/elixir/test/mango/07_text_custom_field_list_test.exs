# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
#   http =>//www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

defmodule CustomFieldsTest do
  use CouchTestCase

  @db_name "custom-fields-test"

  @fields [
    %{"name" => "favorites.[]", "type" => "string"},
    %{"name" => "manager", "type" => "boolean"},
    %{"name" => "age", "type" => "number"},
    # These two are to test the default analyzer for
    # each field.
    %{"name" => "location.state", "type" => "string"},
    %{"name" => "location.address.street", "type" => "string"},
    %{"name" => "name\\.first", "type" => "string"},
  ]

  setup do
    UserDocs.setup(@db_name, "text", false, fields: @fields)
  end

  test "basic" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => 22})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 9
  end

  test "multi field" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => 22, "manager" => true})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 9

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => 22, "manager" => false})
    assert Enum.empty?(docs)
  end

  test "element access" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"favorites.0" => "Ruby"})
    assert length(docs) == 3
    assert Enum.all?(Enum.map(docs, & &1["favorites"]), fn fav -> Enum.member?(fav, "Ruby") end)
  end

  # TODO
  # The Python test asserts an exception because we only index the array
  # favorites.[], and not the string field favorites
  #
  # When mango was new, it failed if it didn’t find an index.
  # shortly after mango was added to CouchDB,
  # a mechanism was added to fall back to the `_all_docs` index if no other matching
  # index could be found. The Python test was possibly written before the change
  # happened and we cannot port the test as it is.

  # test "index selection" do
  #   selector = %{
  #     "selector" => %{"$or" => [%{"favorites" => "Ruby"}, %{"favorites.0" => "Ruby"}]}
  #   }
  #   {:error, resp} = MangoDatabase.find(@db_name, selector)
  #   assert resp.status_code == 400
  # end

  test "in with array" do
    vals = ["Lisp", "Python"]
    {:ok, docs} = MangoDatabase.find(@db_name, %{"favorites" => %{"$in" => vals}})
    assert length(docs) == 10
  end

  test "in with array not explicit" do
    agelist = [22, 51]
    statelist = ["New Hampshire"]
    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$in" => agelist}})
    {:ok, docs2} = MangoDatabase.find(@db_name, %{"location.state" => %{"$in" => statelist}})
    {:ok, docs3} = MangoDatabase.find(@db_name, %{"age" => %{"$in" => statelist}})
    assert length(docs) == 2
    assert length(docs2) == 1
    assert Enum.empty?(docs3)
  end

  # TODO
  # The Python test asserts an exception because we only indexed
  # favorites.[] of type string. For the following query to work, the
  # user has to index favorites.[] of type number, and also
  # favorites.[].Versions.Alpha of type string.
  #
  # When mango was new, it failed if it didn’t find an index.
  # shortly after mango was added to CouchDB,
  # a mechanism was added to fall back to the `_all_docs` index if no other matching
  # index could be found. The Python test was possibly written before the change
  # happened and we cannot port the test as it is.

  # test "in different types" do
  #   vals = ["Random Garbage", 52, %{"Versions" => %{"Alpha" => "Beta"}}]
  #   {:error, resp} = MangoDatabase.find(@db_name, %{"favorites" => %{"$in" => vals}})
  #   assert resp.status_code == 400
  # end

  test "nin with array" do
    vals = ["Lisp", "Python"]
    {:ok, docs} = MangoDatabase.find(@db_name, %{"favorites" => %{"$nin" => vals}})
    assert length(docs) == 5
  end

  test "missing" do
    MangoDatabase.find(@db_name, %{"location.state" => "Nevada"})
  end

  # TODO
  # The Python test asserts an exception.
  #
  # When mango was new, it failed if it didn’t find an index.
  # shortly after mango was added to CouchDB,
  # a mechanism was added to fall back to the `_all_docs` index if no other matching
  # index could be found. The Python test was possibly written before the change
  # happened and we cannot port the test as it is.

  # test "missing type" do
  #   {:error, resp} = MangoDatabase.find(@db_name, %{"age" => "foo"})
  #   assert resp.status_code == 400
  # end

  test "field analyzer is keyword" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"location.state" => "New"})
    assert Enum.empty?(docs)

    {:ok, docs} = MangoDatabase.find(@db_name, %{"location.state" => "New Hampshire"})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 10
  end


  # TODO
  # The Python test asserts an exception.
  # Since our FIELDS list only includes "name\\.first", we should
  # get an error when we try to search for "name.first", since the index
  # for that field does not exist.
  #
  # When mango was new, it failed if it didn’t find an index.
  # shortly after mango was added to CouchDB,
  # a mechanism was added to fall back to the `_all_docs` index if no other matching
  # index could be found. The Python test was possibly written before the change
  # happened and we cannot port the test as it is.

  # test "escaped field" do
  #   {:ok, docs} = MangoDatabase.find(@db_name, %{"name\\.first" => "name dot first"})
  #   assert length(docs) == 1
  #   assert Enum.at(docs, 0)["name.first"] == "name dot first"

  #   {:error, resp} = MangoDatabase.find(@db_name, %{"name.first" => "name dot first"})
  #   assert resp.status_code == 400
  # end

  test "filtered search fields" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => 22}, fields: ["age", "location.state"])
    assert length(docs) == 1
    assert docs == [%{"age" => 22, "location" => %{"state" => "Missouri"}}]

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => 22}, fields: ["age", "Random Garbage"])
    assert length(docs) == 1
    assert docs == [%{"age" => 22}]

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => 22}, fields: ["favorites"])
    assert length(docs) == 1
    assert docs == [%{"favorites" => ["Lisp", "Erlang", "Python"]}]

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => 22}, fields: ["favorites.[]"])
    assert length(docs) == 1
    assert docs == [%{}]

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => 22}, fields: ["all_fields"])
    assert length(docs) == 1
    assert docs == [%{}]
  end

  test "two or" do
    {:ok, docs} = MangoDatabase.find(@db_name,
      %{
        "$or" => [
          %{"location.state" => "New Hampshire"},
          %{"location.state" => "Don't Exist"},
        ]
      }
    )
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 10
  end

  test "all match" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"favorites" => %{"$allMatch" => %{"$eq" => "Erlang"}}})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 10
  end
end

defmodule CustomFieldsExistsTest do
  use CouchTestCase

  @db_name "custom-fields-exists-test"

  @fields [
    %{"name" => "exists_field", "type" => "string"},
    %{"name" => "exists_array.[]", "type" => "string"},
    %{"name" => "exists_object.should", "type" => "string"},
    %{"name" => "twitter", "type" => "string"}
  ]

  setup do
    UserDocs.setup(@db_name, "text", false, fields: @fields)
  end

  test "exists field" do
    selector = %{"exists_field" => %{"$exists" => true}}
    {:ok, docs} = MangoDatabase.find(@db_name, selector)

    assert length(docs) == 2
    Enum.each(docs, fn doc ->
      assert doc["user_id"] in [7, 8]
    end)

    selector = %{"exists_field" => %{"$exists" => false}}
    {:ok, docs} = MangoDatabase.find(@db_name, selector)

    assert length(docs) == UserDocs.len() - 2
    Enum.each(docs, fn doc ->
      refute doc["user_id"] in [7, 8]
    end)
  end

  test "exists array" do
    selector = %{"exists_array" => %{"$exists" => true}}
    {:ok, docs} = MangoDatabase.find(@db_name, selector)

    assert length(docs) == 2
    Enum.each(docs, fn doc ->
      assert doc["user_id"] in [9, 10]
    end)

    selector = %{"exists_array" => %{"$exists" => false}}
    {:ok, docs} = MangoDatabase.find(@db_name, selector)

    assert length(docs) == UserDocs.len() - 2
    Enum.each(docs, fn doc ->
      refute doc["user_id"] in [9, 10]
    end)
  end

  test "exists object member" do
    selector = %{"exists_object.should" => %{"$exists" => true}}
    {:ok, docs} = MangoDatabase.find(@db_name, selector)

    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 11

    selector = %{"exists_object.should" => %{"$exists" => false}}
    {:ok, docs} = MangoDatabase.find(@db_name, selector)

    assert length(docs) == UserDocs.len() - 1
    Enum.each(docs, fn doc ->
      refute doc["user_id"] in [11]
    end)
  end

  test "exists false same as views" do
    selector = %{"twitter" => %{"$exists" => false}}
    {:ok, docs} = MangoDatabase.find(@db_name, selector)

    Enum.each(docs, fn doc ->
      refute doc["user_id"] in [0, 1, 4, 13]
    end)
  end
end

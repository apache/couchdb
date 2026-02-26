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

defmodule UsersDbFindTests do
  use CouchTestCase

  @db_name "_users"

  setup do
    UserDocs.setup_users(@db_name)
  end

  test "simple find" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"name" => %{"$eq" => "demo02"}})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["_id"] == "org.couchdb.user:demo02"
  end

  def multi_cond_and_test() do
    MangoDatabase.create_index(@db_name, ["type", "roles"])
    selector = %{"type" => "user", "roles" => %{"$eq" => ["reader"]}}
    {:ok, docs} = MangoDatabase.find(@db_name, selector)
    assert length(docs) == 1
    assert Enum.at(docs, 0)["_id"] == "org.couchdb.user:demo02"
  end

  test "multi cond and" do
    multi_cond_and_test()
  end

  def multi_cond_or_test() do
    selector = %{"$and" => [%{"type" => "user"}, %{"$or" => [%{"order" => 1}, %{"order" => 3}]}]}
    {:ok, docs} = MangoDatabase.find(@db_name, selector)
    assert length(docs) == 2
    assert Enum.at(docs, 0)["_id"] == "org.couchdb.user:demo01"
    assert Enum.at(docs, 1)["_id"] == "org.couchdb.user:demo03"
  end

  test "multi cond or" do
    multi_cond_or_test()
  end

  def sort_test() do
    MangoDatabase.create_index(@db_name, ["order", "name"])
    selector = %{"name" => %{"$gt" => "demo01"}}
    {:ok, docs1} = MangoDatabase.find(@db_name, selector, sort: [%{"order" => "asc"}])
    docs2 = Enum.sort_by(docs1, & &1["order"])
    assert docs1 == docs2

    {:ok, docs1} = MangoDatabase.find(@db_name, selector, sort: [%{"order" => "desc"}])
    docs2 =
      docs1
      |> Enum.sort_by(& &1["order"])
      |> Enum.reverse()
    assert docs1 == docs2
  end

  test "sort" do
    sort_test()
  end

  test "fields" do
    selector = %{"name" => %{"$eq" => "demo02"}}
    {:ok, docs} = MangoDatabase.find(@db_name, selector, fields: ["name", "order"])
    assert length(docs) == 1
    assert Map.keys(Enum.at(docs, 0)) == ["name", "order"]
  end

  test "empty" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{})
    assert length(docs) == 3
  end
end

defmodule UsersDbIndexFindTests do
  use CouchTestCase

  @db_name "_users"
  setup do
    MangoDatabase.create_index(@db_name, ["name"])
    :ok
  end

  test "multi cond and" do
    MangoDatabase.create_index(@db_name, ["type", "roles"])
    :ok
    UsersDbFindTests.multi_cond_and_test()
  end

  test "multi cond or" do
    MangoDatabase.create_index(@db_name, ["type", "order"])
    UsersDbFindTests.multi_cond_or_test()
  end

  test "sort" do
    MangoDatabase.create_index(@db_name, ["order", "name"])
    UsersDbFindTests.sort_test()
  end
end

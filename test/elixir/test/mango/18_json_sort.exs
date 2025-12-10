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

defmodule JSONIndexSortOptimisations do
  use CouchTestCase

  @db_name "json-sort-docs"
  @docs [
    %{"_id" => "1", "name" => "Jimi", "age" => 10, "cars" => 1},
    %{"_id" => "2", "name" => "Eddie", "age" => 20, "cars" => 1},
    %{"_id" => "3", "name" => "Jane", "age" => 30, "cars" => 2},
    %{"_id" => "4", "name" => "Mary", "age" => 40, "cars" => 2},
    %{"_id" => "5", "name" => "Sam", "age" => 50, "cars" => 3}
  ]

  setup do
    MangoDatabase.recreate(@db_name)
    MangoDatabase.save_docs(@db_name, @docs)
    :ok
  end

  test "test_works_for_basic_case" do
    MangoDatabase.create_index(@db_name, ["cars", "age"], name: "cars-age")
    selector = %{"cars" => "2", "age" => %{"$gt" => 10}}
    {:ok, explain} = MangoDatabase.find(@db_name, selector, sort: ["age"], explain: true)

    assert explain["index"]["name"] == "cars-age"
    assert explain["mrargs"]["direction"] == "fwd"
  end

  test "test_works_for_all_fields_specified" do
    MangoDatabase.create_index(@db_name, ["cars", "age"], name: "cars-age")
    selector = %{"cars" => "2", "age" => %{"$gt" => 10}}
    {:ok, explain} = MangoDatabase.find(@db_name, selector, sort: ["cars", "age"], explain: true)

    assert explain["index"]["name"] == "cars-age"
  end

  test "test_works_for_no_sort_fields_specified" do
    MangoDatabase.create_index(@db_name, ["cars", "age"], name: "cars-age")
    selector = %{"cars" => %{"$gt" => 10}, "age" => %{"$gt" => 10}}
    {:ok, explain} = MangoDatabase.find(@db_name, selector, explain: true)

    assert explain["index"]["name"] == "cars-age"
  end

  test "test_works_for_opp_dir_sort" do
    MangoDatabase.create_index(@db_name, ["cars", "age"], name: "cars-age")
    selector = %{"cars" => "2", "age" => %{"$gt" => 10}}
    {:ok, explain} = MangoDatabase.find(@db_name, selector, sort: [%{"age" => "desc"}], explain: true)

    assert explain["index"]["name"] == "cars-age"
    assert explain["mrargs"]["direction"] == "rev"
  end

  test "test_not_work_for_non_constant_field" do
    MangoDatabase.create_index(@db_name, ["cars", "age"], name: "cars-age")
    selector = %{"cars" => %{"$gt" => 10}, "age" => %{"$gt" => 10}}
    {:error, resp} = MangoDatabase.find(@db_name, selector, sort: ["age"])

    assert resp.body["error"] == "no_usable_index"
    assert resp.status_code == 400
  end

  test "test_three_index_one" do
    MangoDatabase.create_index(@db_name, ["cars", "age", "name"], name: "cars-age-name")
    selector = %{"cars" => "2", "age" => 10, "name" => %{"$gt" => "AA"}}
    {:ok, explain} = MangoDatabase.find(@db_name, selector, sort: ["name"], explain: true)

    assert explain["index"]["name"] == "cars-age-name"
  end

  test "test_three_index_two" do
    MangoDatabase.create_index(@db_name, ["cars", "age", "name"], name: "cars-age-name")
    selector = %{"cars" => "2", "name" => "Eddie", "age" => %{"$gt" => 10}}
    {:ok, explain} = MangoDatabase.find(@db_name, selector, sort: ["age"], explain: true)

    assert explain["index"]["name"] == "cars-age-name"
  end

  test "test_three_index_fails" do
    MangoDatabase.create_index(@db_name, ["cars", "age", "name"], name: "cars-age-name")
    selector = %{"name" => "Eddie", "age" => %{"$gt" => 1}, "cars" => %{"$gt" => "1"}}
    {:error, response} = MangoDatabase.find(@db_name, selector, sort: ["name"])

    assert response.body["error"] == "no_usable_index"
    assert response.status_code == 400
  end

  test "test_empty_sort" do
    MangoDatabase.create_index(@db_name, ["cars", "age", "name"], name: "cars-age-name")
    selector = %{"name" => %{"$gt" => "Eddie"}, "age" => 10, "cars" => %{"$gt" => "1"}}
    {:ok, explain} = MangoDatabase.find(@db_name, selector, explain: true)

    assert explain["index"]["name"] == "cars-age-name"
  end

  test "test_in_between" do
    MangoDatabase.create_index(@db_name, ["cars", "age", "name"], name: "cars-age-name")
    selector = %{"name" => "Eddie", "age" => 10, "cars" => %{"$gt" => "1"}}

    {:ok, explain} = MangoDatabase.find(@db_name, selector, explain: true)
    assert explain["index"]["name"] == "cars-age-name"

    {:error, response} = MangoDatabase.find(@db_name, selector, sort: ["cars", "name"])
    assert response.body["error"] == "no_usable_index"
    assert response.status_code == 400
  end

  test "test_ignore_after_set_sort_value" do
    MangoDatabase.create_index(@db_name, ["cars", "age", "name"], name: "cars-age-name")
    selector = %{"age" => %{"$gt" => 10}, "cars" => 2, "name" => %{"$gt" => "A"}}
    {:ok, explain} = MangoDatabase.find(@db_name, selector, sort: ["age"], explain: true)

    assert explain["index"]["name"] == "cars-age-name"
  end

  test "test_not_use_index_if_other_fields_in_sort" do
    MangoDatabase.create_index(@db_name, ["cars", "age"], name: "cars-age")
    selector = %{"age" => 10, "cars" => %{"$gt" => "1"}}

    {:error, response} = MangoDatabase.find(@db_name, selector, sort: ["cars", "name"])
    assert response.body["error"] == "no_usable_index"
    assert response.status_code == 400
  end

end

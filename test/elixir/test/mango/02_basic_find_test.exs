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

defmodule BasicFindTest do
  use CouchTestCase

  @db_name "basic-find"

  setup do
    UserDocs.setup(@db_name)
  end

  test "bad selector" do
    bad_selectors = [
      nil,
      true,
      false,
      1.0,
      "foobarbaz",
      %{"foo" => %{"$not_an_op" => 2}},
      %{"$gt" => 2},
      [nil, "bing"],
      %{"_id" => %{"" => nil}},
    ]
    Enum.each(bad_selectors, fn bs ->
      {:error, resp} = MangoDatabase.find(@db_name, bs)
      assert resp.status_code == 400
    end)
  end

  test "bad limit" do
    bad_limits = [nil, true, false, -1, 1.2, "no limit!", %{"foo" => "bar"}, [2]]
    Enum.each(bad_limits, fn bl ->
      {:error, resp} = MangoDatabase.find(@db_name, %{"int" => %{"$gt" => 2}}, limit: bl, sort: [%{"age" => "asc"}])
      assert resp.status_code == 400
    end)
  end

  test "bad skip" do
    bad_skips = [nil, true, false, -3, 1.2, "no limit!", %{"foo" => "bar"}, [2]]
    Enum.each(bad_skips, fn bs ->
      {:error, resp} = MangoDatabase.find(@db_name, %{"int" => %{"$gt" => 2}}, skip: bs, sort: [%{"age" => "asc"}])
      assert resp.status_code == 400
    end)
  end

  test "bad sort" do
    bad_sorts = [
      nil,
      true,
      false,
      1.2,
      "no limit!",
      %{"foo" => "bar"},
      [2],
      [%{"foo" => "asc", "bar" => "asc"}],
      [%{"foo" => "asc"}, %{"bar" => "desc"}]
    ]
    Enum.each(bad_sorts, fn bs ->
      {:error, resp} = MangoDatabase.find(@db_name, %{"int" => %{"$gt" => 2}}, sort: bs)
      assert resp.status_code == 400
    end)
  end

  test "bad fields" do
    bad_fields = [
      nil,
      true,
      false,
      1.2,
      "no limit!",
      %{"foo" => "bar"},
      [2],
      [[]],
      ["foo", 2.0],
    ]
    Enum.each(bad_fields, fn bf ->
      {:error, resp} = MangoDatabase.find(@db_name, %{"int" => %{"$gt" => 2}}, fields: bf)
      assert resp.status_code == 400
    end)
  end

  test "bad r" do
    bad_rs = [nil, true, false, 1.2, "no limit!", %{"foo" => "bar"}, [2]]
    Enum.each(bad_rs, fn br ->
      {:error, resp} = MangoDatabase.find(@db_name, %{"int" => %{"$gt" => 2}}, r: br)
      assert resp.status_code == 400
    end)
  end

  test "bad conflicts" do
    bad_conflicts = [nil, 1.2, "no limit!", %{"foo" => "bar"}, [2]]
    Enum.each(bad_conflicts, fn bc ->
      {:error, resp} = MangoDatabase.find(@db_name, %{"int" => %{"$gt" => 2}}, conflicts: bc)
      assert resp.status_code == 400
    end)
  end

  test "simple find" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$lt" => 35}})
    user_ids = Enum.map(docs, fn doc -> doc["user_id"] end)

    assert user_ids == [9, 1, 7]
  end

  test "multi cond and" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"manager" => true, "location.city" => "Longbranch"})

    user_id = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert user_id == [7]
  end

  test "multi cond duplicate field" do
    # need to explicitly define JSON as dict won't allow duplicate keys
    body = ~s({
      "selector": {
        "location.city": {"$regex": "^L+"},
        "location.city": {"$exists": true}
      }
    })
    resp = Couch.post("/#{@db_name}/_find", body: body)
    # expectation is that only the second instance
    # of the "location.city" field is used
    assert length(resp.body["docs"]) == 15
  end

  test "multi cond or" do
    {:ok, docs} = MangoDatabase.find(
      @db_name,
      %{
        "$and" => [
          %{"age" => %{"$gte" => 75}},
          %{"$or" => [%{"name.first" => "Mathis"}, %{"name.first" => "Whitley"}]},
        ]
      }
    )

    user_id = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert user_id == [11, 13]
  end

  test "multi col idx" do
    {:ok, docs} = MangoDatabase.find(
      @db_name,
        %{
          "location.state" => %{"$and" => [%{"$gt" => "Hawaii"}, %{"$lt" => "Maine"}]},
          "location.city" => %{"$lt" => "Longbranch"},
        }
    )

    user_id = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert user_id == [6]
  end

  test "missing not indexed" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"favorites.3" => "C"})
    user_id = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert user_id == [6]

    {:ok, docs} = MangoDatabase.find(@db_name, %{"favorites.3" => nil})
    assert Enum.empty?(docs)

    {:ok, docs} = MangoDatabase.find(@db_name, %{"twitter" => %{"$gt" => nil}})
    user_id = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert user_id == [1, 4, 0, 13]
  end

  test "limit" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$gt" => 0}})
    assert length(docs) == 15

    Enum.each([0, 1, 5, 14], fn l ->
      {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$gt" => 0}}, limit: l, sort: [%{"age" => "asc"}])
      assert length(docs) == l
    end)
  end

  test "skip" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$gt" => 0}})
    assert length(docs) == 15

    Enum.each([0, 1, 5, 14], fn s ->
      {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$gt" => 0}}, skip: s, sort: [%{"age" => "asc"}])
      assert length(docs) == (15 - s)
    end)
  end

  test "sort" do
    {:ok, docs1} = MangoDatabase.find(@db_name, %{"age" => %{"$gt" => 0}}, sort: [%{"age" => "asc"}])
    docs2 = Enum.sort_by(docs1, fn d -> d["age"] end)
    assert docs1 == docs2

    {:ok, docs1} = MangoDatabase.find(@db_name, %{"age" => %{"$gt" => 0}}, sort: [%{"age" => "desc"}])
    docs2 = Enum.reverse(Enum.sort_by(docs1, fn d -> d["age"] end))
    assert docs1 == docs2
  end

  test "sort desc complex" do
    {:ok, docs} = MangoDatabase.find(@db_name,
      %{
        "company" => %{"$lt" => "M"},
        "$or" => [%{"company" => "Dreamia"}, %{"manager" => true}],
      },
      sort: [%{"company" => "desc"}, %{"manager" => "desc"}],
    )
    companies_returned = Enum.map(docs, fn doc -> doc["company"] end)
    desc_companies = Enum.sort(companies_returned, :desc)
    assert desc_companies == companies_returned
  end

  test "sort with primary sort not in selector" do
    {:error, resp} = MangoDatabase.find(@db_name,
      %{"name.last" => %{"$lt" => "M"}},
      sort: [%{"name.first" => "desc"}]
    )
    assert resp.status_code == 400
    assert resp.body["error"] == "no_usable_index"
  end

  test "sort exists true" do
    {:ok, docs1} = MangoDatabase.find(@db_name,
      %{"age" => %{"$gt" => 0, "$exists" => true}},
      sort: [%{"age" => "asc"}]
    )
    docs2 = Enum.sort_by(docs1, fn d -> d["age"] end)
    assert docs1 == docs2
  end

  test "sort desc complex error" do
    {:error, resp} = MangoDatabase.find(@db_name,
      %{
        "company" => %{"$lt" => "M"},
        "$or": [%{"company" => "Dreamia"}, %{"manager" => true}],
      },
      sort: [%{"company" => "desc"}],
    )
    assert resp.status_code == 400
    assert resp.body["error"] == "no_usable_index"
  end

  test "fields" do
    selector = %{"age" => %{"$gt" => 0}}
    {:ok, docs} = MangoDatabase.find(@db_name, selector, fields: ["user_id", "location.address"])
    Enum.each(docs, fn d ->
      assert Enum.sort(Map.keys(d)) == ["location", "user_id"]
      assert Enum.sort(Map.keys(d["location"])) == ["address"]
    end)
  end

  test "r" do
    Enum.each([1, 2, 3], fn r ->
      {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$gt" => 0}}, r: r)
      assert length(docs) == 15
    end)
  end

  test "empty" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{})
    # 15 users
    assert length(docs) == 15
  end

  test "empty subsel" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"_id" => %{"$gt" => nil}, "location" => %{}})
    assert Enum.empty?(docs)
  end

  test "empty subsel match" do
    resp = MangoDatabase.save_docs(@db_name, [%{"user_id" => "eo", "empty_obj" => %{}}])
    {:ok, docs} = MangoDatabase.find(@db_name, %{"_id" => %{"$gt" => nil}, "empty_obj" => %{}})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == "eo"
  end

  test "unsatisfiable range" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"$and" => [%{"age" => %{"$gt" => 0}}, %{"age" => %{"$lt" => 0}}]})
    assert Enum.empty?(docs)
  end

  test "explain view args" do
    {:ok, explain} = MangoDatabase.find(@db_name, %{"age" => %{"$gt" => 0}}, fields: ["manager"], explain: true)
    assert explain["mrargs"]["stable"] == false
    assert explain["mrargs"]["update"] == true
    assert explain["mrargs"]["reduce"] == false
    assert explain["mrargs"]["start_key"] == [0]
    assert explain["mrargs"]["end_key"] == ["<MAX>"]
    assert explain["mrargs"]["include_docs"] == true
  end

  test "explain options" do
    {:ok, explain} = MangoDatabase.find(@db_name, %{"age" => %{"$gt" => 0}}, fields: ["manager"], explain: true)
    opts = explain["opts"]
    assert opts["r"] == 1
    assert opts["limit"] == 25
    assert opts["skip"] == 0
    assert opts["fields"] == ["manager"]
    assert opts["sort"] == %{}
    assert opts["bookmark"] == "nil"
    assert opts["conflicts"] == false
    assert opts["execution_stats"] == false
    assert opts["partition"] == ""
    assert opts["stable"] == false
    assert opts["stale"] == false
    assert opts["update"] == true
    assert opts["use_index"] == []
    assert opts["allow_fallback"] == true
  end

  test "sort with all docs" do
    {:ok, explain} = MangoDatabase.find(@db_name,
      %{"_id" => %{"$gt" => 0}, "age" => %{"$gt" => 0}},
      sort: ["_id"], explain: true
    )
    assert explain["index"]["type"] == "special"
  end

end

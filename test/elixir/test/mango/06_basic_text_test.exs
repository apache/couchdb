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

defmodule TextIndexCheckTests do
  use CouchTestCase

  @db_name "text-index-check"
  @users_db "_users"

  @moduletag config: [
    {
      "chttpd_auth",
      "authentication_db",
      @users_db
    },
    {
      "couch_httpd_auth",
      "authentication_db",
      @users_db
    },
    {
      "chttpd_auth",
      "iterations",
      "1"
    },
    {
      "admins",
      "jan",
      "apple"
    }
  ]

  setup do
    MangoDatabase.recreate(@db_name)
    MangoDatabase.recreate(@users_db)
    :ok
  end

  test "create text index" do
    sess = Couch.login("jan", "apple")

    resp = Couch.Session.post(
      sess,
      "/#{@db_name}/_index",
      body: %{"index" => %{}, "type" => "text"}
    )

    assert resp.status_code == 503
    assert resp
  end
end

defmodule BasicTextTests do
  use CouchTestCase

  @db_name "basic-text-elem-match"

  setup do
    UserDocs.setup(@db_name, "text")
  end

  test "simple" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"$text" => "Stephanie"})

    assert length(docs) == 1
    assert Enum.at(docs, 0)["name"]["first"] == "Stephanie"
  end

  test "with integer" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"name.first" => "Stephanie", "age" => 48})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["name"]["first"] == "Stephanie"
    assert Enum.at(docs, 0)["age"] == 48
  end

  test "with boolean" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"name.first" => "Stephanie", "manager" => false})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["name"]["first"] == "Stephanie"
    assert Enum.at(docs, 0)["manager"] == false
  end

  test "with array" do
    faves = ["Ruby", "C", "Python"]
    {:ok, docs} = MangoDatabase.find(@db_name, %{"name.first" => "Stephanie", "favorites" => faves})
    assert Enum.at(docs, 0)["name"]["first"] == "Stephanie"
    assert Enum.at(docs, 0)["favorites"] == faves
  end

  test "array ref" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"favorites.1" => "Python"})
    assert length(docs) == 4
    assert Enum.all?(Enum.map(docs, & &1["favorites"]), fn fav -> Enum.member?(fav, "Python") end)

    # Nested Level
    {:ok, docs} = MangoDatabase.find(@db_name, %{"favorites.0.2" => "Python"})
    assert length(docs) == 1
    nested_favorite = Enum.map(docs, fn d -> d["favorites"] |> Enum.at(0) |> Enum.at(2) end)
    assert Enum.at(nested_favorite, 0) == "Python"
  end

  test "number ref" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"11111" => "number_field"})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["11111"] == "number_field"

    {:ok, docs} = MangoDatabase.find(@db_name, %{"22222.33333" => "nested_number_field"})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["22222"]["33333"] == "nested_number_field"
  end

  test "lt" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$lt" => 22}})
    assert assert Enum.empty?(docs)

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$lt" => 23}})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 9

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$lt" => 33}})
    user_ids = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert Enum.sort(user_ids) == [1, 9]

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$lt" => 34}})
    assert length(docs) == 3

    user_ids = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert Enum.sort(user_ids) == [1, 7, 9]

    {:ok, docs} = MangoDatabase.find(@db_name, %{"company" => %{"$lt" => "Dreamia"}})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["company"] == "Affluex"

    {:ok, docs} = MangoDatabase.find(@db_name, %{"foo" => %{"$lt" => "bar car apple"}})
    assert assert Enum.empty?(docs)
  end

  test "lte" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$lte" => 21}})
    assert assert Enum.empty?(docs)

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$lte" => 22}})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 9

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$lte" => 33}})
    user_ids = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert Enum.sort(user_ids) == [1, 7, 9]

    {:ok, docs} = MangoDatabase.find(@db_name, %{"company" => %{"$lte" => "Dreamia"}})
    user_ids = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert user_ids == [0, 11]

    {:ok, docs} = MangoDatabase.find(@db_name, %{"foo" => %{"$lte" => "bar car apple"}})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 14
  end

  test "eq" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => 21})
    assert assert Enum.empty?(docs)

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => 22})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 9

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$eq" => 22}})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 9

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => 33})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 7
  end

  test "ne" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$ne" => 22}})
    assert length(docs) == UserDocs.len() - 1
    assert Enum.all?(docs, fn doc -> doc["age"] != 22 end)

    {:ok, docs} = MangoDatabase.find(@db_name, %{"$not" => %{"age" => 22}})
    assert length(docs) == UserDocs.len() - 1
    assert Enum.all?(docs, fn doc -> doc["age"] != 22 end)
  end

  test "gt" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$gt" => 77}})
    user_ids = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert Enum.sort(user_ids) == [3, 13]

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$gt" => 78}})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 3

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$gt" => 79}})
    assert assert Enum.empty?(docs)

    {:ok, docs} = MangoDatabase.find(@db_name, %{"company" => %{"$gt" => "Zialactic"}})
    assert assert Enum.empty?(docs)

    {:ok, docs} = MangoDatabase.find(@db_name, %{"foo" => %{"$gt" => "bar car apple"}})
    assert assert Enum.empty?(docs)

    {:ok, docs} = MangoDatabase.find(@db_name, %{"foo" => %{"$gt" => "bar car"}})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 14
  end

  test "gte" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$gte" => 77}})
    user_ids = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert Enum.sort(user_ids) == [3, 13]

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$gte" => 78}})
    user_ids = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert Enum.sort(user_ids) == [3, 13]

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$gte" => 79}})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 3

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$gte" => 80}})
    assert assert Enum.empty?(docs)

    {:ok, docs} = MangoDatabase.find(@db_name, %{"company" => %{"$gte" => "Zialactic"}})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["company"] == "Zialactic"

    {:ok, docs} = MangoDatabase.find(@db_name, %{"foo" => %{"$gte" => "bar car apple"}})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 14
  end

  test "and" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => 22, "manager" => true})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 9

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => 22, "manager" => false})
    assert assert Enum.empty?(docs)

    {:ok, docs} = MangoDatabase.find(@db_name, %{"$and" => [%{"age" => 22}, %{"manager" => true}]})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 9

    {:ok, docs} = MangoDatabase.find(@db_name, %{"$and" => [%{"age" => 22}, %{"manager" => false}]})
    assert assert Enum.empty?(docs)

    {:ok, docs} = MangoDatabase.find(@db_name, %{"$text" => "Ramona", "age" => 22})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 9

    {:ok, docs} = MangoDatabase.find(@db_name, %{"$and" => [%{"$text" => "Ramona"}, %{"age" => 22}]})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 9

    {:ok, docs} = MangoDatabase.find(@db_name, %{"$and" => [%{"$text" => "Ramona"}, %{"$text" => "Floyd"}]})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 9
  end

  test "or" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"$or" => [%{"age" => 22}, %{"age" => 33}]})
    user_ids = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert Enum.sort(user_ids) == [7, 9]

    q = %{"$or" => [%{"$text" => "Ramona"}, %{"$text" => "Stephanie"}]}
    {:ok, docs} = MangoDatabase.find(@db_name, q)
    user_ids = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert Enum.sort(user_ids) == [0, 9]

    q = %{"$or" => [%{"$text" => "Ramona"}, %{"age" => 22}]}
    {:ok, docs} = MangoDatabase.find(@db_name, q)
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 9
  end

  test "and or" do
    q = %{"age" => 22, "$or" => [%{"manager" => false}, %{"location.state" => "Missouri"}]}
    {:ok, docs} = MangoDatabase.find(@db_name, q)
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 9

    q = %{"$or" => [%{"age" => 22}, %{"age" => 43, "manager" => true}]}
    {:ok, docs} = MangoDatabase.find(@db_name, q)
    user_ids = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert Enum.sort(user_ids) == [9, 10]

    q = %{"$or" => [%{"$text" => "Ramona"}, %{"age" => 43, "manager" => true}]}
    {:ok, docs} = MangoDatabase.find(@db_name, q)
    user_ids = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert Enum.sort(user_ids) == [9, 10]
  end

  test "nor" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"$nor" => [%{"age" => 22}, %{"age" => 33}]})
    assert length(docs) == 13
    assert Enum.all?(docs, fn doc ->
      not Enum.member?([7, 9], doc["user_id"])
    end)
  end

  test "in with value" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$in" => [1, 5]}})
    assert assert Enum.empty?(docs)

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$in" => [1, 5, 22]}})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 9

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$in" => [1, 5, 22, 31]}})
    user_ids = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert Enum.sort(user_ids) == [1, 9]

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$in" => [22, 31]}})
    user_ids = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert Enum.sort(user_ids) == [1, 9]

    # Limits on boolean clauses?
    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$in" => Enum.to_list(0..999)}})
    assert length(docs) == 15
  end

  test "in with array" do
    vals = ["Random Garbage", 52, %{"Versions" => %{"Alpha" => "Beta"}}]
    {:ok, docs} = MangoDatabase.find(@db_name, %{"favorites" => %{"$in" => vals}})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 1

    vals = ["Lisp", "Python"]
    {:ok, docs} = MangoDatabase.find(@db_name, %{"favorites" => %{"$in" => vals}})
    assert length(docs) == 10

    vals = [%{"val1" => 1, "val2" => "val2"}]
    {:ok, docs} = MangoDatabase.find(@db_name, %{"test_in" => %{"$in" => vals}})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 2
  end

  test "nin with value" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$nin" => [1, 5]}})
    assert length(docs) == UserDocs.len()

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$nin" => [1, 5, 22]}})
    assert length(docs) == UserDocs.len() - 1
    assert Enum.all?(docs, fn doc -> doc["user_id"] != 9 end)

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$nin" => [1, 5, 22, 31]}})
    assert length(docs) == UserDocs.len() - 2
    assert Enum.all?(docs, fn doc ->
      not Enum.member?([1, 9], doc["user_id"])
    end)

    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$nin" => [22, 31]}})
    assert length(docs) == UserDocs.len() - 2
    assert Enum.all?(docs, fn doc ->
      not Enum.member?([1, 9], doc["user_id"])
    end)

    # Limits on boolean clauses?
    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$nin" =>  Enum.to_list(0..1000)}})
    assert assert Enum.empty?(docs)
  end

  test "nin with array" do
    vals = ["Random Garbage", 52, %{"Versions" => %{"Alpha" => "Beta"}}]
    {:ok, docs} = MangoDatabase.find(@db_name, %{"favorites" => %{"$nin" => vals}})
    assert length(docs) == UserDocs.len() - 1
    assert Enum.all?(docs, fn doc -> doc["user_id"] != 1 end)

    vals = ["Lisp", "Python"]
    {:ok, docs} = MangoDatabase.find(@db_name, %{"favorites" => %{"$nin" => vals}})
    assert length(docs) == 5

    vals = [%{"val1" => 1, "val2" => "val2"}]
    {:ok, docs} = MangoDatabase.find(@db_name, %{"test_in" => %{"$nin" => vals}})
    assert assert Enum.empty?(docs)
  end

  test "all" do
    vals = ["Ruby", "C", "Python", %{"Versions" => %{"Alpha" => "Beta"}}]
    {:ok, docs} = MangoDatabase.find(@db_name, %{"favorites" => %{"$all" => vals}})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 1

    # This matches where favorites either contains
    # the nested array, or is the nested array. This is
    # notably different than the non-nested array in that
    # it does not match a re-ordered version of the array.
    # The fact that user_id 14 isn't included demonstrates
    # this behavior.
    vals = [["Lisp", "Erlang", "Python"]]
    {:ok, docs} = MangoDatabase.find(@db_name, %{"favorites" => %{"$all" => vals}})
    assert length(docs) == 2
    assert Enum.all?(docs, fn doc ->
      Enum.member?([3, 9], doc["user_id"])
    end)
  end

  test "exists field" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"exists_field" => %{"$exists" => true}})
    assert length(docs) == 2
    assert Enum.all?(docs, fn doc ->
      Enum.member?([7, 8], doc["user_id"])
    end)

    {:ok, docs} = MangoDatabase.find(@db_name, %{"exists_field" => %{"$exists" => false}})
    assert length(docs) == UserDocs.len() - 2
    assert Enum.all?(docs, fn doc ->
      not Enum.member?([7, 8], doc["user_id"])
    end)
  end

  test "exists array" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"exists_array" => %{"$exists" => true}})
    assert length(docs) == 2
    assert Enum.all?(docs, fn doc ->
      Enum.member?([9, 10], doc["user_id"])
    end)

    {:ok, docs} = MangoDatabase.find(@db_name, %{"exists_array" => %{"$exists" => false}})
    assert length(docs) == UserDocs.len() - 2
    assert Enum.all?(docs, fn doc ->
      not Enum.member?([9, 10], doc["user_id"])
    end)
  end

  test "exists object" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"exists_object" => %{"$exists" => true}})
    assert length(docs) == 2
    assert Enum.all?(docs, fn doc ->
      Enum.member?([11, 12], doc["user_id"])
    end)

    {:ok, docs} = MangoDatabase.find(@db_name, %{"exists_object" => %{"$exists" => false}})
    assert length(docs) == UserDocs.len() - 2
    assert Enum.all?(docs, fn doc ->
      not Enum.member?([11, 12], doc["user_id"])
    end)
  end

  test "exists object member" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"exists_object.should" => %{"$exists" => true}})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 11

    {:ok, docs} = MangoDatabase.find(@db_name, %{"exists_object.should" => %{"$exists" => false}})
    assert length(docs) == UserDocs.len() - 1
    assert Enum.all?(docs, fn doc -> doc["user_id"] != 11 end)
  end

  test "exists and" do
    q = %{
      "$and" => [
        %{"manager" => %{"$exists" => true}},
        %{"exists_object.should" => %{"$exists" => true}},
      ]
    }
    {:ok, docs} = MangoDatabase.find(@db_name, q)
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 11

    q = %{
      "$and" => [
        %{"manager" => %{"$exists" => false}},
        %{"exists_object.should" => %{"$exists" => true}},
      ]
    }
    {:ok, docs} = MangoDatabase.find(@db_name, q)
    assert assert Enum.empty?(docs)

    # Translates to manager exists or exists_object.should doesn't
    # exist, which will match all docs
    q = %{"$not" => q}
    {:ok, docs} = MangoDatabase.find(@db_name, q)
    assert length(docs) == UserDocs.len()
  end

  # escape the "
  test "value chars" do
    q = %{"complex_field_value" => "+-(){}[]^~&&*||\"\\/?:!"}
    {:ok, docs} = MangoDatabase.find(@db_name, q)
    assert length(docs) == 1
  end

  test "regex" do
    {:ok, docs} = MangoDatabase.find(@db_name,
      %{"age" => %{"$gt" => 40}, "location.state" => %{"$regex" => "(?i)new.*"}}
    )
    assert length(docs) == 2
    assert Enum.at(docs, 0)["user_id"] == 2
    assert Enum.at(docs, 1)["user_id"] == 10
  end
end

defmodule ElemMatchTests do
  use CouchTestCase

  @db_name "basic-text-elem-match"

  setup do
    FriendDocs.setup(@db_name, "text")
  end

  test "elem match non object" do
    q = %{"bestfriends" => %{"$elemMatch" => %{"$eq" => "Wolverine", "$eq" => "Cyclops"}}}
    {:ok, docs} = MangoDatabase.find(@db_name, q)
    assert length(docs) == 1
    assert Enum.at(docs, 0)["bestfriends"] == ["Wolverine", "Cyclops"]

    q = %{"results" => %{"$elemMatch" => %{"$gte" => 80, "$lt" => 85}}}
    {:ok, docs} = MangoDatabase.find(@db_name, q)
    assert length(docs) == 1
    assert Enum.at(docs, 0)["results"] == [82, 85, 88]
  end

  test "elem match" do
    q = %{"friends" => %{"$elemMatch" => %{"name.first" => "Vargas"}}}
    {:ok, docs} = MangoDatabase.find(@db_name, q)
    user_ids = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert Enum.sort(user_ids) == [0, 1]

    q = %{"friends" => %{"$elemMatch" => %{"name.first" => "Ochoa", "name.last" => "Burch"}}}
    {:ok, docs} = MangoDatabase.find(@db_name, q)
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 4

    # Check that we can do logic in elemMatch
    q = %{"friends" => %{"$elemMatch" => %{"name.first" => "Ochoa", "type" => "work"}}}
    {:ok, docs} = MangoDatabase.find(@db_name, q)
    user_ids = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert Enum.sort(user_ids) == [1, 15]

    q = %{
      "friends" => %{
        "$elemMatch" => %{
          "name.first" => "Ochoa",
          "$or" => [%{"type" => "work"}, %{"type" => "personal"}],
        }
      }
    }
    {:ok, docs} = MangoDatabase.find(@db_name, q)
    user_ids = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert Enum.sort(user_ids) == [1, 4, 15]

    # Same as last, but using $in
    q = %{
      "friends" => %{
        "$elemMatch" => %{
          "name.first" => "Ochoa",
          "type" => %{"$in" => ["work", "personal"]},
        }
      }
    }
    {:ok, docs} = MangoDatabase.find(@db_name, q)
    user_ids = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert Enum.sort(user_ids) == [1, 4, 15]

    q = %{
      "$and" => [
        %{
          "friends" => %{
            "$elemMatch" => %{
              "id" => 0,
              "name" => %{"$exists" => true}
            }
          }
        },
        %{
          "friends" => %{
            "$elemMatch" => %{
              "$or" => [
                %{"name" => %{"first" => "Campos", "last" => "Freeman"}},
                %{
                  "name" => %{
                    "$in" => [
                      %{"first" => "Gibbs", "last" => "Mccarty"},
                      %{"first" => "Wilkins", "last" => "Chang"}
                    ]
                  }
                }
              ]
            }
          }
        }
      ]
    }

    {:ok, docs} = MangoDatabase.find(@db_name, q)
    user_ids = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert Enum.sort(user_ids) == [10, 11, 12]
  end
end

defmodule AllMatchTests do
  use CouchTestCase

  @db_name "basic-text-elem-match"

  setup do
    FriendDocs.setup(@db_name, "text")
  end

  test "all match" do
    q = %{"friends" => %{"$allMatch" => %{"type" => "personal"}}}
    {:ok, docs} = MangoDatabase.find(@db_name, q)
    user_ids = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert Enum.sort(user_ids) == [5, 8]

    # Check that we can do logic in allMatch
    q = %{
      "friends" => %{
        "$allMatch" => %{
          "name.first" => "Ochoa",
          "$or" => [%{"type" => "work"}, %{"type" => "personal"}],
        }
      }
    }
    {:ok, docs} = MangoDatabase.find(@db_name, q)
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 15

    # Same as last, but using $in
    q = %{
      "friends" => %{
        "$allMatch" => %{
          "name.first" => "Ochoa",
          "type" => %{"$in" => ["work", "personal"]},
        }
      }
    }
    {:ok, docs} = MangoDatabase.find(@db_name, q)
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 15
  end
end

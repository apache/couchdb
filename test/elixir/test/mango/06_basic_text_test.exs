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

defmodule BasicTextTest do
  use CouchTestCase

  @db_name "basic-text"

  setup do
    UserDocs.setup(@db_name, "text")
  end

  test "explain options" do
    {:ok, explain} = MangoDatabase.find(
      @db_name,
      %{"age" => %{"$gt" => 0}},
      fields: ["manager"],
      allow_fallback: false,
      explain: true
    )
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
    assert opts["allow_fallback"] == false
  end

  test "explain with bookmarks" do
    query = %{"age" => %{"$gt" => 42}}

    {:ok, resp} = MangoDatabase.find(
      @db_name,
      query,
      limit: 1,
      allow_fallback: false,
      return_raw: true
    )
    assert length(resp["docs"]) == 1
    assert resp["bookmark"] != "nil"
    {:ok, explain} = MangoDatabase.find(
      @db_name,
      query,
      bookmark: resp["bookmark"],
      allow_fallback: false,
      explain: true
    )
    assert is_binary(explain["opts"]["bookmark"])
    assert resp["bookmark"] == explain["opts"]["bookmark"]
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
end

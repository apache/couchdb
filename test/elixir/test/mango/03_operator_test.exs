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

defmodule OperatorTest do
  use CouchTestCase

  @db_name "operator"

  setup do
    UserDocs.setup(@db_name)
  end

  test "all" do
    q = %{"manager" => true, "favorites" => %{"$all" => ["Lisp", "Python"]}}
    {:ok, docs} = MangoDatabase.find(@db_name, q)

    assert length(docs) == 3
    user_ids = Enum.map(docs, fn doc -> doc["user_id"] end)
    assert user_ids == [2, 12, 9]
  end

  test "all non array" do
    q = %{"manager" => true, "location" => %{"$all" => ["Ohai"]}}
    {:ok, docs} = MangoDatabase.find(@db_name, q)

    assert Enum.empty?(docs)
  end

  test "elem match" do
    emdocs = [
      %{"user_id" => "a", "bang" => [%{"foo" => 1, "bar" => 2}]},
      %{"user_id" => "b", "bang" => [%{"foo" => 2, "bam" => true}]},
    ]
    MangoDatabase.save_docs(@db_name, emdocs)

    q = %{
      "_id" => %{"$gt" => nil},
      "bang" => %{"$elemMatch" => %{"foo": %{"$gte": 1}, "bam": true}}
    }
    {:ok, docs} = MangoDatabase.find(@db_name, q)

    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == "b"
  end

  test "all match" do
    amdocs = [
      %{"user_id" => "a", "bang" => [%{"foo" => 1, "bar" => 2}, %{"foo" => 3, "bar" => 4}]},
      %{"user_id" => "b", "bang" => [%{"foo" => 1, "bar" => 2}, %{"foo" => 4, "bar" => 4}]},
    ]
    MangoDatabase.save_docs(@db_name, amdocs)

    q = %{
      "bang" => %{"$allMatch" => %{"foo" => %{"$mod" => [2, 1]}, "bar" => %{"$mod" => [2, 0]}}}
    }
    {:ok, docs} = MangoDatabase.find(@db_name, q)

    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == "a"
  end

  test "empty all match" do
    amdocs = [
      %{"bad_doc" => "a", "emptybang" => []},
    ]
    MangoDatabase.save_docs(@db_name, amdocs)

    q = %{
      "bang" => %{"emptybang" => %{"$allMatch" => %{"foo" => %{"$eq" => 2}}}}
    }
    {:ok, docs} = MangoDatabase.find(@db_name, q)

    assert Enum.empty?(docs)
  end

  test "in operator array" do
    q = %{
      "manager" => true, "favorites" => %{"$in" => ["Ruby", "Python"]}
    }
    {:ok, docs} = MangoDatabase.find(@db_name, q)

    assert length(docs) == 6
    user_ids = Enum.sort(Enum.map(docs, fn doc -> doc["user_id"] end))
    assert user_ids == [2, 6, 7, 9, 11, 12]
  end

  test "nin operator array" do
    q = %{
      "manager" => true, "favorites" => %{"$nin" => ["Erlang", "Python"]}
    }
    {:ok, docs} = MangoDatabase.find(@db_name, q)

    assert length(docs) == 4
    for doc <- docs do
      if is_list(doc["favorites"]) do
        refute "Erlang" in doc["favorites"]
        refute "Python" in doc["favorites"]
      end
    end
  end

  test "regex" do
    q = %{
      "age" => %{"$gt" => 40}, "location.state" => %{"$regex" => "(?i)new.*"}
    }
    {:ok, docs} = MangoDatabase.find(@db_name, q)

    assert length(docs) == 2
    user_ids = Enum.sort(Enum.map(docs, fn doc -> doc["user_id"] end))
    assert user_ids == [2, 10]
  end

  test "exists false" do
    q = %{
      "age" => %{"$gt" => 0}, "twitter" => %{"$exists" => false}
    }
    {:ok, docs} = MangoDatabase.find(@db_name, q)

    assert length(docs) == 10
    user_ids = Enum.sort(Enum.map(docs, fn doc -> doc["user_id"] end))
    assert user_ids == [2, 3, 5, 6, 7, 8, 10, 11, 12, 14]

    for doc <- docs do
      refute "twitter" in doc
    end
  end

  test "eq null does not include missing" do
    q = %{"age" => %{"$gt" => 0}, "twitter" => nil}
    {:ok, docs} = MangoDatabase.find(@db_name, q)

    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 9
    assert Enum.at(docs, 0)["twitter"] == nil
  end

  test "ne includes null but not missing" do
    q = %{"twitter" => %{"$ne" => "notamatch"}}
    {:ok, docs} = MangoDatabase.find(@db_name, q)
    user_ids = Enum.sort(Enum.map(docs, fn doc -> doc["user_id"] end))

    assert length(docs) == 5
    assert user_ids == [0, 1, 4, 9, 13]

    for doc <- docs do
      assert Map.has_key?(doc, "twitter")
    end
  end

  test "lte includes null but not missing" do
    q = %{"twitter" => %{"$lte" => nil}}
    {:ok, docs} = MangoDatabase.find(@db_name, q)

    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 9
    assert Enum.at(docs, 0)["twitter"] == nil
  end

  test "lte at z except null excludes null and missing" do
    q = %{
      "twitter" => %{
        "$and" => [%{"$lte" => "@z"}, %{"$ne" => nil}]
      }
    }
    {:ok, docs} = MangoDatabase.find(@db_name, q)
    user_ids = Enum.sort(Enum.map(docs, fn doc -> doc["user_id"] end))

    assert length(docs) == 4
    assert user_ids == [0, 1, 4, 13]
    for doc <- docs do
      refute doc["twitter"] == nil
    end
  end

  test "range gte null includes null but not missing" do
    q = %{"twitter" => %{"$gte" => nil}}
    {:ok, docs} = MangoDatabase.find(@db_name, q)

    assert length(docs) == 5
    for doc <- docs do
      assert Map.has_key?(doc, "twitter")
    end
  end

  test "exists false returns missing but not null" do
    q = %{"twitter" => %{"$exists" => false}}
    {:ok, docs} = MangoDatabase.find(@db_name, q)

    assert length(docs) == 10
    for doc <- docs do
      refute Map.has_key?(doc, "twitter")
    end
  end

  test "beginsWith" do
    MangoDatabase.save_docs(@db_name, [
      %{"user_id" => 99, "location" => %{"state" => ":Bar"}}
    ])

    cases = [
      %{prefix: "New", user_ids: [2, 10]},
      # test characters that require escaping
      %{prefix: "New ", user_ids: [2, 10]},
      %{prefix: ":", user_ids: [99]},
      %{prefix: "Foo", user_ids: []},
      %{prefix: "\"Foo", user_ids: []},
      %{prefix: " New", user_ids: []}
    ]

    for case <- cases do
      selector = %{"location.state" => %{"$beginsWith" => case.prefix}}
      {:ok, docs} = MangoDatabase.find(@db_name, selector)
      user_ids = Enum.sort(Enum.map(docs, fn doc -> doc["user_id"] end))

      assert length(docs) == length(case.user_ids)
      assert case.user_ids == user_ids
    end
  end

  # non-string prefixes should return an error
  test "beginsWith invalid prefix" do
    cases = [123, true, [], %{}]

    for prefix <- cases do
      q = %{"location.state" => %{"$beginsWith" => prefix}}
      {:error, response} = MangoDatabase.find(@db_name, q)
      assert response.status_code == 400
    end
  end
end

defmodule OperatorJSONTests do
  use CouchTestCase

  @db_name "operator"

  setup do
    UserDocs.setup(@db_name)
  end

  # START: text indexes do not support range queries across type boundaries so only
  # test this for JSON indexes
  test "lt includes null but not missing" do
    q = %{"twitter" => %{"$lt" => 1}}
    {:ok, docs} = MangoDatabase.find(@db_name, q)

    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 9
    assert Enum.at(docs, 0)["twitter"] == nil
  end

  test "lte includes null but not missing" do
    q = %{"twitter" => %{"$lte" => 1}}
    {:ok, docs} = MangoDatabase.find(@db_name, q)

    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 9
    assert Enum.at(docs, 0)["twitter"] == nil
  end

  test "lte respects unicode collation" do
    q = %{"ordered" => %{"$lte" => "a"}}
    {:ok, docs} = MangoDatabase.find(@db_name, q)

    assert length(docs) == 6
    user_ids = Enum.sort(Enum.map(docs, fn doc -> doc["user_id"] end))
    assert user_ids == [7, 8, 9, 10, 11, 12]
  end

  test "gte respects unicode collation" do
    q = %{"ordered" => %{"$gte" => "a"}}
    {:ok, docs} = MangoDatabase.find(@db_name, q)

    assert length(docs) == 3
    user_ids = Enum.sort(Enum.map(docs, fn doc -> doc["user_id"] end))
    assert user_ids == [12, 13, 14]
  end

  # $keyMapMatch operator is only supported for JSON indexes
  test "keymap match" do
    amdocs = [
      %{"foo" => %{"aa" => "bar", "bb" => "bang"}},
      %{"foo" => %{"cc" => "bar", "bb" => "bang"}},
    ]

    MangoDatabase.save_docs(@db_name, amdocs)

    q = %{"foo" => %{"$keyMapMatch" => %{"$eq" => "aa"}}}
    {:ok, docs} = MangoDatabase.find(@db_name, q)

    assert length(docs) == 1
  end
end

defmodule OperatorAllDocsTests do
  use CouchTestCase

  @db_name "operator"

  setup do
    UserDocs.setup(@db_name, "special")
  end

  test "range id eq" do
    doc_id = "8e1c90c0-ac18-4832-8081-40d14325bde0"
    q = %{"_id" => doc_id}
    {:ok, explain} = MangoDatabase.find(@db_name, q, return_raw: true, explain: true)

    assert explain["mrargs"]["end_key"] == doc_id
    assert explain["mrargs"]["start_key"] == doc_id
  end
end

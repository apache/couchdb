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

defmodule IndexSelectorJson do
  use CouchTestCase

  @db_name "index-selectors"
  @docs [
    %{"_id" => "100", "name" => "Jimi", "location" => "AUS", "user_id" => 1, "same" => "value"},
    %{"_id" => "200", "name" => "Eddie", "location" => "BRA", "user_id" => 2, "same" => "value"},
    %{"_id" => "300", "name" => "Harry", "location" => "CAN", "user_id" => 3, "same" => "value"},
    %{"_id" => "400", "name" => "Eddie", "location" => "DEN", "user_id" => 4, "same" => "value"},
    %{"_id" => "500", "name" => "Jones", "location" => "ETH", "user_id" => 5, "same" => "value"},
    %{
      "_id" => "600",
      "name" => "Winnifried",
      "location" => "FRA",
      "user_id" => 6,
      "same" => "value",
    },
    %{"_id" => "700", "name" => "Marilyn", "location" => "GHA", "user_id" => 7, "same" => "value"},
    %{"_id" => "800", "name" => "Sandra", "location" => "ZAR", "user_id" => 8, "same" => "value"},
  ]

  setup do
    MangoDatabase.recreate(@db_name)
    MangoDatabase.save_docs(@db_name, @docs)
    :ok
  end

  test "saves partial filter selector in index" do
    selector = %{"location" => %{"$gte" => "FRA"}}
    MangoDatabase.create_index(@db_name, ["location"], partial_filter_selector: selector)

    {:ok, indexes} = MangoDatabase.list_indexes(@db_name)
    assert Enum.at(indexes, 1)["def"]["partial_filter_selector"] == selector
  end

  test "partial filter only in return if not default" do
    MangoDatabase.create_index(@db_name, ["location"])

    {:ok, indexes} = MangoDatabase.list_indexes(@db_name)
    assert Enum.at(indexes, 1)["def"]["partial_filter_selector"] == nil
  end

  test "saves selector in index throws" do
    selector = %{"location" => %{"$gte" => "FRA"}}

    case MangoDatabase.create_index(@db_name, ["location"], selector: selector) do
      {:error, %{status_code: 400}} ->
        :ok
      {:ok, _} ->
        raise "bad index creation"
    end
  end

  test "uses partial index for query selector" do
    selector = %{"location" => %{"$gte" => "FRA"}}
    MangoDatabase.create_index(
      @db_name,
      ["location"],
      partial_filter_selector: selector,
      ddoc: "Selected",
      name: "Selected"
    )

    {:ok, resp} = MangoDatabase.find(@db_name, selector, explain: true, use_index: "Selected")
    assert resp["index"]["name"] == "Selected"
    {:ok, docs} = MangoDatabase.find(@db_name, selector, use_index: "Selected")
    assert length(docs) == 3
  end

  test "uses partial index with different selector" do
    selector = %{"location" => %{"$gte" => "FRA"}}
    selector2 = %{"location" => %{"$gte" => "A"}}
    MangoDatabase.create_index(
      @db_name,
      ["location"],
      partial_filter_selector: selector,
      ddoc: "Selected",
      name: "Selected"
    )

    {:ok, resp} = MangoDatabase.find(@db_name, selector2, explain: true, use_index: "Selected")
    assert resp["index"]["name"] == "Selected"

    {:ok, docs} = MangoDatabase.find(@db_name, selector2, use_index: "Selected")
    assert length(docs) == 3
  end

  test "doesnot use selector when not specified" do
    selector = %{"location" => %{"$gte" => "FRA"}}
    MangoDatabase.create_index(
      @db_name,
      ["location"],
      partial_filter_selector: selector,
      ddoc: "Selected",
      name: "Selected"
    )

    {:ok, resp} = MangoDatabase.find(@db_name, selector, explain: true)
    assert resp["index"]["name"] == "_all_docs"
  end

  test "doesnot use selector when not specified with index" do
    selector = %{"location" => %{"$gte" => "FRA"}}
    MangoDatabase.create_index(
      @db_name,
      ["location"],
      partial_filter_selector: selector,
      ddoc: "Selected",
      name: "Selected"
    )
    MangoDatabase.create_index(
      @db_name,
      ["location"],
      name: "NotSelected"
    )

    {:ok, resp} = MangoDatabase.find(@db_name, selector, explain: true)
    assert resp["index"]["name"] == "NotSelected"
  end

  test "old selector with no selector still supported" do
    oldschoolnoselectorddoc = %{
      "_id" => "_design/oldschoolnoselector",
      "language" => "query",
      "views" => %{
        "oldschoolnoselector" => %{
          "map" => %{"fields" => %{"location" => "asc"}},
          "reduce" => "_count",
          "options" => %{"def" => %{"fields" => ["location"]}}
        }
      }
    }
    selector = %{"location" => %{"$gte" => "FRA"}}
    MangoDatabase.save_doc(@db_name, oldschoolnoselectorddoc)

    {:ok, resp} = MangoDatabase.find(@db_name, selector, explain: true, use_index: "oldschoolnoselector")
    assert resp["index"]["name"] == "oldschoolnoselector"
    {:ok, docs} = MangoDatabase.find(@db_name, selector, use_index: "oldschoolnoselector")
    assert length(docs) == 3
  end

  test "old selector still supported" do
    oldschoolddoc = %{
      "_id" => "_design/oldschool",
      "language" => "query",
      "views" => %{
        "oldschool" => %{
          "map" => %{
            "fields" => %{"location" => "asc"},
            "selector" => %{"location" => %{"$gte" => "FRA"}},
          },
          "reduce" => "_count",
          "options" => %{"def" => %{"fields" => ["location"]}}
        }
      }
    }
    selector = %{"location" => %{"$gte" => "FRA"}}
    MangoDatabase.save_doc(@db_name, oldschoolddoc)

    {:ok, resp} = MangoDatabase.find(@db_name, selector, explain: true, use_index: "oldschool")
    assert resp["index"]["name"] == "oldschool"
    {:ok, docs} = MangoDatabase.find(@db_name, selector, use_index: "oldschool")
    assert length(docs) == 3
  end

  test "uses partial index with non indexable selector" do
    partial_selector = %{"location" => %{"$gte" => "FRA"}}
    selector = %{"location" => %{"$exists" => true}}
    MangoDatabase.create_index(
      @db_name,
      ["location"],
      partial_filter_selector: partial_selector,
      ddoc: "Selected",
      name: "Selected"
    )

    {:ok, resp} = MangoDatabase.find(@db_name, selector, explain: true, use_index: "Selected")
    assert resp["index"]["name"] == "Selected"
    {:ok, docs} = MangoDatabase.find(@db_name, selector, use_index: "Selected")
    assert length(docs) == 3
  end
end

defmodule IndexSelectorText do
  use CouchTestCase

  @db_name "index-selectors"
  @docs [
    %{"_id" => "100", "name" => "Jimi", "location" => "AUS", "user_id" => 1, "same" => "value"},
    %{"_id" => "200", "name" => "Eddie", "location" => "BRA", "user_id" => 2, "same" => "value"},
    %{"_id" => "300", "name" => "Harry", "location" => "CAN", "user_id" => 3, "same" => "value"},
    %{"_id" => "400", "name" => "Eddie", "location" => "DEN", "user_id" => 4, "same" => "value"},
    %{"_id" => "500", "name" => "Jones", "location" => "ETH", "user_id" => 5, "same" => "value"},
    %{
      "_id" => "600",
      "name" => "Winnifried",
      "location" => "FRA",
      "user_id" => 6,
      "same" => "value",
    },
    %{"_id" => "700", "name" => "Marilyn", "location" => "GHA", "user_id" => 7, "same" => "value"},
    %{"_id" => "800", "name" => "Sandra", "location" => "ZAR", "user_id" => 8, "same" => "value"},
  ]

  setup do
    MangoDatabase.recreate(@db_name)
    MangoDatabase.save_docs(@db_name, @docs)
    :ok
  end

  test "saves partialfilterselector in index" do
    selector = %{"location" => %{"$gte" => "FRA"}}
    MangoDatabase.create_text_index(
      @db_name,
      fields: [%{"name" => "location", "type" => "string"}],
      partial_filter_selector: selector
    )

    {:ok, indexes} = MangoDatabase.list_indexes(@db_name)
    assert Enum.at(indexes, 1)["def"]["partial_filter_selector"] == selector
  end

  test "uses partial index for query selector" do
    selector = %{"location" => %{"$gte" => "FRA"}}
    MangoDatabase.create_text_index(
      @db_name,
      fields: [%{"name" => "location", "type" => "string"}],
      partial_filter_selector: selector,
      ddoc: "Selected",
      name: "Selected"
    )

    {:ok, resp} = MangoDatabase.find(@db_name, selector, explain: true, use_index: "Selected")
    assert resp["index"]["name"] == "Selected"
    {:ok, docs} = MangoDatabase.find(@db_name, selector, use_index: "Selected", fields: ["_id", "location"])
    assert length(docs) == 3
  end

  test "uses partial index with different selector" do
    selector = %{"location" => %{"$gte" => "FRA"}}
    selector2 = %{"location" => %{"$gte" => "A"}}
    MangoDatabase.create_text_index(
      @db_name,
      fields: [%{"name" => "location", "type" => "string"}],
      partial_filter_selector: selector,
      ddoc: "Selected",
      name: "Selected"
    )

    {:ok, resp} = MangoDatabase.find(@db_name, selector2, explain: true, use_index: "Selected")
    assert resp["index"]["name"] == "Selected"

    {:ok, docs} = MangoDatabase.find(@db_name, selector2, use_index: "Selected")
    assert length(docs) == 3
  end

  test "doesnot use selector when not specified" do
    selector = %{"location" => %{"$gte" => "FRA"}}
    MangoDatabase.create_text_index(
      @db_name,
      fields: [%{"name" => "location", "type" => "string"}],
      partial_filter_selector: selector,
      ddoc: "Selected",
      name: "Selected"
    )

    {:ok, resp} = MangoDatabase.find(@db_name, selector, explain: true)
    assert resp["index"]["name"] == "_all_docs"
  end

  test "doesnot use selector when not specified with index" do
    selector = %{"location" => %{"$gte" => "FRA"}}
    MangoDatabase.create_text_index(
      @db_name,
      fields: [%{"name" => "location", "type" => "string"}],
      partial_filter_selector: selector,
      ddoc: "Selected",
      name: "Selected"
    )
    MangoDatabase.create_text_index(
      @db_name,
      fields: [%{"name" => "location", "type" => "string"}],
      name: "NotSelected"
    )

    {:ok, resp} = MangoDatabase.find(@db_name, selector, explain: true)
    assert resp["index"]["name"] == "NotSelected"
  end

  test "old selector still supported" do
    oldschoolddoctext = %{
      "_id" => "_design/oldschooltext",
      "language" => "query",
      "indexes" => %{
        "oldschooltext" => %{
          "index" => %{
            "default_analyzer" => "keyword",
            "default_field" => %{},
            "selector" => %{"location" => %{"$gte" => "FRA"}},
            "fields" => [%{"name" => "location", "type" => "string"}],
            "index_array_lengths" => true
          },
          "analyzer" => %{
            "name" => "perfield",
            "default" => "keyword",
            "fields" => %{"$default" => "standard"}
          }
        }
      }
    }

    selector = %{"location" => %{"$gte" => "FRA"}}
    MangoDatabase.save_doc(@db_name, oldschoolddoctext)

    {:ok, resp} = MangoDatabase.find(@db_name, selector, explain: true, use_index: "oldschooltext")
    assert resp["index"]["name"] == "oldschooltext"
    {:ok, docs} = MangoDatabase.find(@db_name, selector, use_index: "oldschooltext")
    assert length(docs) == 3
  end

  test "old selector still supported via api" do
    selector = %{"location" => %{"$gte" => "FRA"}}
    MangoDatabase.create_text_index(
      @db_name,
      fields: [%{"name" => "location", "type" => "string"}],
      selector: selector,
      ddoc: "Selected",
      name: "Selected"
    )

    {:ok, docs} = MangoDatabase.find(@db_name, %{"location" => %{"$exists" => true}}, use_index: "Selected")
    assert length(docs) == 3
  end

  test "partial filter only in return if not default" do
    MangoDatabase.create_text_index(
      @db_name,
      fields: [%{"name" => "location", "type" => "string"}]
    )

    {:ok, indexes} = MangoDatabase.list_indexes(@db_name)
    assert Enum.at(indexes, 1)["def"]["partial_filter_selector"] == nil
  end

  test "uses partial index with non indexable selector" do
    partial_selector = %{"location" => %{"$gte" => "FRA"}}
    selector = %{"location" => %{"$exists" => true}}
    MangoDatabase.create_text_index(
      @db_name,
      fields: [%{"name" => "location", "type" => "string"}],
      partial_filter_selector: partial_selector,
      ddoc: "Selected",
      name: "Selected"
    )

    {:ok, resp} = MangoDatabase.find(@db_name, selector, explain: true, use_index: "Selected")
    assert resp["index"]["name"] == "Selected"
    {:ok, docs} = MangoDatabase.find(@db_name, selector, use_index: "Selected")
    assert length(docs) == 3
  end
end

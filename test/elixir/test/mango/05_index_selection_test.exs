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

defmodule IndexSelectionTest do
  use CouchTestCase
  defmacro describe(db) do
    quote do
      test "basic" do
        {:ok, resp} = MangoDatabase.find(unquote(db), %{"age" => 123}, explain: true)
        assert resp["index"]["type"] == "json"
      end

      test "with and" do
        {:ok, resp} = MangoDatabase.find(unquote(db),
          %{
            "name.first" => "Stephanie",
            "name.last" => "This doesn't have to match anything.",
          },
          explain: true
        )
        assert resp["index"]["type"] == "json"
      end

      test "with nested and" do
        {:ok, resp} = MangoDatabase.find(unquote(db),
          %{
            "name.first" => %{"$gt" => "a", "$lt" => "z"},
            "name.last" => "Foo"
          },
          explain: true
        )
        assert resp["index"]["type"] == "json"
      end

      test "with or" do
        ddocid = "_design/company_and_manager"
        {:ok, resp} = MangoDatabase.find(unquote(db),
          %{
            "company" => %{"$gt" => "a", "$lt" => "z"},
            "$or" => [%{"manager" => "Foo"}, %{"manager" => "Bar"}],
          },
          explain: true
        )
        assert resp["index"]["ddoc"] == ddocid
      end

      test "use most columns" do
        ddocid = "_design/age"
        {:ok, resp} = MangoDatabase.find(unquote(db),
          %{
            "name.first" =>"Stephanie",
            "name.last" => "Something or other",
            "age" => %{"$gt" => 1},
          },
          explain: true
        )
        assert resp["index"]["ddoc"] != ddocid

        {:ok, resp} = MangoDatabase.find(unquote(db),
          %{
            "name.first" => "Stephanie",
            "name.last" => "Something or other",
            "age" => %{"$gt" => 1},
          },
          use_index: ddocid,
          explain: true
        )
        assert resp["index"]["ddoc"] == ddocid
      end

      test "no valid sort index" do
        {:error, resp} = MangoDatabase.find(unquote(db),
          %{
            "_id" => %{"$gt" => nil}
          },
          sort: ["name"],
          return_raw: true
        )
        assert resp.status_code == 400
      end

      test "invalid use index" do
        # ddoc id for the age index
        ddocid = "_design/age"
        {:ok, r} = MangoDatabase.find(unquote(db), %{}, use_index: ddocid, return_raw: true)
        result =
          r["warning"]
          |> String.split("\n")
          |> Enum.at(0)
          |> String.downcase()
        expected = "#{ddocid} was not used because it does not contain a valid index for this query."
        assert result == expected
      end

      test "uses index when no range or equals" do
        # index on ["manager"] should be valid because
        # selector requires "manager" to exist. The
        # selector doesn't narrow the keyrange so it's
        # a full index scan
        selector = %{"manager" => %{"$exists" => true}}
        {:ok, docs} = MangoDatabase.find(unquote(db), selector)
        assert length(docs) == 14

        {:ok, resp_explain} = MangoDatabase.find(unquote(db), selector, explain: true)
        assert resp_explain["index"]["type"] == "json"
      end

      test "reject use index invalid fields" do
        ddocid = "_design/company_and_manager"
        selector = %{"company" => "Pharmex"}
        {:ok, r} = MangoDatabase.find(unquote(db), selector, use_index: ddocid, return_raw: true)
        result =
          r["warning"]
          |> String.split("\n")
          |> Enum.at(0)
          |> String.downcase()
        expected = "#{ddocid} was not used because it does not contain a valid index for this query."
        assert result == expected

        # should still return a correct result
        assert Enum.empty?(r["docs"]) == false
        Enum.each(r["docs"], fn d ->
          assert d["company"] == "Pharmex"
        end)
      end

      test "reject use index ddoc and name invalid fields" do
        ddocid = "_design/company_and_manager"
        name = "company_and_manager"
        selector = %{"company" => "Pharmex"}

        {:ok, resp} = MangoDatabase.find(unquote(db), selector, use_index: [ddocid, name], return_raw: true)
        result =
          resp["warning"]
          |> String.split("\n")
          |> Enum.at(0)
          |> String.downcase()
        expected = "#{ddocid}, #{name} was not used because it is not a valid index for this query."
        assert result == expected

        # should still return a correct result
        Enum.each(resp["docs"], fn d ->
          assert d["company"] == "Pharmex"
        end)
      end

      test "reject use index sort order" do
        # index on ["company","manager"] which should not be valid
        # and there is no valid fallback (i.e. an index on ["company"])
        ddocid = "_design/company_and_manager"
        selector = %{"company" => %{"$gt" => nil}}
        {:error, resp} = MangoDatabase.find(unquote(db), selector, use_index: ddocid, sort: [%{"company" => "desc"}])
        assert resp.status_code == 400
      end

      test "use index fallback if valid sort" do
        ddocid_valid = "_design/fallbackfoo"
        ddocid_invalid = "_design/fallbackfoobar"
        MangoDatabase.create_index(unquote(db), ["foo"], ddoc: ddocid_invalid)
        MangoDatabase.create_index(unquote(db), ["foo", "bar"], ddoc: ddocid_valid)
        selector = %{"foo" => %{"$gt" => nil}}

        {:ok, resp_explain} = MangoDatabase.find(unquote(db), selector, sort: ["foo", "bar"], use_index: ddocid_invalid, explain: true)
        assert resp_explain["index"]["ddoc"] == ddocid_valid

        {:ok, resp} = MangoDatabase.find(unquote(db), selector, sort: ["foo", "bar"], use_index: ddocid_invalid, return_raw: true)
        result =
          resp["warning"]
          |> String.split("\n")
          |> Enum.at(0)
          |> String.downcase()
        expected = "#{ddocid_invalid} was not used because it does not contain a valid index for this query."
        assert result == expected
        assert Enum.empty?(resp["docs"])
      end

      test "prefer use index over optimal index" do
        # index on ["company"] even though index on ["company", "manager"] is better
        ddocid_preferred = "_design/testsuboptimal"
        MangoDatabase.create_index(unquote(db), ["baz"], ddoc: ddocid_preferred)
        MangoDatabase.create_index(unquote(db), ["baz", "bar"])
        selector = %{"baz" => %{"$gt" => nil}, "bar" => %{"$gt" => nil}}
        {:ok, resp} = MangoDatabase.find(unquote(db), selector, use_index: ddocid_preferred, return_raw: true)
        assert not Map.has_key?(resp, "warning")

        {:ok, resp_explain} = MangoDatabase.find(unquote(db), selector, use_index: ddocid_preferred, explain: true)
        assert resp_explain["index"]["ddoc"] == ddocid_preferred
      end

      # This doc will not be saved given the new ddoc validation code
      # in couch_mrview
      test "manual bad view idx01" do
        design_doc = %{
          "_id" => "_design/bad_view_index",
          "language" => "query",
          "views" => %{
            "queryidx1" => %{
              "map" => %{"fields" => %{"age" => "asc"}},
              "reduce" => "_count",
              "options" => %{"def" => %{"fields" => [%{"age" => "asc"}]}, "w" => 2},
            }
          },
          "views" => %{
            "views001" => %{
              "map" => "function(employee){if(employee.training)"
              <> "{emit(employee.number, employee.training);}}"
            }
          },
        }
        # fails if the result does not match the pattern {:error, _}
        {:error, _} = MangoDatabase.save_docs(unquote(db), [design_doc])
      end

      test "explain sort reverse" do
        selector = %{"manager" => %{"$gt" => nil}}
        {:ok, resp_explain} = MangoDatabase.find(unquote(db), selector, fields: ["manager"], sort: [%{"manager" => "desc"}], explain: true)
        assert resp_explain["index"]["type"] == "json"
      end

      test "use index with invalid name" do
        Enum.each(["foo/bar/baz", ["foo", "bar", "baz"]], fn index ->
          {:error, resp} = MangoDatabase.find(unquote(db), %{"manager" => true}, use_index: index)
          assert resp.status_code == 400
        end)
      end

      test "use index without fallback succeeds for valid index" do
        {:ok, docs} = MangoDatabase.find(unquote(db), %{"manager" => true}, use_index: "manager", allow_fallback: false)
        assert length(docs) > 0
      end

      test "use index without fallback fails for invalid index with fallback available" do
        {:error, resp} = MangoDatabase.find(unquote(db), %{"manager" => true}, use_index: "invalid", allow_fallback: false)
        assert resp.status_code == 400
      end

      test "use index without fallback succeeds for empty index" do
        {:ok, docs} = MangoDatabase.find(unquote(db), %{"manager" => true}, use_index: [], allow_fallback: false)
        assert length(docs) > 0
      end

      test "use index without fallback fails for empty index" do
        {:error, resp} = MangoDatabase.find(unquote(db), %{"company" => "foobar"}, use_index: [], allow_fallback: false)
        assert resp.status_code == 400
      end

      test "use index without fallback fails for invalid index no fallback exists" do
        {:error, resp} = MangoDatabase.find(unquote(db), %{"company" => "foobar"}, use_index: "invalid", allow_fallback: false)
        assert resp.status_code == 400
      end

      test "index without fallback" do
        {:ok, docs} = MangoDatabase.find(unquote(db), %{"manager" => true}, allow_fallback: false)
        assert length(docs) > 0
      end

      test "no index without fallback" do
        {:error, resp} = MangoDatabase.find(unquote(db), %{"company" => "foobar"}, allow_fallback: false)
        assert resp.status_code == 400
      end
    end
  end
end

defmodule JSONIndexSelectionTest do
  use CouchTestCase
  require IndexSelectionTest

  @db_name "json-index-selection"

  setup do
    UserDocs.setup(@db_name)
  end

  IndexSelectionTest.describe(@db_name)

  test "uses all docs when fields do not match selector" do
    # index exists on ["company", "manager"] but not ["company"]
    # so we should fall back to all docs (so we include docs
    # with no "manager" field)
    selector = %{"company" => "Pharmex"}
    {:ok, docs} = MangoDatabase.find(@db_name, selector)
    assert length(docs) == 1
    assert Enum.at(docs, 0)["company"] == "Pharmex"
    assert not Map.has_key?(Enum.at(docs, 0), "manager")

    {:ok, resp_explain} = MangoDatabase.find(@db_name, selector, explain: true)
    assert resp_explain["index"]["type"] == "special"
  end

  test "uses all docs when selector doesnt require fields to exist" do
    # as in test above, use a selector that doesn't overlap with the index
    # due to an explicit exists clause
    selector = %{"company" => "Pharmex", "manager" => %{"$exists" => false}}
    {:ok, docs} = MangoDatabase.find(@db_name, selector)
    assert length(docs) == 1
    assert Enum.at(docs, 0)["company"] == "Pharmex"
    assert not Map.has_key?(Enum.at(docs, 0), "manager")

    {:ok, resp_explain} = MangoDatabase.find(@db_name, selector, explain: true)
    assert resp_explain["index"]["type"] == "special"
  end
end

defmodule TextIndexSelectionTest do
  use CouchTestCase

  @db_name "text-index-selection"

  setup do
    UserDocs.setup(@db_name, "text")
  end

  test "with text" do
    {:ok, resp} = MangoDatabase.find(@db_name,
      %{
        "$text" => "Stephanie",
        "name.first" => "Stephanie",
        "name.last" => "This doesn't have to match anything.",
      },
      explain: true
    )
    assert resp["index"]["type"] == "text"
  end

  test "no view index" do
    {:ok, resp} = MangoDatabase.find(@db_name, %{"name.first" => "Ohai!"}, explain: true)
    assert resp["index"]["type"] == "text"
  end

  test "with or" do
    {:ok, resp} = MangoDatabase.find(@db_name,
    %{
      "$or" => [
          %{"name.first" => "Stephanie"},
          %{"name.last" => "This doesn't have to match anything."},
        ]
      },
      explain: true
    )
    assert resp["index"]["type"] == "text"
  end

  test "manual bad text idx" do
    design_doc = %{
      "_id" => "_design/bad_text_index",
      "language" => "query",
      "indexes" => %{
        "text_index" => %{
          "default_analyzer" => "keyword",
          "default_field" => %{},
          "selector" => %{},
          "fields" => "all_fields",
          "analyzer" => %{
            "name" => "perfield",
            "default" => "keyword",
            "fields" => %{"$default" => "standard"},
          },
        }
      },
      "indexes" => %{
        "st_index" => %{
          "analyzer" => "standard",
          "index" => "function(doc){\n index(\"st_index\", doc.geometry);\n}",
        }
      },
    }
    MangoDatabase.save_docs(@db_name, [design_doc])
    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => 48})
    assert length(docs) == 1
    assert Enum.at(docs, 0)["name"]["first"] == "Stephanie"
    assert Enum.at(docs, 0)["age"] == 48
  end
end

defmodule MultiTextIndexSelectionTest do
  use CouchTestCase

  @db_name "multi-text-index-selection"

  setup do
    UserDocs.setup(@db_name)
    MangoDatabase.create_text_index(@db_name, ddoc: "foo", analyzer: "keyword")
    MangoDatabase.create_text_index(@db_name, ddoc: "bar", analyzer: "email")
    :ok
  end

  test "fallback to json with multi text" do
    {:ok, resp} = MangoDatabase.find(@db_name,
      %{"name.first" => "A first name", "name.last" => "A last name"},
      explain: true)
    assert resp["index"]["type"] == "json"
  end

  test "multi text index is error" do
    {:error, resp} = MangoDatabase.find(@db_name, %{"$text" => "a query"}, explain: true)
    assert resp.status_code == 400
  end

  test "use index works" do
    {:ok, resp} = MangoDatabase.find(@db_name, %{"$text" => "a query"}, use_index: "foo", explain: true)
    assert resp["index"]["ddoc"] == "_design/foo"
  end
end

defmodule RegexVsTextIndexTest do
  use CouchTestCase

  @db_name "regex-text-index"

  setup do
    MangoDatabase.recreate(@db_name)
    :ok
  end

  test "regex works with text index" do
    doc = %{"currency" => "HUF", "location" => "EUROPE"}
    saved_docs = MangoDatabase.save_docs(@db_name, [doc], w: 3)

    selector = %{"currency" => %{"$regex" => "HUF"}}
    {:ok, docs} = MangoDatabase.find(@db_name, selector)
    assert docs == saved_docs

    # Now that it is confirmed to be working, try again the
    # previous query with a text index on `location`.  This
    # attempt should succeed as well.
    MangoDatabase.create_text_index(@db_name, name: "TextIndexByLocation", fields: [%{"name" => "location", "type" => "string"}])
    {:ok, docs} = MangoDatabase.find(@db_name, selector)
    assert docs == saved_docs
  end
end

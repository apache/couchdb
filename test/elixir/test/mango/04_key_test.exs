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

defmodule KeyTest do
  use CouchTestCase

  @db_name "key-test"

  @test_docs [
    %{"_id" => "100", "type" => "complex_key", "title" => "normal key"},
    %{
        "_id" => "200",
        "type" => "complex_key",
        "title" => "key with dot",
        "dot.key" => "dot's value",
        "none" => %{"dot" => "none dot's value"},
        "name.first" => "Kvothe",
    },
    %{
        "_id" => "300",
        "type" => "complex_key",
        "title" => "key with peso",
        "$key" => "peso",
        "deep" => %{"$key" => "deep peso"},
        "name" => %{"first" => "Master Elodin"},
    },
    %{"_id" => "400", "type" => "complex_key", "title" => "unicode key", "" => "apple"},
    %{
        "_id" => "500",
        "title" => "internal_fields_format",
        "utf8-1[]:string" => "string",
        "utf8-2[]:boolean[]" => true,
        "utf8-3[]:number" => 9,
        "utf8-3[]:null" => nil,
    },
  ]

  setup do
    MangoDatabase.recreate(@db_name)
    MangoDatabase.save_docs(@db_name, @test_docs, w: 3)
    MangoDatabase.create_index(@db_name, ["type"], ddoc: "view")
    MangoDatabase.create_text_index(@db_name, ddoc: "text")
    :ok
  end

  defp run_check(query, check, opts) do
    fields = Keyword.get(opts, :fields, [])
    indexes = Keyword.get(opts, :indexes, ["view", "text"])
    Enum.each(indexes, fn idx ->
      {:ok, docs} = MangoDatabase.find(@db_name, query, fields: fields, use_index: idx)
      check.(docs)
    end)
  end

  test "dot key" do
    query = %{"type" => "complex_key"}
    fields = ["title", "dot\\.key", "none.dot"]

    check = fn docs ->
      assert length(docs) == 4
      doc = Enum.at(docs, 1)
      assert Map.has_key?(doc, "dot.key")
      assert doc["dot.key"] == "dot's value"
      assert Map.has_key?(doc, "none")
      assert doc["none"]["dot"] == "none dot's value"
    end

    run_check(query, check, fields: fields)
  end

  test "peso key" do
    query = %{"type" => "complex_key"}
    fields = ["title", "$key", "deep.$key"]

    check = fn docs ->
      assert length(docs) == 4
      doc = Enum.at(docs, 2)
      assert Map.has_key?(doc, "$key")
      assert doc["$key"] == "peso"
      assert Map.has_key?(doc, "deep")
      assert doc["deep"]["$key"] == "deep peso"
    end

    run_check(query, check, fields: fields)
  end

  test "unicode in fieldname" do
    query = %{"type" => "complex_key"}
    fields = ["title", ""]

    check = fn docs ->
      assert length(docs) == 4
      doc = Enum.at(docs, 3)
      # note:  == \uf8ff
      assert Map.has_key?(doc, "\uf8ff" )
      assert doc["\uf8ff"] == "apple"
    end

    run_check(query, check, fields: fields)
  end

  # The rest of these tests are only run against the text
  # indexes because view indexes don't have to worry about
  # field *name* escaping in the index.

  test "unicode in selector field" do
    query = %{"" => "apple"}

    check = fn docs ->
      assert length(docs) == 1
      doc = Enum.at(docs, 0)
      assert doc["\uf8ff"] == "apple"
    end

    run_check(query, check, indexes: ["text"])
  end

  test "internal field tests" do
    queries = [
      %{"utf8-1[]:string" => "string"},
      %{"utf8-2[]:boolean[]" => true},
      %{"utf8-3[]:number" => 9},
      %{"utf8-3[]:null" => nil},
    ]

    check = fn docs ->
      assert length(docs) == 1
      doc = Enum.at(docs, 0)
      assert doc["title"] == "internal_fields_format"
    end

    Enum.each(queries, fn query ->
      run_check(query, check, indexes: ["text"])
    end)
  end

  test "escape period" do
    query = %{"name\\.first" => "Kvothe"}
    check = fn docs ->
      assert length(docs) == 1
      doc = Enum.at(docs, 0)
      assert doc["name.first"] == "Kvothe"
    end
    run_check(query, check, indexes: ["text"])

    query = %{"name.first" => "Kvothe"}
    check_empty = fn docs ->
      assert Enum.empty?(docs)
    end
    run_check(query, check_empty, indexes: ["text"])
  end

  test "object period" do
    query = %{"name.first" => "Master Elodin"}
    check = fn docs ->
      assert length(docs) == 1
      doc = Enum.at(docs, 0)
      assert doc["title"] == "key with peso"
    end
    run_check(query, check, indexes: ["text"])

    query = %{"name\\.first" => "Master Elodin"}
    check_empty = fn docs ->
      assert Enum.empty?(docs)
    end
    run_check(query, check_empty, indexes: ["text"])
  end
end

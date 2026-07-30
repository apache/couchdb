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

defmodule NoDefaultFieldTest do
  use CouchTestCase

  @db_name "no-default-field"

  setup do
    UserDocs.setup(@db_name, "text")
    MangoDatabase.create_text_index(
      @db_name,
      default_field: false,
      ddoc: "text"
    )
    :ok
  end

  test "basic" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"$text" => "Ramona"}, use_index: "text")
    # Or should this throw an error?
    assert Enum.empty?(docs)
  end

  test "other fields exist" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => 22}, use_index: "text")
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 9
  end
end

defmodule NoDefaultFieldWithAnalyzer do
  use CouchTestCase

  @db_name "no-default-field-with-analyzer"

  setup do
    UserDocs.setup(@db_name, "text")
    MangoDatabase.create_text_index(
      @db_name,
      default_field: %{"enabled" => false, "analyzer" => "keyword"},
      ddoc: "text"
    )
    :ok
  end

  test "basic" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"$text" => "Ramona"}, use_index: "text")
    assert Enum.empty?(docs)
  end

  test "other fields exist" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => 22}, use_index: "text")
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 9
  end
end

defmodule DefaultFieldWithCustomAnalyzer do
  use CouchTestCase

  @db_name "default-field-with-custom-analyser"

  setup do
    UserDocs.setup(@db_name, "text")
    MangoDatabase.create_text_index(
      @db_name,
      default_field: %{"enabled" => true, "analyzer" => "keyword"},
      ddoc: "text"
    )
    :ok
  end

  test "basic" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"$text" => "Ramona"}, use_index: "text")
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 9
  end

  test "not analyzed" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"$text" => "Lott Place"}, use_index: "text")
    assert length(docs) == 1
    assert Enum.at(docs, 0)["user_id"] == 9

    {:ok, docs} = MangoDatabase.find(@db_name, %{"$text" => "Lott"}, use_index: "text")
    assert Enum.empty?(docs)

    {:ok, docs} = MangoDatabase.find(@db_name, %{"$text" => "Place"}, use_index: "text")
    assert Enum.empty?(docs)
  end
end

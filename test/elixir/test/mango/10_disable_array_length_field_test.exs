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

defmodule DisableIndexArrayLengthsTest do
  use CouchTestCase

  @db_name "disable-array-length-field-docs"

  setup do
    UserDocs.setup(@db_name, "text")

    MangoDatabase.create_text_index(@db_name,
      ddoc: "disable_index_array_lengths",
      analyzer: "keyword",
      index_array_lengths: false
    )

    MangoDatabase.create_text_index(@db_name,
      ddoc: "explicit_enable_index_array_lengths",
      analyzer: "keyword",
      index_array_lengths: true
    )
    :ok
  end

  test "disable index array length" do
    q = %{"favorites" => %{"$size" => 4}}
    {:ok, docs} = MangoDatabase.find(@db_name, q, use_index: "disable_index_array_lengths")
    assert docs == []
  end

  test "enable index array length" do
    q = %{"favorites" => %{"$size" => 4}}
    {:ok, docs} = MangoDatabase.find(@db_name, q, use_index: "explicit_enable_index_array_lengths")

    assert length(docs) == 4
    for doc <- docs do
      assert length(doc["favorites"]) == 4
    end
  end
end

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

defmodule SupportStableAndUpdate do
  use CouchTestCase

  @db_name "supports-stable-and-update"
  @docs1 [
    %{
      "_id" => "54af50626de419f5109c962f",
      "user_id" => 0,
      "age" => 10,
      "name" => "Jimi",
      "location" => "UK",
      "number" => 4,
    },
    %{
      "_id" => "54af50622071121b25402dc3",
      "user_id" => 1,
      "age" => 12,
      "name" => "Eddie",
      "location" => "ZAR",
      "number" => 2,
    },
  ]

  setup do
    MangoDatabase.recreate(@db_name)
    # Hack to prevent auto-indexer from foiling update=False test
    # https://github.com/apache/couchdb/issues/2313
    MangoDatabase.save_doc(@db_name, %{"_id" => "_design/foo", "language" => "query", "autoupdate" => false})
    MangoDatabase.create_index(@db_name, ["name"], ddoc: "foo")
    MangoDatabase.save_docs(@db_name, @docs1)
    :ok
  end

  test "update updates view when specified" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"name" => "Eddie"}, update: false)
    assert Enum.empty?(docs)
    {:ok, docs} = MangoDatabase.find(@db_name, %{"name" => "Eddie"}, update: true)
    assert length(docs) == 1
  end
end

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

defmodule IgnoreDesignDocsForAllDocsIndexTests do
  use CouchTestCase

  @db_name "ignore-design-docs"

  setup do
    MangoDatabase.recreate(@db_name)
    docs = [
      %{"_id" => "_design/my-design-doc"},
      %{"_id" => "54af50626de419f5109c962f", "user_id" => 0, "age" => 10, "name" => "Jimi"},
      %{"_id" => "54af50622071121b25402dc3", "user_id" => 1, "age" => 11, "name" => "Eddie"}
    ]
    MangoDatabase.save_docs(@db_name, docs)
    :ok
  end

  test "should not return design docs" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"_id" => %{"$gte" => nil}})
    assert length(docs) == 2
  end
end

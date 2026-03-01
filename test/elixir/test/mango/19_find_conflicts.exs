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

defmodule ChooseCorrectIndexForDocs do
  use CouchTestCase

  @db_name "find-conflicts"

  setup do
    doc = %{"_id" => "doc", "a" => 2}
    conflicts = [%{"_id" => "doc", "_rev" => "1-23202479633c2b380f79507a776743d5", "a" => 1}]

    MangoDatabase.recreate(@db_name)
    MangoDatabase.save_doc(@db_name, doc)
    MangoDatabase.save_docs_with_conflicts(@db_name, conflicts)
    :ok
  end

  test "retrieve conflicts" do
    MangoDatabase.create_index(@db_name, ["_conflicts"])
    selector = %{"_conflicts" => %{"$exists" => true}}
    {:ok, result} = MangoDatabase.find(@db_name, selector, conflicts: true)

    assert Enum.at(result, 0)["_conflicts"] == ["1-23202479633c2b380f79507a776743d5"]
    assert Enum.at(result, 0)["_rev"] == "1-3975759ccff3842adf690a5c10caee42"
  end
end

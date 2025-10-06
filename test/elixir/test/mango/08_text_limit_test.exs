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

defmodule LimitTests do
  use CouchTestCase

  @db_name "limit-docs"

  setup do
    LimitDocs.setup(@db_name, "text")
  end

  test "limit field" do
    q = %{"$or" => [%{"user_id" => %{"$lt" => 10}}, %{"filtered_array.[]" => 1}]}
    docs = MangoDatabase.find(@db_name, q, limit: 10)

    assert length(docs) == 8
    Enum.each(docs, fn d -> assert d["user_id"] < 10 end)
  end
end

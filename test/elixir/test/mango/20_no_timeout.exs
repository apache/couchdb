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

defmodule LongRunningMangoTest do
  use CouchTestCase

  @db_name "no-timeout"

  setup do
    MangoDatabase.recreate(@db_name)

    0..100_000
    |> Enum.reduce([], fn i, docs ->
      docs = [%{"_id" => "#{i}", "another" => "field"} | docs]

      if rem(i, 20_000) == 0 do
        MangoDatabase.save_docs(@db_name, docs)
        []
      else
        docs
      end
    end)
  end

  test "query does not time out" do
    selector = %{"_id" => %{"$gt" => 0}, "another" => "wrong"}
    {:ok, docs} = MangoDatabase.find(@db_name, selector)
    assert docs == []
  end
end

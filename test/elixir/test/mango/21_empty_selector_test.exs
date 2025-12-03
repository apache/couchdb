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

defmodule EmptySelectorTests do
  use CouchTestCase

  defmacro describe(db, index_type) do
    quote do
      test "empty" do
        {:ok, resp} = MangoDatabase.find(unquote(db), %{}, explain: true)
        assert resp["index"]["type"] == "special"
      end

      test "empty array or" do
        {:ok, resp} = MangoDatabase.find(unquote(db), %{"$or" => []}, explain: true)
        assert resp["index"]["type"] == unquote(index_type)

        {:ok, docs} = MangoDatabase.find(unquote(db), %{"$or" => []})
        assert Enum.empty?(docs)
      end

      test "empty array or with age" do
        selector =  %{"age" => 22, "$or" => []}
        {:ok, resp} = MangoDatabase.find(unquote(db), selector, explain: true)
        assert resp["index"]["type"] == unquote(index_type)

        {:ok, docs} = MangoDatabase.find(unquote(db), selector)
        assert length(docs) == 1
      end

      test "empty array in with age" do
        selector =  %{"age" => 22, "company" => %{"$in" => []}}
        {:ok, resp} = MangoDatabase.find(unquote(db), selector, explain: true)
        assert resp["index"]["type"] == unquote(index_type)

        {:ok, docs} = MangoDatabase.find(unquote(db), selector)
        assert Enum.empty?(docs)
      end

      test "empty array and with age" do
        selector =  %{"age" => 22, "$and" => []}
        {:ok, resp} = MangoDatabase.find(unquote(db), selector, explain: true)
        assert resp["index"]["type"] == unquote(index_type)

        {:ok, docs} = MangoDatabase.find(unquote(db), selector)
        assert length(docs) == 1
      end

      test "empty array all age" do
        selector =  %{"age" => 22, "company" => %{"$all" => []}}
        {:ok, resp} = MangoDatabase.find(unquote(db), selector, explain: true)
        assert resp["index"]["type"] == unquote(index_type)

        {:ok, docs} = MangoDatabase.find(unquote(db), selector)
        assert Enum.empty?(docs)
      end

      test "empty array nested all with age" do
        selector =  %{"age" => 22, "$and" => [%{"company" => %{"$all" => []}}]}
        {:ok, resp} = MangoDatabase.find(unquote(db), selector, explain: true)
        assert resp["index"]["type"] == unquote(index_type)

        {:ok, docs} = MangoDatabase.find(unquote(db), selector)
        assert Enum.empty?(docs)
      end

      test "empty arrays complex" do
        selector =  %{"$or" => [], "a" => %{"$in" => []}}
        {:ok, resp} = MangoDatabase.find(unquote(db), selector, explain: true)
        assert resp["index"]["type"] == unquote(index_type)

        {:ok, docs} = MangoDatabase.find(unquote(db), selector)
        assert Enum.empty?(docs)
      end

      test "empty nin" do
        selector =  %{"favorites" => %{"$nin" => []}}
        {:ok, resp} = MangoDatabase.find(unquote(db), selector, explain: true)
        assert resp["index"]["type"] == unquote(index_type)

        {:ok, docs} = MangoDatabase.find(unquote(db), selector)
        assert length(docs) == UserDocs.len()
      end
    end
  end
end

defmodule EmptySelectorNoIndexTests do
  use ExUnit.Case
  require EmptySelectorTests

  setup do
    db_name = "empty-selector-noindex"
    UserDocs.setup(db_name, "special")
  end
  EmptySelectorTests.describe("empty-selector-noindex", "special")
end

defmodule EmptySelectorTextTests do
  use ExUnit.Case
  require EmptySelectorTests

  setup do
    db_name = "empty-selector-text"
    UserDocs.setup(db_name, "text")
  end
  EmptySelectorTests.describe("empty-selector-text", "text")
end

defmodule EmptySelectorUserDocTests do
  use ExUnit.Case
  require EmptySelectorTests

  setup do
    db_name = "empty-selector"
    UserDocs.setup(db_name)
  end
  EmptySelectorTests.describe("empty-selector", "json")
end

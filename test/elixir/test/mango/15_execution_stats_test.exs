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

defmodule ExecutionStatsTests do
  use CouchTestCase
  use ExUnit.Case

  @db_name "execution-stats-docs"

  setup do
    UserDocs.setup(@db_name)
  end

  test "simple json index" do
    selector = %{"age" => %{"$lt" => 35}}
    {:ok, raw} = MangoDatabase.find(@db_name, selector, return_raw: true, executionStats: true)

    assert length(raw["docs"]) == 3
    assert raw["execution_stats"]["total_keys_examined"] == 3
    assert raw["execution_stats"]["total_docs_examined"] == 3
    assert raw["execution_stats"]["total_quorum_docs_examined"] == 0
    assert raw["execution_stats"]["results_returned"] == 3
    assert raw["execution_stats"]["execution_time_ms"] > 0
  end

  test "no execution stats" do
    selector = %{"age" => %{"$lt" => 35}}
    {:ok, raw} = MangoDatabase.find(@db_name, selector, return_raw: true, executionStats: false)

    refute Map.has_key?(raw, "execution_stats")
  end

  test "quorum json index" do
    selector = %{"age" => %{"$lt" => 35}}
    {:ok, raw} = MangoDatabase.find(
      @db_name,
      selector,
      return_raw: true,
      r: 3,
      executionStats: true
    )

    assert length(raw["docs"]) == 3
    assert raw["execution_stats"]["total_keys_examined"] == 3
    assert raw["execution_stats"]["total_docs_examined"] == 0
    assert raw["execution_stats"]["total_quorum_docs_examined"] == 3
    assert raw["execution_stats"]["results_returned"] == 3
    assert raw["execution_stats"]["execution_time_ms"] > 0
  end

  test "results returned limit" do
    selector = %{"age" => %{"$lt" => 35}}
    {:ok, raw} = MangoDatabase.find(
      @db_name,
      selector,
      return_raw: true,
      limit: 2,
      executionStats: true
    )

    assert raw["execution_stats"]["results_returned"] == length(raw["docs"])
  end

  test "no matches index scan" do
    selector = %{"age" => %{"$lt" => 35}, "nomatch" => "me"}
    {:ok, raw} = MangoDatabase.find(
      @db_name,
      selector,
      return_raw: true,
      executionStats: true
    )

    assert raw["execution_stats"]["total_docs_examined"] == 3
    assert raw["execution_stats"]["results_returned"] == 0
  end

  test "covering json index" do
    selector = %{"age" => %{"$lt" => 35}}
    {:ok, raw} = MangoDatabase.find(
      @db_name,
      selector,
      fields: ["_id", "age"],
      return_raw: true,
      executionStats: true
    )

    assert length(raw["docs"]) == 3
    assert raw["execution_stats"]["total_keys_examined"] == 3
    assert raw["execution_stats"]["total_docs_examined"] == 0
    assert raw["execution_stats"]["total_quorum_docs_examined"] == 0
    assert raw["execution_stats"]["results_returned"] == 3
  end

  # reporting consistency
  @cases [
  %{
    title: "with limit",
    selector: %{"age" => %{"$lte" => 42}},
    fields: ["name", "email", "age"],
    limit: 3,
    total_keys_examined: 4,
    total_docs_examined: 4,
    results_returned: 3

  },
  %{
    title: "partial matches",
    selector: %{"favorites" => %{"$elemMatch" => %{"$eq" => "Erlang"}}},
    fields: ["name", "email", "twitter"],
    limit: 200,
    total_keys_examined: 15,
    total_docs_examined: 15,
    results_returned: 6,
  },
  %{
    title: "no matches, using _all_docs",
    selector: %{"foo" => "bar"},
    fields: [],
    limit: 200,
    total_keys_examined: 25,
    total_docs_examined: 25,
    results_returned: 0,
  },
  %{
    title: "no matches, indexed column (no keys examined)",
    selector: %{"name.first" => "Lee", "name.last" => "Jackson"},
    fields: ["email", "twitter"],
    limit: 200,
    total_keys_examined: 0,
    total_docs_examined: 0,
    results_returned: 0,
  },
  %{
    title: "no matches, indexed column",
    selector: %{"favorites" => %{"$elemMatch" => %{"$eq" => "Haskell"}}},
    fields: ["name", "email", "twitter"],
    limit: 200,
    total_keys_examined: 15,
    total_docs_examined: 15,
    results_returned: 0,
  }]

  for case <- @cases do
    test "scenario #{case.title}" do
      case = unquote(Macro.escape(case))
      {:ok, resp} = MangoDatabase.find(
        @db_name,
        case.selector,
        fields: case.fields,
        limit: case.limit,
        return_raw: true,
        executionStats: true
      )

      execution_stats = resp["execution_stats"]
      assert execution_stats["total_keys_examined"] == case.total_keys_examined
      assert execution_stats["total_docs_examined"] == case.total_docs_examined
      assert execution_stats["results_returned"] == case.results_returned
    end
  end
end

defmodule ExecutionStatsTestsText do
  use CouchTestCase
  use ExUnit.Case

  @db_name "execution-stats-text-docs"

  setup do
    UserDocs.setup(@db_name, "text")
  end

  test "simple text index" do
    selector = %{"$text" => "Stephanie"}
    {:ok, raw} = MangoDatabase.find(@db_name, selector, return_raw: true, executionStats: true)

    assert length(raw["docs"]) == 1
    assert raw["execution_stats"]["total_keys_examined"] == 1
    assert raw["execution_stats"]["total_docs_examined"] == 1
    assert raw["execution_stats"]["total_quorum_docs_examined"] == 0
    assert raw["execution_stats"]["results_returned"] == 1
    assert raw["execution_stats"]["execution_time_ms"] > 0
  end

  test "no execution stats" do
    selector = %{"$text" => "Stephanie"}
    {:ok, raw} = MangoDatabase.find(@db_name, selector, return_raw: true)
    refute Map.has_key?(raw, "execution_stats")
  end
end
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

defmodule BeginsWithOperator do
  use CouchTestCase

  @db_name "begins-with-operator"

  @docs [
    %{"_id" => "aaa", "name" => "Jimi", "location" => "AUS", "age" => 27},
    %{"_id" => "abc", "name" => "Eddie", "location" => "AND", "age" => 65},
    %{"_id" => "bbb", "name" => "Harry", "location" => "CAN", "age" => 21},
    %{"_id" => "ccc", "name" => "Eddie", "location" => "DEN", "age" => 37},
    %{"_id" => "ddd", "name" => "Jones", "location" => "ETH", "age" => 49},
]

  setup do
    MangoDatabase.recreate(@db_name)
    MangoDatabase.save_docs(@db_name, @docs)
    MangoDatabase.create_index(@db_name, ["location"])
    MangoDatabase.create_index(@db_name, ["name", "location"])
    :ok
  end

  # split into Unicode graphemes
  defp split_unicode(binary) do
    binary
    |> String.graphemes()
  end

  defp get_mrargs(selector, opts \\ []) do
    sort = Keyword.get(opts, :sort, [])
    {:ok, explain} = MangoDatabase.find(@db_name, selector, sort: sort, explain: true)
    explain["mrargs"]
  end

  defp assert_doc_ids(user_ids, docs) do
    user_ids_returned =
      docs
      |> Enum.map(fn d -> d["_id"] end)
      |> Enum.sort()
    assert Enum.sort(user_ids) == user_ids_returned
  end

  test "basic" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"location" => %{"$beginsWith" => "A"}})
    assert length(docs) == 2
    assert_doc_ids(["aaa", "abc"], docs)
  end

  test "json range" do
    mrargs = get_mrargs(%{"location" => %{"$beginsWith" => "A"}})
    assert mrargs["start_key"] == ["A"]
    assert mrargs["end_key"] == [<<"A\xef\xbf\xbf">>, <<"<MAX>">>]
  end

  test "compound key" do
    selector = %{"name" => "Eddie", "location" => %{"$beginsWith" => "A"}}
    mrargs = get_mrargs(selector)
    assert mrargs["start_key"] == ["Eddie", "A"]
    assert mrargs["end_key"] == [<<"Eddie">>, <<"A\xef\xbf\xbf">>, <<"<MAX>">>]

    {:ok, docs} = MangoDatabase.find(@db_name, selector)
    assert length(docs) == 1
    assert_doc_ids(["abc"], docs)
  end

  test "sort" do
    selector = %{"location" => %{"$beginsWith" => "A"}}
    cases = [
        %{
            "sort" => ["location"],
            "start_key" => [<<"A">>],
            "end_key" => [<<"A\xef\xbf\xbf">>, <<"<MAX>">>],
            "direction" => "fwd",
        },
        %{
            "sort" => [%{"location" => "desc"}],
            "start_key" => [<<"A\xef\xbf\xbf">>, <<"<MAX>">>],
            "end_key" => [<<"A">>],
            "direction" => "rev",
        },
    ]
    for case <- cases do
      mrargs = get_mrargs(selector, sort: case["sort"])

      assert mrargs["start_key"] == case["start_key"]
      assert mrargs["end_key"] == case["end_key"]
      assert mrargs["direction"] == case["direction"]
    end
  end

  test "all docs range" do
    mrargs = get_mrargs(%{"_id" => %{"$beginsWith" => "a"}})

    assert mrargs["start_key"] == "a"
    end_key_bytes = split_unicode(mrargs["end_key"])
    assert end_key_bytes == [<<"a">>, <<"\xef\xbf\xbf">>]
  end

  test "no index" do
    selector = %{"foo" => %{"$beginsWith" => "a"}}
    {:ok, resp_explain} = MangoDatabase.find(@db_name, selector, explain: true)
    mrargs = resp_explain["mrargs"]

    assert resp_explain["index"]["type"] == "special"
    assert mrargs["start_key"] == nil
    assert mrargs["end_key"] == "<MAX>"
  end

  test "invalid operand" do
    {:error, resp} = MangoDatabase.find(@db_name, %{"_id" => %{"$beginsWith" => true}})
    assert resp.status_code == 400
    assert resp.body["error"] == "invalid_operator"
  end

  test "does not match non string value" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"age" => %{"$beginsWith" => "a"}})
    assert Enum.empty?(docs)
  end

  test "no matches" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"name" => %{"$beginsWith" => "Z"}})
    assert Enum.empty?(docs)
  end

  test "case sensitivity" do
    {:ok, docs} = MangoDatabase.find(@db_name, %{"name" => %{"$beginsWith" => "j"}})
    assert Enum.empty?(docs)

    {:ok, docs} = MangoDatabase.find(@db_name, %{"name" => %{"$beginsWith" => "J"}})
    assert length(docs) == 2
  end
end

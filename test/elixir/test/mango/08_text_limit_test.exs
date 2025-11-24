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
    {:ok, docs} = MangoDatabase.find(@db_name, q, limit: 10, sort: ["user_id"])

    assert length(docs) == 8
    Enum.each(docs, fn d -> assert d["user_id"] < 10 end)
  end

  test "limit field 2" do
    q = %{"$or" => [%{"user_id" => %{"$lt" => 20}}, %{"filtered_array.[]" => 1}]}
    {:ok, docs} = MangoDatabase.find(@db_name, q, limit: 10, sort: ["user_id"])
    assert length(docs) == 10
    Enum.each(docs, fn d -> assert d["user_id"] < 20 end)
  end

  test "limit field 3" do
    q = %{"$or" => [%{"user_id" => %{"$lt" => 100}}, %{"filtered_array.[]" => 1}]}
    {:ok, docs} = MangoDatabase.find(@db_name, q, limit: 1, sort: ["user_id"])
    assert length(docs) == 1
    Enum.each(docs, fn d -> assert d["user_id"] < 100 end)
  end

  test "limit field 4" do
    q = %{"$or" => [%{"user_id" => %{"$lt" => 0}}, %{"filtered_array.[]" => 1}]}
    {:ok, docs} = MangoDatabase.find(@db_name, q, limit: 35)
    assert Enum.empty?(docs)
  end

  # We reach our cap here of 50
  test "limit field 5" do
    q = %{"age" => %{"$exists" => true}}
    {:ok, docs} = MangoDatabase.find(@db_name, q, limit: 250)
    assert length(docs) == LimitDocs.get_docs_length()
    Enum.each(docs, fn d -> assert d["age"] < 100 end)
  end

  test "limit skip field 1" do
    q = %{"$or" => [%{"user_id" => %{"$lt" => 100}}, %{"filtered_array.[]" => 1}]}
    {:ok, docs} = MangoDatabase.find(@db_name, q, limit: 10, skip: 20, sort: ["user_id"])
    assert length(docs) == 10
    Enum.each(docs, fn d -> assert d["user_id"] > 20 end)
  end

  test "limit skip field 2" do
    q = %{"$or" => [%{"user_id" => %{"$lt" => 100}}, %{"filtered_array.[]" => 1}]}
    {:ok, docs} = MangoDatabase.find(@db_name, q, limit: 100, skip: 100)
    assert Enum.empty?(docs)
  end

  test "limit skip field 3" do
    q = %{"$or" => [%{"user_id" => %{"$lt" => 20}}, %{"filtered_array.[]" => 1}]}
    {:ok, docs} = MangoDatabase.find(@db_name, q, limit: 1, skip: 30)
    assert Enum.empty?(docs)
  end

  test "limit skip field 4" do
    q = %{"$or" => [%{"user_id" => %{"$lt" => 100}}, %{"filtered_array.[]" => 1}]}
    {:ok, docs} = MangoDatabase.find(@db_name, q, limit: 0, skip: 0)
    assert Enum.empty?(docs)
  end

  test "limit skip field 5" do
    q = %{"$or" => [%{"user_id" => %{"$lt" => 100}}, %{"filtered_array.[]" => 1}]}
    {:error, resp} = MangoDatabase.find(@db_name, q, limit: -1)
    assert resp.status_code == 400
  end

  test "limit skip field 6" do
    q = %{"$or" => [%{"user_id" => %{"$lt" => 100}}, %{"filtered_array.[]" => 1}]}
    {:error, resp} = MangoDatabase.find(@db_name, q, skip: -1)
    assert resp.status_code == 400
  end

  # Basic test to ensure we can iterate through documents with a bookmark
  test "limit bookmark" do
    Enum.each(
      Enum.to_list(1..LimitDocs.get_docs_length()//5), #[1]
      fn size ->
        seen_docs = run_bookmark_check(size, "")

        assert MapSet.size(seen_docs) == LimitDocs.get_docs_length()
      end
    )
  end

  defp run_bookmark_check(size, bookmark) do
    q = %{"age" => %{"$gt" => 0}}
    seen_docs = MapSet.new([])

    seen_docs = get_docs(q, size, bookmark, seen_docs)

    assert MapSet.size(seen_docs) == LimitDocs.get_docs_length()

    seen_docs
  end

  defp get_docs(q, size, bookmark, seen_docs) do
    {:ok, json} = MangoDatabase.find(@db_name, q, limit: size, bookmark: bookmark, return_raw: true)

    seen_docs = Enum.reduce(json["docs"], seen_docs,
      fn doc, seen_docs ->
        assert not Enum.member?(seen_docs, doc["_id"])
        MapSet.put(seen_docs, doc["_id"])
      end
    )

    if length(json["docs"]) != 0 do
      assert bookmark != json["bookmark"]
      get_docs(q, size, json["bookmark"], seen_docs)
    else
      seen_docs
    end
  end

  test "limit bookmark with sort" do
    Enum.each(
      Enum.to_list(1..LimitDocs.get_docs_length()//5), #[1]
      fn size ->
        seen_docs = run_bookmark_check_with_sort(size, "", 0)

        assert MapSet.size(seen_docs) == LimitDocs.get_docs_length()
      end
    )
  end

  defp run_bookmark_check_with_sort(size, bookmark, age) do
    q = %{"age" => %{"$gt" => 0}}
    seen_docs = MapSet.new([])

    seen_docs = get_docs_with_sort(q, size, bookmark, age, seen_docs)

    assert MapSet.size(seen_docs) == LimitDocs.get_docs_length()

    seen_docs
  end

  defp get_docs_with_sort(q, size, bookmark, age, seen_docs) do
    {:ok, json} = MangoDatabase.find(
      @db_name,
      q,
      limit: size,
      bookmark: bookmark,
      sort: ["age"],
      return_raw: true
    )

    {seen_docs, age} = Enum.reduce(json["docs"], {seen_docs, age},
      fn doc, {seen_docs, age} ->
        assert not Enum.member?(seen_docs, doc["_id"])
        assert doc["age"] >= age

        {MapSet.put(seen_docs, doc["_id"]), doc["age"]}
      end
    )

    if length(json["docs"]) != 0 do
      assert bookmark != json["bookmark"]
      get_docs_with_sort(q, size, json["bookmark"], age, seen_docs)
    else
      seen_docs
    end
  end
end

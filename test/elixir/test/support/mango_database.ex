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

defmodule MangoDatabase do
  @moduledoc false
  def has_text_service() do
    resp = Couch.get("/")
    "search" in resp.body["features"]
  end

  def recreate(db, opts \\ []) do
    resp = Couch.get("/#{db}")
    if resp.status_code == 200 do
      docs = resp.body["doc_count"] + resp.body["doc_del_count"]
      if docs > 0 do
        delete(db)
        create(db, opts)
      end
    else
      create(db, opts)
    end
  end

  def create(db, opts \\ []) do
    partitioned = Keyword.get(opts, :partitioned, false)
    Couch.put("/#{db}?partitioned=#{partitioned}")
  end

  defp delete(db) do
    Couch.delete("/#{db}")
  end

  def save_doc(db, doc, opts \\ []) do
    MangoDatabase.save_docs(db, [doc], opts)
  end

  def save_docs_with_conflicts(db, docs) do
    body = %{"docs" => docs, "new_edits" => false}
    Couch.post("/#{db}/_bulk_docs", body: body)
  end

  # If a certain keyword like sort or field is passed in the options,
  # then it is added to the request body.
  defp put_if_set(map, key, opts, opts_key) do
    if Keyword.has_key?(opts, opts_key) do
      Map.put(map, key, opts[opts_key])
    else
      map
    end
  end

  # TODO: make this use batches if necessary
  def save_docs(db, docs, opts \\ []) do
    query = %{}
    |> put_if_set("w", opts, :w)

    result = Couch.post("/#{db}/_bulk_docs", body: %{"docs" => docs}, query: query)
    zipped_docs = Enum.zip(docs, result.body)

    # This returns the doc list including _id and _rev values
    resp = Enum.map(zipped_docs, fn {doc, result} ->
      doc
      |> Map.put("_id", result["id"])
      |> Map.put("_rev", result["rev"])
    end)

    # _bulk_docs sometimes returns errors in the body and this is captured here
    errors = Enum.filter(result.body, fn r ->
      Map.has_key?(r, "error")
    end)
    if errors == [] do
      resp
    else
      {:error, errors}
    end
  end

  def create_index(db, fields, options \\ []) do
    index = %{
      "fields" => fields,
    }
    |> put_if_set("selector", options, :selector)
    |> put_if_set("partial_filter_selector", options, :partial_filter_selector)

    body = %{
      "index" => index,
      "type" => "json",
      "w" => 3
    }
    |> put_if_set("type", options, :idx_type)
    |> put_if_set("name", options, :name)
    |> put_if_set("ddoc", options, :ddoc)

    resp = Couch.post("/#{db}/_index", body: body)
    if resp.status_code == 200 do
      {:ok, resp.body["result"] == "created"}
    else
      {:error, resp}
    end
  end

  def create_text_index(db, options \\ []) do
    index = %{}
    |> put_if_set("default_analyzer", options, :analyzer)
    |> put_if_set("default_field", options, :default_field)
    |> put_if_set("index_array_lengths", options, :index_array_lengths)
    |> put_if_set("selector", options, :selector)
    |> put_if_set("partial_filter_selector", options, :partial_filter_selector)
    |> put_if_set("fields", options, :fields)

    body = %{
      "index" => index,
      "type" => Keyword.get(options, :idx_type, "text"),
      "w" => 3
    }
    |> put_if_set("name", options, :name)
    |> put_if_set("ddoc", options, :ddoc)

    resp = Couch.post("/#{db}/_index", body: body)

    if resp.status_code == 200 do
      {:ok, resp.body["result"] == "created"}
    else
      {:error, resp}
    end
  end

  def list_indexes(db, opts \\ []) do
    limit = Keyword.get(opts, :limit)
    skip  = Keyword.get(opts, :skip)
    query =
      [limit: limit, skip: skip]
      |> Enum.filter(fn {_k, v} -> not is_nil(v) end)
      |> Enum.map_join("&", fn {k, v} -> "#{k}=#{v}" end)

    path =
      if query == "" do
        "/#{db}/_index"
      else
        "/#{db}/_index?#{query}"
      end
    resp = Couch.get(path)

    if resp.status_code == 200 do
      {:ok, resp.body["indexes"]}
    else
      {:error, resp}
    end
  end

  def find(db, selector, opts \\ []) do
    defaults = [
      use_index: nil,
      skip: 0,
      limit: 25,
      r: 1,
      conflicts: false,
      explain: false,
      return_raw: false
    ]
    options = Keyword.merge(defaults, opts)

    path =
      case options[:explain] do
        true -> "/#{db}/_explain"
        _ -> "/#{db}/_find"
      end

    resp = Couch.post(path, body: %{
      "selector" => selector,
      "use_index" => options[:use_index],
      "skip" => options[:skip],
      "limit" => options[:limit],
      "r" => options[:r],
      "conflicts" => options[:conflicts]
    }
    |> put_if_set("sort", options, :sort)
    |> put_if_set("fields", options, :fields)
    |> put_if_set("execution_stats", options, :executionStats)
    |> put_if_set("allow_fallback", options, :allow_fallback)
    |> put_if_set("bookmark", options, :bookmark)
    )

    case {(options[:explain] or options[:return_raw]), resp.status_code} do
      {false, 200} -> {:ok, resp.body["docs"]}
      {true, 200} -> {:ok, resp.body}
      _ -> {:error, resp}
    end
  end
end

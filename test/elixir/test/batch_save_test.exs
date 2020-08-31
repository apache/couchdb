defmodule BatchSaveTest do
  use CouchTestCase

  @moduletag :batch_save
  @moduletag kind: :performance

  @moduledoc """
  Test CouchDB batch save
  This is a port of batch_save.js
  """

  @doc_count 100

  @tag :with_db
  test "batch put", context do
    path_fun = &"/#{&1}/#{&2}"
    run(&Couch.put/2, path_fun, context[:db_name], @doc_count)
  end

  @tag :with_db
  test "batch post", context do
    path_fun = fn db_name, _ -> "/#{db_name}" end
    run(&Couch.post/2, path_fun, context[:db_name], @doc_count)
  end

  @tag :with_db
  test "batch put with identical doc ids", context do
    path_fun = fn db_name, _ -> "/#{db_name}/foo" end
    run(&Couch.put/2, path_fun, context[:db_name], 1)
  end

  defp run(req_fun, path_fun, db_name, expected_doc_count) do
    for i <- 1..@doc_count do
      opts = [body: %{a: i, b: i}, query: %{batch: "ok"}]
      resp = req_fun.(path_fun.(db_name, i), opts)
      assert resp.body["ok"] and resp.status_code == 202
    end

    retry_until(fn ->
      Couch.get("/#{db_name}").body["doc_count"] == expected_doc_count
    end)
  end
end

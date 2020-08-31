defmodule InvalidDocIDsTest do
  use CouchTestCase

  @moduletag :invalid_doc_ids
  @moduletag kind: :single_node

  @moduledoc """
  Test invalid document ids
  This is a port of the invalid_docids.js suite
  """

  @tag :with_db
  test "_local-prefixed ids are illegal", context do
    db_name = context[:db_name]

    [
      "/#{db_name}/_local",
      "/#{db_name}/_local/",
      "/#{db_name}/_local%2F",
      "/#{db_name}/_local/foo/bar"
    ]
    |> Enum.each(fn url ->
      %{status_code: status, body: body} = Couch.put(url, body: %{})
      assert status === 400
      assert body["error"] === "bad_request"
    end)
  end

  @tag :with_db
  test "using a non-string id is forbidden", context do
    db_name = context[:db_name]
    %{status_code: status, body: body} = Couch.post("/#{db_name}", body: %{:_id => 1})
    assert status === 400
    assert body["error"] === "illegal_docid"
    assert body["reason"] === "Document id must be a string"
  end

  @tag :with_db
  test "a PUT request with absent _id is forbidden", context do
    db_name = context[:db_name]
    %{status_code: status, body: body} = Couch.put("/#{db_name}/_other", body: %{})
    assert status === 400
    assert body["error"] === "illegal_docid"
  end

  @tag :with_db
  test "accidental POST to form handling code", context do
    db_name = context[:db_name]
    %{status_code: status, body: body} = Couch.put("/#{db_name}/_tmp_view", body: %{})
    assert status === 400
    assert body["error"] === "illegal_docid"
  end

  @tag :with_db
  test "invalid _prefix", context do
    db_name = context[:db_name]

    %{status_code: status, body: body} =
      Couch.post("/#{db_name}", body: %{:_id => "_invalid"})

    assert status === 400
    assert body["error"] === "illegal_docid"
    assert body["reason"] === "Only reserved document ids may start with underscore."
  end

  @tag :with_db
  test "explicit _bulk_docks policy", context do
    db_name = context[:db_name]
    docs = [%{:_id => "_design/foo"}, %{:_id => "_local/bar"}]

    %{status_code: status} = Couch.post("/#{db_name}/_bulk_docs", body: %{docs: docs})

    assert status in [201, 202]

    Enum.each(docs, fn %{:_id => id} ->
      %{:body => %{"_id" => document_id}} = Couch.get("/#{db_name}/#{id}")
      assert document_id === id
    end)

    %{status_code: invalid_status, body: invalid_body} =
      Couch.post("/#{db_name}/_bulk_docs", body: %{docs: [%{:_id => "_invalid"}]})

    assert invalid_status === 400
    assert invalid_body["error"] === "illegal_docid"
  end
end

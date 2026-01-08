defmodule ValidateDocUpdateTest do
  use CouchTestCase

  @moduledoc """
  Test validate_doc_update behaviour
  """

  @js_type_check %{
    language: "javascript",

    validate_doc_update: ~s"""
      function (newDoc) {
        if (!newDoc.type) {
          throw {forbidden: 'Documents must have a type field'};
        }
      }
    """
  }

  @tag :with_db
  test "JavaScript VDU accepts a valid document", context do
    db = context[:db_name]
    Couch.put("/#{db}/_design/js-test", body: @js_type_check)

    resp = Couch.put("/#{db}/doc", body: %{"type" => "movie"})
    assert resp.status_code == 201
    assert resp.body["ok"] == true
  end

  @tag :with_db
  test "JavaScript VDU rejects an invalid document", context do
    db = context[:db_name]
    Couch.put("/#{db}/_design/js-test", body: @js_type_check)

    resp = Couch.put("/#{db}/doc", body: %{"not" => "valid"})
    assert resp.status_code == 403
    assert resp.body["error"] == "forbidden"
  end

  @js_change_check %{
    language: "javascript",

    validate_doc_update: ~s"""
      function (newDoc, oldDoc) {
        if (oldDoc && newDoc.type !== oldDoc.type) {
          throw {forbidden: 'Documents cannot change their type field'};
        }
      }
    """
  }

  @tag :with_db
  test "JavaScript VDU accepts a valid change", context do
    db = context[:db_name]
    Couch.put("/#{db}/_design/js-test", body: @js_change_check)

    Couch.put("/#{db}/doc", body: %{"type" => "movie"})

    doc = Couch.get("/#{db}/doc").body
    updated = doc |> Map.merge(%{"type" => "movie", "title" => "Duck Soup"})
    resp = Couch.put("/#{db}/doc", body: updated)

    assert resp.status_code == 201
  end

  @tag :with_db
  test "JavaScript VDU rejects an invalid change", context do
    db = context[:db_name]
    Couch.put("/#{db}/_design/js-test", body: @js_change_check)

    Couch.put("/#{db}/doc", body: %{"type" => "movie"})

    doc = Couch.get("/#{db}/doc").body
    updated = doc |> Map.put("type", "director")
    resp = Couch.put("/#{db}/doc", body: updated)

    assert resp.status_code == 403
  end
end

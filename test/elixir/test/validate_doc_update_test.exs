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

  @mango_type_check %{
    language: "query",

    validate_doc_update: %{
      "newDoc" => %{"type" => %{"$exists" => true}}
    }
  }

  @tag :with_db
  test "Mango VDU accepts a valid document", context do
    db = context[:db_name]
    resp = Couch.put("/#{db}/_design/mango-test", body: @mango_type_check)
    assert resp.status_code == 201

    resp = Couch.put("/#{db}/doc", body: %{"type" => "movie"})
    assert resp.status_code == 201
    assert resp.body["ok"] == true
  end

  @tag :with_db
  test "Mango VDU rejects an invalid document", context do
    db = context[:db_name]
    resp = Couch.put("/#{db}/_design/mango-test", body: @mango_type_check)
    assert resp.status_code == 201

    resp = Couch.put("/#{db}/doc", body: %{"no" => "type"})
    assert resp.status_code == 403
    assert resp.body["error"] == "forbidden"
  end

  @tag :with_db
  test "updating a Mango VDU updates its effects", context do
    db = context[:db_name]

    resp = Couch.put("/#{db}/_design/mango-test", body: @mango_type_check)
    assert resp.status_code == 201

    ddoc = %{
      language: "query",

      validate_doc_update: %{
        "newDoc" => %{
          "type" => %{"$type" => "string"},
          "year" => %{"$lt" => 2026}
        }
      }
    }
    resp = Couch.put("/#{db}/_design/mango-test", body: ddoc, query: %{rev: resp.body["rev"]})
    assert resp.status_code == 201

    resp = Couch.put("/#{db}/doc1", body: %{"type" => "movie", "year" => 1994})
    assert resp.status_code == 201

    resp = Couch.put("/#{db}/doc2", body: %{"type" => 42, "year" => 1994})
    assert resp.status_code == 403
    assert resp.body["error"] == "forbidden"

    resp = Couch.put("/#{db}/doc3", body: %{"type" => "movie", "year" => 2094})
    assert resp.status_code == 403
    assert resp.body["error"] == "forbidden"
  end

  @tag :with_db
  test "converting a Mango VDU to JavaScript updates its effects", context do
    db = context[:db_name]

    resp = Couch.put("/#{db}/_design/mango-test", body: @mango_type_check)
    assert resp.status_code == 201

    ddoc = %{
      language: "javascript",

      validate_doc_update: ~s"""
        function (newDoc) {
          if (typeof newDoc.year !== 'number') {
            throw {forbidden: 'Documents must have a valid year field'};
          }
        }
      """
    }
    resp = Couch.put("/#{db}/_design/mango-test", body: ddoc, query: %{rev: resp.body["rev"]})
    assert resp.status_code == 201

    resp = Couch.put("/#{db}/doc1", body: %{"year" => 1994})
    assert resp.status_code == 201

    resp = Couch.put("/#{db}/doc2", body: %{"year" => "1994"})
    assert resp.status_code == 403
    assert resp.body["error"] == "forbidden"
  end

  @tag :with_db
  test "deleting a Mango VDU removes its effects", context do
    db = context[:db_name]

    resp = Couch.put("/#{db}/_design/mango-test", body: @mango_type_check)
    assert resp.status_code == 201

    resp = Couch.delete("/#{db}/_design/mango-test", query: %{rev: resp.body["rev"]})
    assert resp.status_code == 200

    resp = Couch.put("/#{db}/doc", body: %{"no" => "type"})
    assert resp.status_code == 201
  end

  @tag :with_db
  test "Mango VDU rejects a doc if any existing ddoc fails to match", context do
    db = context[:db_name]
    resp = Couch.put("/#{db}/_design/mango-test", body: @mango_type_check)
    assert resp.status_code == 201

    ddoc = %{
      language: "query",

      validate_doc_update: %{
        "newDoc" => %{"year" => %{"$lt" => 2026}}
      }
    }
    resp = Couch.put("/#{db}/_design/mango-test-2", body: ddoc)
    assert resp.status_code == 201

    resp = Couch.put("/#{db}/doc1", body: %{"type" => "movie", "year" => 1994})
    assert resp.status_code == 201

    resp = Couch.put("/#{db}/doc2", body: %{"year" => 1994})
    assert resp.status_code == 403
    assert resp.body["error"] == "forbidden"

    resp = Couch.put("/#{db}/doc3", body: %{"type" => "movie", "year" => 2094})
    assert resp.status_code == 403
    assert resp.body["error"] == "forbidden"
  end
end

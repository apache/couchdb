defmodule CopyDocTest do
  use CouchTestCase

  @moduletag :copy_doc
  @moduletag kind: :single_node

  @moduledoc """
  Test CouchDB Copy Doc
  This is a port of the copy_doc.js suite
  """
  @tag :with_db
  test "Copy doc tests", context do
    db_name = context[:db_name]
    create_doc(db_name, %{_id: "doc_to_be_copied", v: 1})

    resp =
      Couch.request(
        :copy,
        "/#{db_name}/doc_to_be_copied",
        headers: [Destination: "doc_that_was_copied"]
      )

    assert resp.body["ok"]
    assert resp.status_code in [201, 202]

    assert Couch.get("/#{db_name}/doc_that_was_copied").body["v"] == 1

    create_doc(db_name, %{_id: "doc_to_be_copied2", v: 1})
    {_, resp} = create_doc(db_name, %{_id: "doc_to_be_overwritten", v: 2})
    rev = resp.body["rev"]

    resp =
      Couch.request(
        :copy,
        "/#{db_name}/doc_to_be_copied2",
        headers: [Destination: "doc_to_be_overwritten"]
      )

    assert resp.status_code == 409

    resp =
      Couch.request(
        :copy,
        "/#{db_name}/doc_to_be_copied2"
      )

    assert resp.status_code == 400
    assert resp.body["reason"] == "Destination header is mandatory for COPY."

    resp =
      Couch.request(
        :copy,
        "/#{db_name}/doc_to_be_copied2",
        headers: [Destination: "http://localhost:5984/#{db_name}/doc_to_be_written"]
      )

    assert resp.status_code == 400
    assert resp.body["reason"] == "Destination URL must be relative."

    resp =
      Couch.request(
        :copy,
        "/#{db_name}/doc_to_be_copied2",
        headers: [Destination: "doc_to_be_overwritten?rev=#{rev}"]
      )

    assert resp.status_code in [201, 202]
    resp = Couch.get("/#{db_name}/doc_to_be_overwritten")
    assert resp.body["_rev"] != rev
    assert resp.body["v"] == 1
  end
end

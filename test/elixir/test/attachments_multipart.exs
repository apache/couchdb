defmodule AttachmentMultipartTest do
  use CouchTestCase

  @moduletag :attachments

  @moduledoc """
  Test CouchDB attachment multipart requests
  This is a port of the attachments_multipart.js suite
  """

  @tag :with_db
  test "manages attachments in views successfully", context do
    db_name = context[:db_name]

    document = """
    {
      "body": "This is a body.",
      "_attachments": {
        "foo.txt": {
          "follows": true,
          "content_type": "application/test",
          "length": 21
        },
        "bar.txt": {
          "follows": true,
          "content_type": "application/test",
          "length": 20
        },
        "baz.txt": {
          "follows": true,
          "content_type": "text/plain",
          "length": 19
        }
      }
    }
    """

    multipart_data =
      "--abc123\r\n" <>
        "content-type: application/json\r\n" <>
        "\r\n" <>
        document <>
        "\r\n--abc123\r\n" <>
        "\r\n" <>
        "this is 21 chars long" <>
        "\r\n--abc123\r\n" <>
        "\r\n" <>
        "this is 20 chars lon" <>
        "\r\n--abc123\r\n" <>
        "\r\n" <>
        "this is 19 chars lo" <>
        "\r\n--abc123--epilogue"

    resp =
      Couch.put("/#{db_name}/multipart",
        body: multipart_data,
        headers: ["Content-Type": "multipart/related;boundary=\"abc123\""]
      )

    assert resp.status_code == 201
    assert resp.body["ok"] == true

    resp = Couch.get("/#{db_name}/multipart/foo.txt")

    assert resp.body == "this is 21 chars long"

    resp = Couch.get("/#{db_name}/multipart/bar.txt")

    assert resp.body == "this is 20 chars lon"

    resp = Couch.get("/#{db_name}/multipart/baz.txt")

    assert resp.body == "this is 19 chars lo"

    doc = Couch.get("/#{db_name}/multipart", query: %{att_encoding_info: true})
    first_rev = doc.body["_rev"]

    assert doc.body["_attachments"]["foo.txt"]["stub"] == true
    assert doc.body["_attachments"]["bar.txt"]["stub"] == true
    assert doc.body["_attachments"]["baz.txt"]["stub"] == true

    assert Map.has_key?(doc.body["_attachments"]["foo.txt"], "encoding") == false
    assert Map.has_key?(doc.body["_attachments"]["bar.txt"], "encoding") == false
    assert doc.body["_attachments"]["baz.txt"]["encoding"] == "gzip"

    document_updated = """
    {
      "_rev": "#{first_rev}",
      "body": "This is a body.",
      "_attachments": {
        "foo.txt": {
          "stub": true,
          "content_type": "application/test"
        },
        "bar.txt": {
          "follows": true,
          "content_type": "application/test",
          "length": 18
        }
      }
    }
    """

    multipart_data_updated =
      "--abc123\r\n" <>
        "content-type: application/json\r\n" <>
        "\r\n" <>
        document_updated <>
        "\r\n--abc123\r\n" <>
        "\r\n" <>
        "this is 18 chars l" <>
        "\r\n--abc123--"

    resp =
      Couch.put("/#{db_name}/multipart",
        body: multipart_data_updated,
        headers: ["Content-Type": "multipart/related;boundary=\"abc123\""]
      )

    assert resp.status_code == 201

    resp = Couch.get("/#{db_name}/multipart/bar.txt")

    assert resp.body == "this is 18 chars l"

    resp = Couch.get("/#{db_name}/multipart/baz.txt")

    assert resp.status_code == 404
  end
end

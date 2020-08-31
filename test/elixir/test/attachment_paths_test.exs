defmodule AttachmentPathsTest do
  use CouchTestCase

  @moduletag :attachments
  @moduletag kind: :single_node

  @bin_att_doc """
  {
     "_id": "bin_doc",
     "_attachments": {
       "foo/bar.txt": {
         "content_type": "text/plain",
         "data": "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
       },
       "foo%2Fbaz.txt": {
         "content_type": "text/plain",
         "data": "V2UgbGlrZSBwZXJjZW50IHR3byBGLg=="
       }
     }
   }
  """

  @design_att_doc """
  {
     "_id": "_design/bin_doc",
     "_attachments": {
       "foo/bar.txt": {
         "content_type": "text/plain",
         "data": "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
       },
       "foo%2Fbaz.txt": {
         "content_type": "text/plain",
         "data": "V2UgbGlrZSBwZXJjZW50IHR3byBGLg=="
       }
     }
   }
  """

  @moduledoc """
  Test CouchDB attachment names
  This is a port of the attachment_names.js suite
  """

  @tag :with_db_name
  test "manages attachment paths successfully", context do
    db_name =
      URI.encode(
        "#{context[:db_name]}/with_slashes",
        &URI.char_unreserved?(&1)
      )

    create_db(db_name)

    resp = Couch.post("/#{db_name}", body: @bin_att_doc)
    msg = "Should return 201-Created"

    assert resp.status_code in [201, 202], msg

    rev = resp.body["rev"]

    resp = Couch.get("/#{db_name}/bin_doc/foo/bar.txt")
    assert resp.status_code == 200
    assert resp.body == "This is a base64 encoded text"
    assert resp.headers["Content-Type"] == "text/plain"

    resp = Couch.get("/#{db_name}/bin_doc/foo%2Fbar.txt")
    assert resp.status_code == 200
    assert resp.body == "This is a base64 encoded text"
    assert resp.headers["Content-Type"] == "text/plain"

    resp = Couch.get("/#{db_name}/bin_doc/foo/baz.txt")
    assert resp.status_code == 404

    resp = Couch.get("/#{db_name}/bin_doc/foo%252Fbaz.txt")
    assert resp.status_code == 200
    assert resp.body == "We like percent two F."

    resp =
      Couch.put(
        "/#{db_name}/bin_doc/foo/attachment.txt",
        body: "Just some text",
        headers: ["Content-Type": "text/plain;charset=utf-8"]
      )

    assert resp.status_code == 409

    resp =
      Couch.put(
        "/#{db_name}/bin_doc/foo/bar2.txt",
        query: %{rev: rev},
        body: "This is no base64 encoded text",
        headers: ["Content-Type": "text/plain;charset=utf-8"]
      )

    assert resp.status_code in [201, 202]

    resp = Couch.get("/#{db_name}/bin_doc")
    assert resp.status_code == 200

    att_doc = resp.body

    assert att_doc["_attachments"]["foo/bar.txt"]
    assert att_doc["_attachments"]["foo%2Fbaz.txt"]
    assert att_doc["_attachments"]["foo/bar2.txt"]

    ctype = att_doc["_attachments"]["foo/bar2.txt"]["content_type"]
    assert ctype == "text/plain;charset=utf-8"

    assert att_doc["_attachments"]["foo/bar2.txt"]["length"] == 30
    delete_db(db_name)
  end

  @tag :with_db_name
  test "manages attachment paths successfully - design docs", context do
    db_name =
      URI.encode(
        "#{context[:db_name]}/with_slashes",
        &URI.char_unreserved?(&1)
      )

    create_db(db_name)
    resp = Couch.post("/#{db_name}", body: @design_att_doc)
    assert resp.status_code in [201, 202]

    rev = resp.body["rev"]

    resp = Couch.get("/#{db_name}/_design/bin_doc/foo/bar.txt")
    assert resp.status_code == 200
    assert resp.body == "This is a base64 encoded text"
    assert resp.headers["Content-Type"] == "text/plain"

    resp = Couch.get("/#{db_name}/_design/bin_doc/foo%2Fbar.txt")
    assert resp.status_code == 200
    assert resp.body == "This is a base64 encoded text"
    assert resp.headers["Content-Type"] == "text/plain"

    resp = Couch.get("/#{db_name}/_design/bin_doc/foo/baz.txt")
    assert resp.status_code == 404

    resp = Couch.get("/#{db_name}/_design/bin_doc/foo%252Fbaz.txt")
    assert resp.status_code == 200
    assert resp.body == "We like percent two F."

    resp =
      Couch.put(
        "/#{db_name}/_design/bin_doc/foo/attachment.txt",
        body: "Just some text",
        headers: ["Content-Type": "text/plain;charset=utf-8"]
      )

    assert resp.status_code == 409

    resp =
      Couch.put(
        "/#{db_name}/_design/bin_doc/foo/bar2.txt",
        query: %{rev: rev},
        body: "This is no base64 encoded text",
        headers: ["Content-Type": "text/plain;charset=utf-8"]
      )

    assert resp.status_code in [201, 202]

    resp = Couch.get("/#{db_name}/_design/bin_doc")
    assert resp.status_code == 200

    att_doc = resp.body

    assert att_doc["_attachments"]["foo/bar.txt"]
    assert att_doc["_attachments"]["foo%2Fbaz.txt"]
    assert att_doc["_attachments"]["foo/bar2.txt"]

    ctype = att_doc["_attachments"]["foo/bar2.txt"]["content_type"]
    assert ctype == "text/plain;charset=utf-8"

    assert att_doc["_attachments"]["foo/bar2.txt"]["length"] == 30
    delete_db(db_name)
  end
end

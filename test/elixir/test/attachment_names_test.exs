defmodule AttachmentNamesTest do
  use CouchTestCase

  @moduletag :attachments
  @moduletag kind: :single_node

  @good_doc """
   {
    "_id": "good_doc",
    "_attachments": {
      "Kолян.txt": {
        "content_type": "application/octet-stream",
        "data": "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
      }
    }
  }
  """

  @bin_att_doc %{
    _id: "bin_doc",
    _attachments: %{
      footxt: %{
        content_type: "text/plain",
        data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
      }
    }
  }

  @bin_data "JHAPDO*AU£PN ){(3u[d 93DQ9¡€])}    ææøo'∂ƒæ≤çæππ•¥∫¶®#†π¶®¥π€ª®˙π8np"

  @leading_underscores_att """
   {
    "_id": "bin_doc2",
    "_attachments": {
      "_foo.txt": {
        "content_type": "text/plain",
        "data": "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
      }
    }
  }
  """

  @moduledoc """
  Test CouchDB attachment names
  This is a port of the attachment_names.js suite
  """

  @tag :with_db
  test "saves attachment names successfully", context do
    db_name = context[:db_name]
    filename = URI.encode("Kолян.txt", &URI.char_unreserved?(&1))
    resp = Couch.post("/#{db_name}", body: @good_doc)
    msg = "Should return 201-Created"
    assert resp.status_code in [201, 202], msg

    resp = Couch.get("/#{db_name}/good_doc/#{filename}")
    assert resp.body == "This is a base64 encoded text"
    assert resp.headers["Content-Type"] == "application/octet-stream"
    assert resp.headers["Etag"] == ~s("aEI7pOYCRBLTRQvvqYrrJQ==")

    resp = Couch.post("/#{db_name}", body: @bin_att_doc)
    assert(resp.status_code == 201)

    # standalone docs
    resp =
      Couch.put(
        "/#{db_name}/bin_doc3/attachmenttxt",
        body: @bin_data,
        headers: ["Content-Type": "text/plain;charset=utf-8"]
      )

    assert(resp.status_code == 201)

    # bulk docs
    docs = %{
      docs: [@bin_att_doc]
    }

    resp =
      Couch.post(
        "/#{db_name}/_bulk_docs",
        body: docs
      )

    assert(resp.status_code == 201)

    resp =
      Couch.put(
        "/#{db_name}/bin_doc2",
        body: @leading_underscores_att
      )

    assert resp.status_code == 400

    assert resp.body["reason"] ==
             "Attachment name '_foo.txt' starts with prohibited character '_'"
  end
end

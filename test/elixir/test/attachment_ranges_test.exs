defmodule AttachmentRangesTest do
  use CouchTestCase

  @moduletag :attachments
  @moduletag kind: :single_node

  @moduledoc """
  Test CouchDB attachment range requests
  This is a port of the attachment_ranges.js suite
  """

  @tag :with_db
  test "manages attachment range requests successfully", context do
    db_name = context[:db_name]

    bin_att_doc = %{
      _id: "bin_doc",
      _attachments: %{
        "foo.txt": %{
          content_type: "application/octet-stream",
          data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
        }
      }
    }

    create_doc(db_name, bin_att_doc)
    # Fetching the whole entity is a 206
    resp =
      Couch.get(
        "/#{db_name}/bin_doc/foo.txt",
        headers: [Range: "bytes=0-28"]
      )

    assert(resp.status_code == 206)
    assert resp.body == "This is a base64 encoded text"
    assert resp.headers["Content-Range"] == "bytes 0-28/29"
    assert resp.headers["Content-Length"] == "29"

    # Fetch the whole entity without an end offset is a 200
    resp =
      Couch.get(
        "/#{db_name}/bin_doc/foo.txt",
        headers: [Range: "bytes=0-"]
      )

    assert(resp.status_code == 200)
    assert resp.body == "This is a base64 encoded text"
    assert resp.headers["Content-Range"] == nil
    assert resp.headers["Content-Length"] == "29"

    # Even if you ask multiple times.
    resp =
      Couch.get(
        "/#{db_name}/bin_doc/foo.txt",
        headers: [Range: "bytes=0-,0-,0-"]
      )

    assert(resp.status_code == 200)

    # Badly formed range header is a 200
    resp =
      Couch.get(
        "/#{db_name}/bin_doc/foo.txt",
        headers: [Range: "bytes:0-"]
      )

    assert(resp.status_code == 200)

    # Fetch the end of an entity without an end offset is a 206
    resp =
      Couch.get(
        "/#{db_name}/bin_doc/foo.txt",
        headers: [Range: "bytes=2-"]
      )

    assert(resp.status_code == 206)
    assert resp.body == "is is a base64 encoded text"
    assert resp.headers["Content-Range"] == "bytes 2-28/29"
    assert resp.headers["Content-Length"] == "27"

    # Fetch first part of entity is a 206
    resp =
      Couch.get(
        "/#{db_name}/bin_doc/foo.txt",
        headers: [Range: "bytes=0-3"]
      )

    assert(resp.status_code == 206)
    assert resp.body == "This"
    assert resp.headers["Content-Range"] == "bytes 0-3/29"
    assert resp.headers["Content-Length"] == "4"

    # Fetch middle of entity is also a 206
    resp =
      Couch.get(
        "/#{db_name}/bin_doc/foo.txt",
        headers: [Range: "bytes=10-15"]
      )

    assert(resp.status_code == 206)
    assert resp.body == "base64"
    assert resp.headers["Content-Range"] == "bytes 10-15/29"
    assert resp.headers["Content-Length"] == "6"

    # Fetch end of entity is also a 206
    resp =
      Couch.get(
        "/#{db_name}/bin_doc/foo.txt",
        headers: [Range: "bytes=-3"]
      )

    assert(resp.status_code == 206)
    assert resp.body == "ext"
    assert resp.headers["Content-Range"] == "bytes 26-28/29"
    assert resp.headers["Content-Length"] == "3"

    # backward range is 416
    resp =
      Couch.get(
        "/#{db_name}/bin_doc/foo.txt",
        headers: [Range: "bytes=5-3"]
      )

    assert(resp.status_code == 416)

    # range completely outside of entity is 416
    resp =
      Couch.get(
        "/#{db_name}/bin_doc/foo.txt",
        headers: [Range: "bytes=300-310"]
      )

    assert(resp.status_code == 416)

    # We ignore a Range header with too many ranges
    resp =
      Couch.get(
        "/#{db_name}/bin_doc/foo.txt",
        headers: [Range: "bytes=0-1,0-1,0-1,0-1,0-1,0-1,0-1,0-1,0-1,0-1"]
      )

    assert(resp.status_code == 200)
  end
end

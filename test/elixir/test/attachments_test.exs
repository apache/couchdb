defmodule AttachmentsTest do
  use CouchTestCase

  @moduletag :attachments
  @moduletag kind: :single_node

  #  MD5 Digests of compressible attachments and therefore Etags
  #  will vary depending on platform gzip implementation.
  #  These MIME types are defined in [attachments] compressible_types
  @bin_att_doc %{
    _id: "bin_doc",
    _attachments: %{
      "foo.txt": %{
        content_type: "application/octet-stream",
        data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
      }
    }
  }

  @moduledoc """
  Test CouchDB attachments
  This is a port of the attachments.js suite
  """

  @tag :with_db
  test "saves attachment successfully", context do
    db_name = context[:db_name]

    resp = Couch.put("/#{db_name}/bin_doc", body: @bin_att_doc, query: %{w: 3})
    assert resp.status_code in [201, 202]
    assert resp.body["ok"]
  end

  @tag :with_db
  test "errors for bad attachment", context do
    db_name = context[:db_name]

    bad_att_doc = %{
      _id: "bad_doc",
      _attachments: %{
        "foo.txt": %{
          content_type: "text/plain",
          data: "notBase64Encoded="
        }
      }
    }

    resp = Couch.put("/#{db_name}/bad_doc", body: bad_att_doc, query: %{w: 3})
    assert resp.status_code == 400
  end

  @tag :with_db
  test "reads attachment successfully", context do
    db_name = context[:db_name]

    resp = Couch.put("/#{db_name}/bin_doc", body: @bin_att_doc, query: %{w: 3})
    assert resp.status_code in [201, 202]

    resp = Couch.get("/#{db_name}/bin_doc/foo.txt", body: @bin_att_doc)

    assert resp.body == "This is a base64 encoded text"
    assert resp.headers["Content-Type"] == "application/octet-stream"
    assert resp.headers["Etag"] == "\"aEI7pOYCRBLTRQvvqYrrJQ==\""
  end

  @tag :with_db
  test "update attachment", context do
    db_name = context[:db_name]

    bin_att_doc2 = %{
      _id: "bin_doc2",
      _attachments: %{
        "foo.txt": %{
          content_type: "text/plain",
          data: ""
        }
      }
    }

    resp = Couch.put("/#{db_name}/bin_doc2", body: bin_att_doc2, query: %{w: 3})
    assert resp.status_code in [201, 202]
    rev = resp.body["rev"]

    resp = Couch.get("/#{db_name}/bin_doc2/foo.txt")

    assert resp.headers["Content-Type"] == "text/plain"
    assert resp.body == ""

    resp =
      Couch.put(
        "/#{db_name}/bin_doc2/foo2.txt",
        query: %{rev: rev, w: 3},
        body: "This is no base64 encoded text",
        headers: ["Content-Type": "text/plain;charset=utf-8"]
      )

    assert resp.status_code in [201, 202]
    assert Regex.match?(~r/bin_doc2\/foo2.txt/, resp.headers["location"])
  end

  @tag :with_db
  test "delete attachment", context do
    db_name = context[:db_name]

    resp = Couch.put("/#{db_name}/bin_doc", body: @bin_att_doc, query: %{w: 3})
    assert resp.status_code in [201, 202]
    rev = resp.body["rev"]

    resp = Couch.delete("/#{db_name}/bin_doc/foo.txt", query: %{w: 3})

    assert resp.status_code == 409

    resp = Couch.delete("/#{db_name}/bin_doc/foo.txt", query: %{w: 3, rev: rev})
    assert resp.status_code == 200
    assert resp.headers["location"] == nil
  end

  @tag :with_db
  test "saves binary", context do
    db_name = context[:db_name]

    bin_data = "JHAPDO*AU£PN ){(3u[d 93DQ9¡€])}    ææøo'∂ƒæ≤çæππ•¥∫¶®#†π¶®¥π€ª®˙π8np"

    resp =
      Couch.put(
        "/#{db_name}/bin_doc3/attachment.txt",
        body: bin_data,
        headers: ["Content-Type": "text/plain;charset=utf-8"],
        query: %{w: 3}
      )

    assert resp.status_code in [201, 202]
    assert resp.body["ok"]

    rev = resp.body["rev"]

    resp = Couch.get("/#{db_name}/bin_doc3/attachment.txt")
    assert resp.body == bin_data

    resp =
      Couch.put("/#{db_name}/bin_doc3/attachment.txt", body: bin_data, query: %{w: 3})

    assert resp.status_code == 409

    # non-existent rev
    resp =
      Couch.put(
        "/#{db_name}/bin_doc3/attachment.txt",
        query: %{rev: "1-adae8575ecea588919bd08eb020c708e", w: 3},
        headers: ["Content-Type": "text/plain;charset=utf-8"],
        body: bin_data
      )

    assert resp.status_code == 409

    # current rev
    resp =
      Couch.put(
        "/#{db_name}/bin_doc3/attachment.txt",
        query: %{rev: rev, w: 3},
        headers: ["Content-Type": "text/plain;charset=utf-8"],
        body: bin_data
      )

    assert resp.status_code in [201, 202]

    rev = resp.body["rev"]

    resp = Couch.get("/#{db_name}/bin_doc3/attachment.txt")
    assert String.downcase(resp.headers["Content-Type"]) == "text/plain;charset=utf-8"
    assert resp.body == bin_data

    resp = Couch.get("/#{db_name}/bin_doc3/attachment.txt", query: %{rev: rev})
    assert String.downcase(resp.headers["Content-Type"]) == "text/plain;charset=utf-8"
    assert resp.body == bin_data

    resp = Couch.delete("/#{db_name}/bin_doc3/attachment.txt", query: %{rev: rev, w: 3})
    assert resp.status_code == 200

    resp = Couch.get("/#{db_name}/bin_doc3/attachment.txt")
    assert resp.status_code == 404

    resp = Couch.get("/#{db_name}/bin_doc3/attachment.txt", query: %{rev: rev})
    assert String.downcase(resp.headers["Content-Type"]) == "text/plain;charset=utf-8"
    assert resp.body == bin_data
  end

  @tag :with_db
  test "empty attachments", context do
    db_name = context[:db_name]

    resp =
      Couch.put(
        "/#{db_name}/bin_doc4/attachment.txt",
        body: "",
        headers: ["Content-Type": "text/plain;charset=utf-8"],
        query: %{w: 3}
      )

    assert resp.status_code in [201, 202]
    assert resp.body["ok"]

    rev = resp.body["rev"]

    resp = Couch.get("/#{db_name}/bin_doc4/attachment.txt")
    assert resp.status_code == 200
    assert resp.body == ""

    resp =
      Couch.put(
        "/#{db_name}/bin_doc4/attachment.txt",
        query: %{rev: rev, w: 3},
        headers: ["Content-Type": "text/plain;charset=utf-8"],
        body: "This is a string"
      )

    assert resp.status_code in [201, 202]

    resp = Couch.get("/#{db_name}/bin_doc4/attachment.txt")
    assert resp.status_code == 200
    assert resp.body == "This is a string"
  end

  @tag :with_db
  test "large attachments COUCHDB-366", context do
    db_name = context[:db_name]

    lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. "
    range = 1..10

    large_att = Enum.reduce(range, lorem, fn _, acc -> lorem <> acc end)

    resp =
      Couch.put(
        "/#{db_name}/bin_doc5/attachment.txt",
        body: large_att,
        query: %{w: 3},
        headers: ["Content-Type": "text/plain;charset=utf-8"]
      )

    assert resp.status_code in [201, 202]
    assert resp.body["ok"]

    resp = Couch.get("/#{db_name}/bin_doc5/attachment.txt")
    assert String.downcase(resp.headers["Content-Type"]) == "text/plain;charset=utf-8"
    assert resp.body == large_att

    lorem_b64 =
      "TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2NpbmcgZWxpdC4g"

    range = 1..10

    large_b64_att = Enum.reduce(range, lorem_b64, fn _, acc -> lorem_b64 <> acc end)

    resp =
      Couch.get(
        "/#{db_name}/bin_doc5",
        query: %{attachments: true},
        headers: [Accept: "application/json"]
      )

    assert large_b64_att == resp.body["_attachments"]["attachment.txt"]["data"]
  end

  @tag :with_db
  test "etags for attachments", context do
    db_name = context[:db_name]

    lorem_att = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. "

    resp =
      Couch.put(
        "/#{db_name}/bin_doc6/attachment.txt",
        body: lorem_att,
        headers: ["Content-Type": "text/plain;charset=utf-8"],
        query: %{w: 3}
      )

    assert resp.status_code in [201, 202]
    assert resp.body["ok"]

    resp = Couch.get("/#{db_name}/bin_doc6/attachment.txt")
    assert resp.status_code == 200
    etag = resp.headers["etag"]

    resp =
      Couch.get("/#{db_name}/bin_doc6/attachment.txt", headers: ["if-none-match": etag])

    assert resp.status_code == 304
  end

  @tag :with_db
  test "test COUCHDB-497 - empty attachments", context do
    db_name = context[:db_name]

    lorem_att = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. "

    resp =
      Couch.put(
        "/#{db_name}/bin_doc7/attachment.txt",
        body: lorem_att,
        headers: ["Content-Type": "text/plain;charset=utf-8"],
        query: %{w: 3}
      )

    assert resp.status_code in [201, 202]
    assert resp.body["ok"]

    rev = resp.body["rev"]

    resp =
      Couch.put(
        "/#{db_name}/bin_doc7/empty.txt",
        query: %{rev: rev, w: 3},
        body: "",
        headers: ["Content-Type": "text/plain;charset=utf-8"]
      )

    assert resp.status_code in [201, 202]
    rev = resp.body["rev"]

    resp =
      Couch.put(
        "/#{db_name}/bin_doc7/empty.txt",
        query: %{rev: rev, w: 3},
        body: "",
        headers: ["Content-Type": "text/plain;charset=utf-8"]
      )

    assert resp.status_code in [201, 202]
  end

  @tag :with_db
  test "implicit doc creation allows creating docs with a reserved id. COUCHDB-565",
       context do
    db_name = context[:db_name]

    resp =
      Couch.put(
        "/#{db_name}/_nonexistant/attachment.txt",
        body: "ATTACHMENT INFO",
        headers: ["Content-Type": "text/plain;charset=utf-8"],
        query: %{w: 3}
      )

    assert resp.status_code == 400
  end

  @tag :with_db
  test "COUCHDB-809 - stubs should only require the 'stub' field", context do
    db_name = context[:db_name]

    stub_doc = %{
      _id: "stub_doc",
      _attachments: %{
        "foo.txt": %{
          content_type: "text/plain",
          data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
        }
      }
    }

    resp =
      Couch.put(
        "/#{db_name}/stub_doc",
        body: stub_doc,
        query: %{w: 3}
      )

    assert resp.status_code in [201, 202]
    assert resp.body["ok"]

    rev = resp.body["rev"]

    stub_doc =
      Map.merge(stub_doc, %{
        _rev: rev,
        _attachments: %{"foo.txt": %{stub: true}}
      })

    resp =
      Couch.put(
        "/#{db_name}/stub_doc",
        query: %{rev: rev, w: 3},
        body: stub_doc
      )

    assert resp.status_code in [201, 202]
    assert resp.body["ok"]

    rev = resp.body["rev"]

    stub_doc =
      Map.merge(stub_doc, %{
        _rev: rev,
        _attachments: %{"foo.txt": %{stub: true, revpos: 10}}
      })

    resp =
      Couch.put(
        "/#{db_name}/stub_doc",
        query: %{rev: rev},
        body: stub_doc
      )

    assert resp.status_code == 412
    assert resp.body["error"] == "missing_stub"
  end

  @tag :with_db
  test "md5 header for attachments", context do
    db_name = context[:db_name]
    md5 = "MntvB0NYESObxH4VRDUycw=="

    bin_data = "foo bar"

    resp =
      Couch.put(
        "/#{db_name}/bin_doc8/attachment.txt",
        body: bin_data,
        headers: ["Content-Type": "application/octet-stream", "Content-MD5": md5],
        query: %{w: 3}
      )

    assert resp.status_code in [201, 202]
    assert resp.body["ok"]

    resp = Couch.get("/#{db_name}/bin_doc8/attachment.txt")
    assert resp.status_code == 200
    assert md5 == resp.headers["Content-MD5"]
  end

  @tag :with_db
  test "attachment via multipart/form-data", context do
    db_name = context[:db_name]

    form_data_doc = %{
      _id: "form-data-doc"
    }

    resp =
      Couch.put(
        "/#{db_name}/form_data_doc",
        body: form_data_doc,
        query: %{w: 3}
      )

    assert resp.status_code in [201, 202]
    assert resp.body["ok"]
    rev = resp.body["rev"]

    body =
      "------TF\r\n" <>
        "Content-Disposition: form-data; name=\"_rev\"\r\n\r\n" <>
        rev <>
        "\r\n" <>
        "------TF\r\n" <>
        "Content-Disposition: form-data; name=\"_attachments\"; filename=\"file.txt\"\r\n" <>
        "Content-Type: text/plain\r\n\r\n" <>
        "contents of file.txt\r\n\r\n" <> "------TF--"

    resp =
      Couch.post(
        "/#{db_name}/form_data_doc",
        body: body,
        query: %{w: 3},
        headers: [
          Referer: "http://127.0.0.1:15984",
          "Content-Type": "multipart/form-data; boundary=----TF",
          "Content-Length": byte_size(body)
        ]
      )

    assert resp.status_code in [201, 202]
    assert resp.body["ok"]

    resp = Couch.get("/#{db_name}/form_data_doc")
    assert resp.status_code == 200

    doc = resp.body
    assert doc["_attachments"]["file.txt"]["length"] == 22
  end
end

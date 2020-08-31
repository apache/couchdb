defmodule AttachmentMultipartTest do
  use CouchTestCase

  @moduletag :attachments
  @moduletag kind: :single_node

  @moduledoc """
  Test CouchDB attachment multipart requests
  This is a port of the attachments_multipart.js suite
  """

  @tag :with_db
  test "manages attachments multipart requests successfully", context do
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
        "\r\n--abc123\r\n" <> "\r\n" <> "this is 19 chars lo" <> "\r\n--abc123--epilogue"

    resp =
      Couch.put(
        "/#{db_name}/multipart",
        body: multipart_data,
        headers: ["Content-Type": "multipart/related;boundary=\"abc123\""]
      )

    assert resp.status_code in [201, 202]
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
        "\r\n--abc123\r\n" <> "\r\n" <> "this is 18 chars l" <> "\r\n--abc123--"

    resp =
      Couch.put(
        "/#{db_name}/multipart",
        body: multipart_data_updated,
        headers: ["Content-Type": "multipart/related;boundary=\"abc123\""]
      )

    assert resp.status_code in [201, 202]

    resp = Couch.get("/#{db_name}/multipart/bar.txt")

    assert resp.body == "this is 18 chars l"

    resp = Couch.get("/#{db_name}/multipart/baz.txt")

    assert resp.status_code == 404

    resp =
      Couch.get(
        "/#{db_name}/multipart",
        query: %{:attachments => true},
        headers: [accept: "multipart/related,*/*;"]
      )

    assert resp.status_code == 200
    assert resp.headers["Content-length"] == "790"
    # parse out the multipart
    sections = parse_multipart(resp)

    assert length(sections) == 3
    # The first section is the json doc. Check it's content-type.
    # Each part carries their own meta data.

    assert Enum.at(sections, 0).headers["Content-Type"] == "application/json"
    assert Enum.at(sections, 1).headers["Content-Type"] == "application/test"
    assert Enum.at(sections, 2).headers["Content-Type"] == "application/test"

    assert Enum.at(sections, 1).headers["Content-Length"] == "21"
    assert Enum.at(sections, 2).headers["Content-Length"] == "18"

    assert Enum.at(sections, 1).headers["Content-Disposition"] ==
             ~s(attachment; filename="foo.txt")

    assert Enum.at(sections, 2).headers["Content-Disposition"] ==
             ~s(attachment; filename="bar.txt")

    doc = :jiffy.decode(Enum.at(sections, 0).body, [:return_maps])

    assert doc["_attachments"]["foo.txt"]["follows"] == true
    assert doc["_attachments"]["bar.txt"]["follows"] == true

    assert Enum.at(sections, 1).body == "this is 21 chars long"
    assert Enum.at(sections, 2).body == "this is 18 chars l"

    # now get attachments incrementally (only the attachments changes since
    # a certain rev).

    resp =
      Couch.get(
        "/#{db_name}/multipart",
        query: %{:atts_since => ~s(["#{first_rev}"])},
        headers: [accept: "multipart/related,*/*;"]
      )

    assert resp.status_code == 200

    sections = parse_multipart(resp)
    assert length(sections) == 2

    doc = :jiffy.decode(Enum.at(sections, 0).body, [:return_maps])

    assert doc["_attachments"]["foo.txt"]["stub"] == true
    assert doc["_attachments"]["bar.txt"]["follows"] == true
    assert Enum.at(sections, 1).body == "this is 18 chars l"

    # try the atts_since parameter together with the open_revs parameter
    resp =
      Couch.get(
        "/#{db_name}/multipart",
        query: %{
          :open_revs => ~s(["#{doc["_rev"]}"]),
          :atts_since => ~s(["#{first_rev}"])
        },
        headers: [accept: "multipart/related,*/*;"]
      )

    assert resp.status_code == 200
    sections = parse_multipart(resp)

    # 1 section, with a multipart/related Content-Type
    assert length(sections) == 1

    ctype_value = Enum.at(sections, 0).headers["Content-Type"]
    assert String.starts_with?(ctype_value, "multipart/related;") == true

    inner_sections = parse_multipart(Enum.at(sections, 0))
    # 2 inner sections: a document body section plus an attachment data section
    assert length(inner_sections) == 3
    assert Enum.at(inner_sections, 0).headers["Content-Type"] == "application/json"

    doc = :jiffy.decode(Enum.at(inner_sections, 0).body, [:return_maps])
    assert doc["_attachments"]["foo.txt"]["follows"] == true
    assert doc["_attachments"]["bar.txt"]["follows"] == true

    assert Enum.at(inner_sections, 1).body == "this is 21 chars long"
    assert Enum.at(inner_sections, 2).body == "this is 18 chars l"

    # try it with a rev that doesn't exist (should get all attachments)

    resp =
      Couch.get(
        "/#{db_name}/multipart",
        query: %{
          :atts_since => ~s(["1-2897589","#{first_rev}"])
        },
        headers: [accept: "multipart/related,*/*;"]
      )

    assert resp.status_code == 200
    sections = parse_multipart(resp)

    assert length(sections) == 2

    doc = :jiffy.decode(Enum.at(sections, 0).body, [:return_maps])
    assert doc["_attachments"]["foo.txt"]["stub"] == true
    assert doc["_attachments"]["bar.txt"]["follows"] == true
    assert Enum.at(sections, 1).body == "this is 18 chars l"
  end

  @tag :with_db
  test "manages compressed attachments successfully", context do
    db_name = context[:db_name]

    # check that with the document multipart/mixed API it's possible to receive
    #  attachments in compressed form (if they're stored in compressed form)
    server_config = [
      %{
        :section => "attachments",
        :key => "compression_level",
        :value => "8"
      },
      %{
        :section => "attachments",
        :key => "compressible_types",
        :value => "text/plain"
      }
    ]

    run_on_modified_server(
      server_config,
      fn -> test_multipart_att_compression(db_name) end
    )
  end

  defp test_multipart_att_compression(dbname) do
    doc = %{
      "_id" => "foobar"
    }

    lorem = Couch.get("/_utils/script/test/lorem.txt").body
    hello_data = "hello world"
    {_, resp} = create_doc(dbname, doc)
    first_rev = resp.body["rev"]

    resp =
      Couch.put(
        "/#{dbname}/#{doc["_id"]}/data.bin",
        query: %{:rev => first_rev},
        body: hello_data,
        headers: ["Content-Type": "application/binary"]
      )

    assert resp.status_code in [201, 202]
    second_rev = resp.body["rev"]

    resp =
      Couch.put(
        "/#{dbname}/#{doc["_id"]}/lorem.txt",
        query: %{:rev => second_rev},
        body: lorem,
        headers: ["Content-Type": "text/plain"]
      )

    assert resp.status_code in [201, 202]
    third_rev = resp.body["rev"]

    resp =
      Couch.get(
        "/#{dbname}/#{doc["_id"]}",
        query: %{:open_revs => ~s(["#{third_rev}"])},
        headers: [Accept: "multipart/mixed", "X-CouchDB-Send-Encoded-Atts": "true"]
      )

    assert resp.status_code == 200
    sections = parse_multipart(resp)
    # 1 section, with a multipart/related Content-Type
    assert length(sections) == 1
    ctype_value = Enum.at(sections, 0).headers["Content-Type"]
    assert String.starts_with?(ctype_value, "multipart/related;") == true

    inner_sections = parse_multipart(Enum.at(sections, 0))
    # 3 inner sections: a document body section plus 2 attachment data sections
    assert length(inner_sections) == 3
    assert Enum.at(inner_sections, 0).headers["Content-Type"] == "application/json"

    doc = :jiffy.decode(Enum.at(inner_sections, 0).body, [:return_maps])
    assert doc["_attachments"]["lorem.txt"]["follows"] == true
    assert doc["_attachments"]["lorem.txt"]["encoding"] == "gzip"
    assert doc["_attachments"]["data.bin"]["follows"] == true
    assert doc["_attachments"]["data.bin"]["encoding"] != "gzip"

    if Enum.at(inner_sections, 1).body == hello_data do
      assert Enum.at(inner_sections, 2).body != lorem
    else
      if assert Enum.at(inner_sections, 2).body == hello_data do
        assert Enum.at(inner_sections, 1).body != lorem
      else
        assert false, "Could not found data.bin attachment data"
      end
    end

    # now test that it works together with the atts_since parameter

    resp =
      Couch.get(
        "/#{dbname}/#{doc["_id"]}",
        query: %{:open_revs => ~s(["#{third_rev}"]), :atts_since => ~s(["#{second_rev}"])},
        headers: [Accept: "multipart/mixed", "X-CouchDB-Send-Encoded-Atts": "true"]
      )

    assert resp.status_code == 200
    sections = parse_multipart(resp)
    # 1 section, with a multipart/related Content-Type

    assert length(sections) == 1
    ctype_value = Enum.at(sections, 0).headers["Content-Type"]
    assert String.starts_with?(ctype_value, "multipart/related;") == true

    inner_sections = parse_multipart(Enum.at(sections, 0))
    # 3 inner sections: a document body section plus 2 attachment data sections
    assert length(inner_sections) == 3
    assert Enum.at(inner_sections, 0).headers["Content-Type"] == "application/json"
    doc = :jiffy.decode(Enum.at(inner_sections, 0).body, [:return_maps])
    assert doc["_attachments"]["lorem.txt"]["follows"] == true
    assert doc["_attachments"]["lorem.txt"]["encoding"] == "gzip"
    assert Enum.at(inner_sections, 1).body != lorem
  end

  def get_boundary(response) do
    ctype = response.headers["Content-Type"]
    ctype_args = String.split(ctype, "; ")
    ctype_args = Enum.slice(ctype_args, 1, length(ctype_args))

    boundary_arg =
      Enum.find(
        ctype_args,
        fn arg -> String.starts_with?(arg, "boundary=") end
      )

    boundary = Enum.at(String.split(boundary_arg, "="), 1)

    if String.starts_with?(boundary, ~s(")) do
      :jiffy.decode(boundary)
    else
      boundary
    end
  end

  def parse_multipart(response) do
    boundary = get_boundary(response)

    leading = "--#{boundary}\r\n"
    last = "\r\n--#{boundary}--"
    body = response.body
    mimetext = Enum.at(String.split(body, leading, parts: 2), 1)
    mimetext = Enum.at(String.split(mimetext, last, parts: 2), 0)

    sections = String.split(mimetext, ~s(\r\n--#{boundary}))

    Enum.map(sections, fn section ->
      section_parts = String.split(section, "\r\n\r\n", parts: 2)
      raw_headers = String.split(Enum.at(section_parts, 0), "\r\n")
      body = Enum.at(section_parts, 1)

      headers =
        Enum.reduce(raw_headers, %{}, fn raw_header, acc ->
          if raw_header != "" do
            header_parts = String.split(raw_header, ": ")
            Map.put(acc, Enum.at(header_parts, 0), Enum.at(header_parts, 1))
          else
            acc
          end
        end)

      %{
        :headers => headers,
        :body => body
      }
    end)
  end
end

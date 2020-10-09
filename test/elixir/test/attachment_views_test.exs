defmodule AttachmentViewTest do
  use CouchTestCase

  @moduletag :attachments
  @moduletag kind: :single_node

  @moduledoc """
  Test CouchDB attachment views requests
  This is a port of the attachment_views.js suite
  """

  @tag :with_db
  test "manages attachments in views successfully", context do
    db_name = context[:db_name]
    attachment_data = "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="

    attachment_template_1 = %{
      "_attachments" => %{
        "foo.txt" => %{
          "content_type" => "text/plain",
          "data" => attachment_data
        }
      }
    }

    attachment_template_2 = %{
      "_attachments" => %{
        "foo.txt" => %{
          "content_type" => "text/plain",
          "data" => attachment_data
        },
        "bar.txt" => %{
          "content_type" => "text/plain",
          "data" => attachment_data
        }
      }
    }

    attachment_template_3 = %{
      "_attachments" => %{
        "foo.txt" => %{
          "content_type" => "text/plain",
          "data" => attachment_data
        },
        "bar.txt" => %{
          "content_type" => "text/plain",
          "data" => attachment_data
        },
        "baz.txt" => %{
          "content_type" => "text/plain",
          "data" => attachment_data
        }
      }
    }

    bulk_save(db_name, make_docs(0..9))
    bulk_save(db_name, make_docs(10..19, attachment_template_1))
    bulk_save(db_name, make_docs(20..29, attachment_template_2))
    bulk_save(db_name, make_docs(30..39, attachment_template_3))

    map_function = """
    function(doc) {
    var count = 0;

    for(var idx in doc._attachments) {
      count = count + 1;
    }

    emit(parseInt(doc._id), count);
    }
    """

    reduce_function = """
    function(key, values) {
    return sum(values);
    }
    """

    result = query(db_name, map_function, reduce_function)
    assert length(result["rows"]) == 1
    assert Enum.at(result["rows"], 0)["value"] == 60

    result =
      query(db_name, map_function, reduce_function, %{
        startkey: 10,
        endkey: 19
      })

    assert length(result["rows"]) == 1
    assert Enum.at(result["rows"], 0)["value"] == 10

    result = query(db_name, map_function, reduce_function, %{startkey: 20, endkey: 29})
    assert length(result["rows"]) == 1
    assert Enum.at(result["rows"], 0)["value"] == 20

    result =
      query(db_name, map_function, nil, %{
        startkey: 30,
        endkey: 39,
        include_docs: true
      })

    assert length(result["rows"]) == 10
    assert Enum.at(result["rows"], 0)["value"] == 3
    attachment = Enum.at(result["rows"], 0)["doc"]["_attachments"]["baz.txt"]
    assert attachment["stub"] == true
    assert Map.has_key?(attachment, "data") == false
    assert Map.has_key?(attachment, "encoding") == false
    assert Map.has_key?(attachment, "encoded_length") == false

    result =
      query(db_name, map_function, nil, %{
        startkey: 30,
        endkey: 39,
        include_docs: true,
        attachments: true
      })

    assert length(result["rows"]) == 10
    assert Enum.at(result["rows"], 0)["value"] == 3
    attachment = Enum.at(result["rows"], 0)["doc"]["_attachments"]["baz.txt"]
    assert attachment["data"] == attachment_data
    assert Map.has_key?(attachment, "stub") == false
    assert Map.has_key?(attachment, "encoding") == false
    assert Map.has_key?(attachment, "encoded_length") == false

    result =
      query(db_name, map_function, nil, %{
        startkey: 30,
        endkey: 39,
        include_docs: true,
        att_encoding_info: true
      })

    assert length(result["rows"]) == 10
    assert Enum.at(result["rows"], 0)["value"] == 3
    attachment = Enum.at(result["rows"], 0)["doc"]["_attachments"]["baz.txt"]
    assert attachment["stub"] == true
    assert attachment["encoding"] == "gzip"
    assert attachment["encoded_length"] == 47
    assert Map.has_key?(attachment, "data") == false
  end
end

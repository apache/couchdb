defmodule ViewOffsetTest do
  use CouchTestCase

  @moduletag :view_offsets
  @moduletag kind: :single_node

  @moduledoc """
  Tests about view offsets.
  This is a port of the view_offsets.js javascript test suite.
  """

  @docs [
    %{"_id" => "a1", "letter" => "a", "number" => 1, "foo" => "bar"},
    %{"_id" => "a2", "letter" => "a", "number" => 2, "foo" => "bar"},
    %{"_id" => "a3", "letter" => "a", "number" => 3, "foo" => "bar"},
    %{"_id" => "b1", "letter" => "b", "number" => 1, "foo" => "bar"},
    %{"_id" => "b2", "letter" => "b", "number" => 2, "foo" => "bar"},
    %{"_id" => "b3", "letter" => "b", "number" => 3, "foo" => "bar"},
    %{"_id" => "b4", "letter" => "b", "number" => 4, "foo" => "bar"},
    %{"_id" => "b5", "letter" => "b", "number" => 5, "foo" => "bar"},
    %{"_id" => "c1", "letter" => "c", "number" => 1, "foo" => "bar"},
    %{"_id" => "c2", "letter" => "c", "number" => 2, "foo" => "bar"}
  ]

  @design_doc %{
    "_id" => "_design/test",
    "views" => %{
      "offset" => %{
        "map" => "function(doc) { emit([doc.letter, doc.number], doc); }"
      }
    }
  }

  @tag :with_db
  test "basic view offsets", context do
    db_name = context[:db_name]
    save(db_name, @design_doc)
    bulk_save(db_name, @docs)

    [
      [["c", 2], 0],
      [["c", 1], 1],
      [["b", 5], 2],
      [["b", 4], 3],
      [["b", 3], 4],
      [["b", 2], 5],
      [["b", 1], 6],
      [["a", 3], 7],
      [["a", 2], 8],
      [["a", 1], 9]
    ]
    |> Enum.each(fn [start_key, offset] ->
      result =
        view(db_name, "test/offset", %{
          "startkey" => :jiffy.encode(start_key),
          "descending" => true
        })

      assert result.body["offset"] === offset
    end)
  end

  test "repeated view offsets" do
    0..14 |> Enum.each(fn _ -> repeated_view_offset_test_fun end)
  end

  def repeated_view_offset_test_fun do
    db_name = random_db_name()
    create_db(db_name)

    save(db_name, @design_doc)
    bulk_save(db_name, @docs)

    first_response =
      view(db_name, "test/offset", %{
        "startkey" => :jiffy.encode(["b", 4]),
        "startkey_docid" => "b4",
        "endkey" => :jiffy.encode(["b"]),
        "descending" => true,
        "limit" => 2,
        "skip" => 1
      })

    second_response =
      view(db_name, "test/offset", %{
        "startkey" => :jiffy.encode(["c", 3])
      })

    third_response =
      view(db_name, "test/offset", %{
        "startkey" => :jiffy.encode(["b", 6]),
        "endkey" => :jiffy.encode(["b", 7])
      })

    assert first_response.body["offset"] === 4
    assert second_response.body["offset"] === length(@docs)
    assert third_response.body["offset"] === 8

    delete_db(db_name)
  end
end

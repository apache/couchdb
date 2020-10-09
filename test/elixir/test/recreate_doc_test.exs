defmodule RecreateDocTest do
  use CouchTestCase

  @moduletag :recreate_doc
  @moduletag kind: :single_node

  @moduledoc """
  Test CouchDB document recreation
  This is a port of the recreate_doc.js suite
  """

  @tag :with_db
  test "recreate document", context do
    db_name = context[:db_name]

    # First create a new document with the ID "foo", and delete it again
    doc = %{_id: "foo", a: "bar", b: 42}
    {:ok, resp} = create_doc(db_name, doc)
    first_rev = resp.body["rev"]

    resp = Couch.delete("/#{db_name}/foo?rev=#{first_rev}")
    assert resp.status_code == 200

    # Now create a new document with the same ID, save it, and then modify it
    doc = %{_id: "foo"}

    for _i <- 0..9 do
      {:ok, _} = create_doc(db_name, doc)
      resp = Couch.get("/#{db_name}/foo")

      updated_doc =
        resp.body
        |> Map.put("a", "baz")

      resp = Couch.put("/#{db_name}/foo", body: updated_doc)
      assert resp.status_code == 201
      rev = resp.body["rev"]
      resp = Couch.delete("/#{db_name}/foo?rev=#{rev}")
      assert resp.status_code == 200
    end
  end

  @tag :with_db
  test "COUCHDB-292 - recreate a deleted document", context do
    db_name = context[:db_name]
    # First create a new document with the ID "foo", and delete it again
    doc = %{_id: "foo", a: "bar", b: 42}
    {:ok, resp} = create_doc(db_name, doc)
    first_rev = resp.body["rev"]

    resp = Couch.delete("/#{db_name}/foo?rev=#{first_rev}")
    assert resp.status_code == 200

    # COUCHDB-292 now attempt to save the document with a prev that's since
    # been deleted and this should generate a conflict exception
    updated_doc =
      doc
      |> Map.put(:_rev, first_rev)

    resp = Couch.put("/#{db_name}/foo", body: updated_doc)
    assert resp.status_code == 409

    # same as before, but with binary
    bin_att_doc = %{
      _id: "foo",
      _rev: first_rev,
      _attachments: %{
        "foo.txt": %{
          content_type: "text/plain",
          data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
        }
      }
    }

    resp = Couch.put("/#{db_name}/foo", body: bin_att_doc)
    assert resp.status_code == 409
  end

  @tag :with_db
  test "Recreate a deleted document with non-exsistant rev", context do
    db_name = context[:db_name]

    doc = %{_id: "foo", a: "bar", b: 42}
    {:ok, resp} = create_doc(db_name, doc)
    first_rev = resp.body["rev"]

    resp = Couch.delete("/#{db_name}/foo?rev=#{first_rev}")
    assert resp.status_code == 200

    # random non-existant prev rev
    updated_doc =
      doc
      |> Map.put(:_rev, "1-asfafasdf")

    resp = Couch.put("/#{db_name}/foo", body: updated_doc)
    assert resp.status_code == 409

    # random non-existant prev rev with bin
    bin_att_doc = %{
      _id: "foo",
      _rev: "1-aasasfasdf",
      _attachments: %{
        "foo.txt": %{
          content_type: "text/plain",
          data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
        }
      }
    }

    resp = Couch.put("/#{db_name}/foo", body: bin_att_doc)
    assert resp.status_code == 409
  end

  @tag :with_db
  test "COUCHDB-1265 - changes feed after we try and break the update_seq tree",
       context do
    db_name = context[:db_name]

    # Test COUCHDB-1265 - Reinserting an old revision into the revision tree causes
    # duplicates in the update_seq tree.
    revs = create_rev_doc(db_name, "a", 3)

    resp =
      Couch.put("/#{db_name}/a",
        body: Enum.at(revs, 0),
        query: [new_edits: false]
      )

    assert resp.status_code == 201

    resp =
      Couch.put("/#{db_name}/a",
        body: Enum.at(revs, -1)
      )

    assert resp.status_code == 201

    resp = Couch.get("/#{db_name}/_changes")
    assert resp.status_code == 200

    assert length(resp.body["results"]) == 1
  end

  # function to create a doc with multiple revisions
  defp create_rev_doc(db_name, id, num_revs) do
    doc = %{_id: id, count: 0}
    {:ok, resp} = create_doc(db_name, doc)
    create_rev_doc(db_name, id, num_revs, [Map.put(doc, :_rev, resp.body["rev"])])
  end

  defp create_rev_doc(db_name, id, num_revs, revs) do
    if length(revs) < num_revs do
      doc = %{_id: id, _rev: Enum.at(revs, -1)[:_rev], count: length(revs)}
      {:ok, resp} = create_doc(db_name, doc)

      create_rev_doc(
        db_name,
        id,
        num_revs,
        revs ++ [Map.put(doc, :_rev, resp.body["rev"])]
      )
    else
      revs
    end
  end
end

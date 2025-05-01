defmodule DropSeqTest do
  use CouchTestCase

  @moduletag :drop_seq

  @tag :with_db
  test "dropped document vanishes after compaction", context do
    db_name = context[:db_name]
    ddoc_id = "_design/foo"
    doc_id = "testdoc"

    # create something that will maintain a peer checkpoint
    resp = Couch.put("/#{db_name}/#{ddoc_id}", body: %{
      views: %{
        bar: %{
          map: "function(doc) { emit(); }"
        }
      }
    })
    assert resp.status_code == 201

    # create a document document
    resp = Couch.put("/#{db_name}/#{doc_id}", body: %{})
    assert resp.status_code == 201
    rev = resp.body["rev"]

    # delete it
    resp = Couch.delete("/#{db_name}/#{doc_id}?rev=#{rev}")
    assert resp.status_code == 200

    # update view and its peer checkpoint
    assert Couch.get("/#{db_name}/#{ddoc_id}/_view/bar").status_code == 200

    # update drop seq
    resp = Couch.post("/#{db_name}/_update_drop_seq")
    retry_until(
      fn ->
        resp = Couch.post("/#{db_name}/_update_drop_seq")
        assert resp.status_code == 201
        map_size(resp.body["results"]) == 1
      end,
      200,
      10_000
    )

    # confirm deleted doc is still in _changes response
    resp = Couch.get("/#{db_name}/_changes")
    assert resp.status_code == 200
    assert length(resp.body["results"]) == 2
    assert Enum.at(resp.body["results"], 1)["id"] == doc_id
    assert Enum.at(resp.body["results"], 1)["deleted"]

    # compact
    compact(db_name)

    # confirm deleted doc is not in _changes response
    resp = Couch.get("/#{db_name}/_changes")
    assert resp.status_code == 200
    assert length(resp.body["results"]) == 1 # just the ddoc left.

  end

end

defmodule DelayedCommitsTest do
  use CouchTestCase

  @moduledoc """
  Test CouchDB delayed commits
  This is a port of the delayed_commits.js suite

  Note that delayed_commits is deprecated in 2.0, so this is a minimal
  test to show it still works. delayed_commits will be removed in 3.0.

  This test is now skipped. Its a bit of a flaky test so no point running it
  since we are removing this feature.
  """

  @tag :pending
  @tag config: [
         {"couchdb", "delayed_commits", "true"}
       ]
  @tag :with_db
  test "delayed commit", context do
    db_name = context[:db_name]
    doc_id = "doc-1"
    resp = Couch.put("/#{db_name}/#{doc_id}", body: %{a: 2, b: 4})
    assert resp.status_code in 201..204
    assert resp.body["ok"]

    resp = Couch.get("/#{db_name}/#{doc_id}")
    assert resp.status_code == 200, "The new doc should be in the database"

    restart_cluster()

    resp = Couch.get("/#{db_name}/#{doc_id}")
    assert resp.status_code == 404, "The new doc should be missing"
  end
end

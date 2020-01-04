defmodule EtagsViewsTest do
  use CouchTestCase

  @moduletag :etags

  @tag :with_db
  test "etag header on creation", context do
    db_name = context[:db_name]

    resp =
      Couch.put("/#{db_name}/1",
        headers: ["Content-Type": "application/json"],
        body: %{}
      )

    assert resp.status_code == 201
    assert Map.has_key?(resp.headers.hdrs, "etag")
  end


end

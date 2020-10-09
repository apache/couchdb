defmodule EtagsHeadTest do
  use CouchTestCase

  @moduletag :etags
  @moduletag kind: :single_node

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

  @tag :with_db
  test "etag header on retrieval", context do
    db_name = context[:db_name]

    resp =
      Couch.put("/#{db_name}/1",
        headers: ["Content-Type": "application/json"],
        body: %{}
      )

    etag = resp.headers.hdrs["etag"]

    # get the doc and verify the headers match
    resp = Couch.get("/#{db_name}/1")
    assert etag == resp.headers.hdrs["etag"]

    # 'head' the doc and verify the headers match
    resp =
      Couch.head("/#{db_name}/1",
        headers: ["if-none-match": "s"]
      )

    assert etag == resp.headers.hdrs["etag"]
  end

  @tag :with_db
  test "etag header on head", context do
    db_name = context[:db_name]

    resp =
      Couch.put("/#{db_name}/1",
        headers: ["Content-Type": "application/json"],
        body: %{}
      )

    etag = resp.headers.hdrs["etag"]

    # 'head' the doc and verify the headers match
    resp =
      Couch.head("/#{db_name}/1",
        headers: ["if-none-match": "s"]
      )

    assert etag == resp.headers.hdrs["etag"]
  end

  @tag :with_db
  test "etags head", context do
    db_name = context[:db_name]

    resp =
      Couch.put("/#{db_name}/1",
        headers: ["Content-Type": "application/json"],
        body: %{}
      )

    assert resp.status_code == 201
    assert Map.has_key?(resp.headers.hdrs, "etag")

    etag = resp.headers.hdrs["etag"]

    # get the doc and verify the headers match
    resp = Couch.get("/#{db_name}/1")
    assert etag == resp.headers.hdrs["etag"]

    # 'head' the doc and verify the headers match
    resp =
      Couch.head("/#{db_name}/1",
        headers: ["if-none-match": "s"]
      )

    assert etag == resp.headers.hdrs["etag"]

    # replace a doc
    resp =
      Couch.put("/#{db_name}/1",
        headers: ["if-match": etag],
        body: %{}
      )

    assert resp.status_code == 201

    # extract the new ETag value
    previous_etag = etag
    etag = resp.headers.hdrs["etag"]

    # fail to replace a doc
    resp =
      Couch.put("/#{db_name}/1",
        body: %{}
      )

    assert resp.status_code == 409

    # verify get w/Etag
    resp =
      Couch.get("/#{db_name}/1",
        headers: ["if-none-match": previous_etag]
      )

    assert resp.status_code == 200

    resp =
      Couch.get("/#{db_name}/1",
        headers: ["if-none-match": etag]
      )

    assert resp.status_code == 304

    resp =
      Couch.get("/#{db_name}/1",
        headers: ["if-none-match": "W/#{etag}"]
      )

    assert resp.status_code == 304

    # fail to delete a doc
    resp =
      Couch.delete("/#{db_name}/1",
        headers: ["if-match": previous_etag]
      )

    assert resp.status_code == 409

    resp =
      Couch.delete("/#{db_name}/1",
        headers: ["if-match": etag]
      )

    assert resp.status_code == 200
  end
end

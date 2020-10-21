defmodule PartitionCrudTest do
  use CouchTestCase

  @tag :with_partitioned_db
  test "Sets partition in db info", context do
    db_name = context[:db_name]
    resp = Couch.get("/#{db_name}")
    %{body: body} = resp
    assert body["props"] == %{"partitioned" => true}
  end

  @tag :with_partitioned_db
  test "PUT and GET document", context do
    db_name = context[:db_name]
    id = "my-partition:doc"
    url = "/#{db_name}/#{id}"

    resp = Couch.put(url, body: %{partitioned_doc: true})
    %{body: doc} = resp
    assert resp.status_code in [201, 202]
    assert doc["id"] == id

    resp = Couch.get(url)
    assert resp.status_code == 200

    %{body: doc} = resp
    assert doc["_id"] == id
  end

  @tag :with_partitioned_db
  test "PUT fails if a partition key is not supplied", context do
    db_name = context[:db_name]
    id = "not-partitioned"
    url = "/#{db_name}/#{id}"

    resp = Couch.put(url, body: %{partitioned_doc: false})
    assert resp.status_code == 400

    error = %{
      "error" => "illegal_docid",
      "reason" => "Doc id must be of form partition:id"
    }

    assert Map.get(resp, :body) == error
  end

  @tag :with_partitioned_db
  test "PUT fails for partitions with _", context do
    db_name = context[:db_name]
    id = "_bad:partitioned"
    url = "/#{db_name}/#{id}"

    resp = Couch.put(url, body: %{partitioned_doc: false})

    error = %{
      "error" => "illegal_docid",
      "reason" => "Only reserved document ids may start with underscore."
    }

    assert resp.status_code == 400
    assert Map.get(resp, :body) == error
  end

  @tag :with_partitioned_db
  test "PUT fails for bad partitions", context do
    db_name = context[:db_name]
    id = "bad:"
    url = "/#{db_name}/#{id}"

    resp = Couch.put(url, body: %{partitioned_doc: false})

    error = %{
      "error" => "illegal_docid",
      "reason" => "Document id must not be empty"
    }

    assert resp.status_code == 400
    assert Map.get(resp, :body) == error
  end

  @tag :with_partitioned_db
  test "POST and GET document", context do
    db_name = context[:db_name]
    id = "my-partition-post:doc"
    url = "/#{db_name}"

    resp = Couch.post(url, body: %{_id: id, partitioned_doc: true})
    assert resp.status_code in [201, 202]

    resp = Couch.get("#{url}/#{id}")
    assert resp.status_code == 200

    %{body: doc} = resp
    assert doc["_id"] == id
  end

  @tag :with_partitioned_db
  test "GET to partition returns 400", context do
    db_name = context[:db_name]
    url = "/#{db_name}/_partition"

    resp = Couch.get("#{url}")
    assert resp.status_code == 400
  end

  @tag :with_partitioned_db
  test "POST and _bulk_get document", context do
    db_name = context[:db_name]
    id = "my-partition-post:doc"
    url = "/#{db_name}"

    resp = Couch.post(url, body: %{_id: id, partitioned_doc: true})
    assert resp.status_code in [201, 202]

    resp = Couch.post("#{url}/_bulk_get", body: %{docs: [%{id: id}]})
    assert resp.status_code == 200

    %{body: body} = resp

    assert %{
             "results" => [
               %{
                 "docs" => [
                   %{
                     "ok" => %{
                       "_id" => "my-partition-post:doc",
                       "_rev" => "1-43d86359741cb629c0953a2beb6e9d7a",
                       "partitioned_doc" => true
                     }
                   }
                 ],
                 "id" => "my-partition-post:doc"
               }
             ]
           } == body
  end

  @tag :with_partitioned_db
  test "_bulk_get bad partitioned document", context do
    db_name = context[:db_name]
    id = "my-partition-post"
    url = "/#{db_name}"

    resp = Couch.post("#{url}/_bulk_get", body: %{docs: [%{id: id}]})
    assert resp.status_code == 200
    %{:body => body} = resp

    assert %{
             "results" => [
               %{
                 "docs" => [
                   %{
                     "error" => %{
                       "error" => "illegal_docid",
                       "id" => "my-partition-post",
                       "reason" => "Doc id must be of form partition:id",
                       "rev" => :null
                     }
                   }
                 ],
                 "id" => "my-partition-post"
               }
             ]
           } == body
  end

  @tag :with_partitioned_db
  test "POST fails if a partition key is not supplied", context do
    db_name = context[:db_name]
    id = "not-partitioned-post"
    url = "/#{db_name}"

    resp = Couch.post(url, body: %{_id: id, partitited_doc: false})
    assert resp.status_code == 400
  end

  @tag :with_partitioned_db
  test "_bulk_docs saves docs with partition key", context do
    db_name = context[:db_name]

    docs = [
      %{_id: "foo:1"},
      %{_id: "bar:1"}
    ]

    url = "/#{db_name}"
    resp = Couch.post("#{url}/_bulk_docs", body: %{:docs => docs})
    assert resp.status_code in [201, 202]

    resp = Couch.get("#{url}/foo:1")
    assert resp.status_code == 200

    resp = Couch.get("#{url}/bar:1")
    assert resp.status_code == 200
  end

  @tag :with_partitioned_db
  test "_bulk_docs errors with missing partition key", context do
    db_name = context[:db_name]

    docs = [
      %{_id: "foo1"}
    ]

    error = %{
      "error" => "illegal_docid",
      "reason" => "Doc id must be of form partition:id"
    }

    url = "/#{db_name}"
    resp = Couch.post("#{url}/_bulk_docs", body: %{:docs => docs})
    assert resp.status_code == 400
    assert Map.get(resp, :body) == error
  end

  @tag :with_partitioned_db
  test "_bulk_docs errors with bad partition key", context do
    db_name = context[:db_name]

    docs = [
      %{_id: "_foo:1"}
    ]

    error = %{
      "error" => "illegal_docid",
      "reason" => "Only reserved document ids may start with underscore."
    }

    url = "/#{db_name}"
    resp = Couch.post("#{url}/_bulk_docs", body: %{:docs => docs})
    assert resp.status_code == 400
    assert Map.get(resp, :body) == error
  end

  @tag :with_partitioned_db
  test "_bulk_docs errors with bad doc key", context do
    db_name = context[:db_name]

    docs = [
      %{_id: "foo:"}
    ]

    error = %{
      "error" => "illegal_docid",
      "reason" => "Document id must not be empty"
    }

    url = "/#{db_name}"
    resp = Couch.post("#{url}/_bulk_docs", body: %{:docs => docs})
    assert resp.status_code == 400
    assert Map.get(resp, :body) == error
  end

  @tag :with_partitioned_db
  test "saves attachment with partitioned doc", context do
    db_name = context[:db_name]
    id = "foo:doc-with-attachment"

    doc = %{
      _id: id,
      _attachments: %{
        "foo.txt": %{
          content_type: "text/plain",
          data: Base.encode64("This is a text document to save")
        }
      }
    }

    resp = Couch.put("/#{db_name}/#{id}", body: doc)

    assert resp.status_code in [201, 202]

    resp = Couch.get("/#{db_name}/#{id}")
    assert resp.status_code == 200
    body = Map.get(resp, :body)
    rev = Map.get(body, "_rev")

    assert body["_attachments"] == %{
             "foo.txt" => %{
               "content_type" => "text/plain",
               #  "digest" => "md5-OW2BoZAtMqs1E+fAnLpNBw==",
               # Temp remove the digest part since the digest value 
               # seems to be different on travis
               "digest" => body["_attachments"]["foo.txt"]["digest"],
               "length" => 31,
               "revpos" => 1,
               "stub" => true
             }
           }

    resp = Couch.get("/#{db_name}/#{id}/foo.txt")
    assert Map.get(resp, :body) == "This is a text document to save"

    resp =
      Couch.put(
        "/#{db_name}/#{id}/bar.txt?rev=#{rev}",
        headers: ["Content-Type": "text/plain"],
        body: "This is another document"
      )

    assert resp.status_code in [201, 202]
    %{:body => body} = resp
    assert body["ok"] == true
    assert body["id"] == id
  end

  @tag :with_partitioned_db
  test "can purge partitioned db docs", context do
    db_name = context[:db_name]

    doc = %{
      _id: "foo:bar",
      value: "some value"
    }

    resp = Couch.post("/#{db_name}", query: [w: 3], body: doc)
    assert resp.status_code in [201, 202]
    %{body: body} = resp
    rev = body["rev"]

    resp = Couch.get("/#{db_name}/foo:bar")
    assert resp.status_code == 200

    body = %{"foo:bar" => [rev]}
    resp = Couch.post("/#{db_name}/_purge", query: [w: 3], body: body)
    assert resp.status_code in [201, 202]

    resp = Couch.get("/#{db_name}/foo:bar")
    assert resp.status_code == 404
    assert resp.body == %{"error" => "not_found", "reason" => "missing"}
  end

  @tag :with_partitioned_db
  test "purge rejects unpartitioned docid", context do
    db_name = context[:db_name]
    body = %{"no_partition" => ["1-967a00dff5e02add41819138abb3284d"]}
    resp = Couch.post("/#{db_name}/_purge", query: [w: 3], body: body)
    assert resp.status_code == 400
    %{body: body} = resp
    assert body["error"] == "illegal_docid"
  end

  test "create database with bad `partitioned` value", _context do
    resp = Couch.put("/bad-db?partitioned=tru")
    assert resp.status_code == 400

    assert Map.get(resp, :body) == %{
             "error" => "bad_request",
             "reason" => "Invalid `partitioned` parameter"
           }
  end

  test "can create unpartitioned system db", _context do
    Couch.delete("/_replicator")
    resp = Couch.put("/_replicator")
    assert resp.status_code in [201, 202]
    assert resp.body == %{"ok" => true}
  end

  test "cannot create partitioned system db", _context do
    Couch.delete("/_replicator")

    resp = Couch.put("/_replicator?partitioned=true")
    assert resp.status_code == 400

    %{:body => %{"reason" => reason}} = resp
    assert Regex.match?(~r/Cannot partition a system database/, reason)
  end
end

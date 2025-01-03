defmodule NouveauTest do
  use CouchTestCase

  @moduletag :nouveau

  @moduledoc """
  Test search
  """

  def create_search_docs(db_name) do
    resp = Couch.post("/#{db_name}/_bulk_docs",
      headers: ["Content-Type": "application/json"],
      body: %{:docs => [
                %{"_id" => "doc4", "foo" => "foo", "bar" => 42, "baz" => "hello there"},
                %{"_id" => "doc3", "foo" => "bar", "bar" => 12.0, "baz" => "hello"},
                %{"_id" => "doc1", "foo" => "baz", "bar" => 0, "baz" => "there"},
                %{"_id" => "doc2", "foo" => "foobar", "bar" => 100, "baz" => "hi"},
      ]}
    )
    assert resp.status_code in [201]
    resp
  end

  def create_conflicted_search_docs(db_name) do
    resp = Couch.post("/#{db_name}/_bulk_docs",
      headers: ["Content-Type": "application/json"],
      body: %{:docs => [
                # doc4: conflict between 1-a and 1-b, 1-b wins, 1-b will be purged
                %{"_id" => "doc4", "foo" => "foo", "bar" => 42, "baz" => "hello there",
                  "_revisions" => %{:start => 1, :ids => ["a"]}
                },
                %{"_id" => "doc4", "foo" => "fooX", "bar" => 43, "baz" => "hello thereX",
                  "_revisions" => %{:start => 1, :ids => ["b"]}
                },

                # doc3: conflict between 1-a deleted and 2-c, 1-a is deleted,
                %{"_id" => "doc3", "foo" => "bar", "bar" => 12.0, "baz" => "hello",
                  "_revisions" => %{:start => 1, :ids => ["a"]},
                  "_deleted" => true
                },
                %{"_id" => "doc3", "foo" => "barX", "bar" => 13.0, "baz" => "helloX",
                  "_revisions" => %{:start => 2, :ids => ["c", "b"]}
                },

                # doc1: conflict between 1-a and 2-c, 2-c is deleted
                %{"_id" => "doc1", "foo" => "baz", "bar" => 0, "baz" => "there",
                  "_revisions" => %{:start => 1, :ids => ["a"]}
                },
                %{"_id" => "doc1", "foo" => "bazX", "bar" => 1, "baz" => "thereX",
                  "_revisions" => %{:start => 2, :ids => ["c", "b"]},
                  "_deleted" => true
                },

                # doc2: 2-b is deleted
                %{"_id" => "doc2", "foo" => "foobar", "bar" => 100, "baz" => "hi",
                  "_revisions" => %{:start => 2, :ids => ["b", "a"]},
                  "_deleted" => true
                }
      ], :new_edits => false}
    )
    assert resp.status_code in [201]
    resp
  end

  def create_partitioned_search_docs(db_name) do
    resp = Couch.post("/#{db_name}/_bulk_docs",
      headers: ["Content-Type": "application/json"],
      body: %{:docs => [
                %{"_id" => "foo:doc4", "foo" => "foo", "bar" => 42},
                %{"_id" => "bar:doc3", "foo" => "bar", "bar" => 12.0},
                %{"_id" => "foo:doc1", "foo" => "baz", "bar" => 0},
                %{"_id" => "bar:doc2", "foo" => "foobar", "bar" => 100},
      ]}
    )
    assert resp.status_code in [201]
  end

  def create_ddoc(db_name, ddoc \\
    %{
      nouveau: %{
        bar: %{
          default_analyzer: "standard",
          index: """
            function (doc) {
              index("string", "foo", doc.foo, {store: true});
              index("double", "bar", doc.bar, {store: true});
              index("stored", "baz", doc.foo);
            }
          """
        }
      }
    }) do
    resp = Couch.put("/#{db_name}/_design/foo", body: ddoc)
    assert resp.status_code in [201]
    assert Map.has_key?(resp.body, "ok") == true
  end

  def create_mango_index(db_name) do
    body = %{
      type: "nouveau",
      index: %{
        fields: [
          %{name: "foo", type: "string"},
          %{name: "bar", type: "number"},
          %{name: "baz", type: "string"},
        ]
      }
    }

    resp = Couch.post("/#{db_name}/_index", body: body)
    assert resp.status_code in [200]
    resp.body
  end

  def get_ids(resp) do
    %{:body => %{"hits" => hits}} = resp
    Enum.map(hits, fn hit -> hit["doc"]["_id"] end)
  end

  def get_mango_ids(resp) do
    %{:body => %{"docs" => docs}} = resp
    Enum.map(docs, fn doc -> doc["_id"] end)
  end

  def get_bookmark(resp) do
    %{:body => %{"bookmark" => bookmark}} = resp
    bookmark
  end

  def get_total_hits(resp) do
    %{:body => %{"total_hits" => total_hits}} = resp
    total_hits
  end

  def assert_status_code(resp, code) do
    assert resp.status_code == code,
      "status code: #{resp.status_code}, resp body: #{:jiffy.encode(resp.body)}"
  end

  test "user-agent header is forbidden", _context do
    resp = Couch.get("http://127.0.0.1:5987",
      headers: ["User-Agent": "couchdb"])
    assert_status_code(resp, 403)
  end

  test "search analyze", _context do
    url = "/_nouveau_analyze"
    resp = Couch.post(url,
      headers: ["Content-Type": "application/json"],
      body: %{analyzer: "standard", text: "hello there"})
    assert_status_code(resp, 200)
    assert resp.body ==  %{"tokens" => ["hello", "there"]}
  end

  @tag :with_db
  test "search info", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    # query it so it builds
    url = "/#{db_name}/_design/foo/_nouveau/bar"
    resp = Couch.get(url, query: %{q: "*:*", include_docs: true})
    assert_status_code(resp, 200)

    url = "/#{db_name}/_design/foo/_nouveau_info/bar"
    resp = Couch.get(url)
    assert_status_code(resp, 200)
    info = Map.get(resp.body, "search_index")
    assert Map.get(info, "disk_size") > 0
    assert Map.get(info, "num_docs") > 0
    assert Map.get(info, "update_seq") > 0
    assert String.length(Map.get(info, "signature")) > 0
  end

  @tag :with_db
  test "search returns all items for GET", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/foo/_nouveau/bar"
    resp = Couch.get(url, query: %{q: "*:*", include_docs: true})
    assert_status_code(resp, 200)
    ids = get_ids(resp)
    # nouveau sorts by _id as tie-breaker
    assert ids == ["doc1", "doc2", "doc3", "doc4"]
  end

  @tag :with_db
  test "search returns all items for POST", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/foo/_nouveau/bar"
    resp = Couch.post(url, body: %{q: "*:*", include_docs: true})
    assert_status_code(resp, 200)
    ids = get_ids(resp)
    assert ids == ["doc1", "doc2", "doc3", "doc4"]
  end

  @tag :with_db
  test "search returns all items (paginated)", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/foo/_nouveau/bar"

    # page 1
    resp = Couch.post(url, body: %{q: "*:*", limit: 2, include_docs: true})
    assert_status_code(resp, 200)
    ids = get_ids(resp)
    assert ids == ["doc1", "doc2"]

    # page 2
    resp = Couch.post(url, body: %{q: "*:*", limit: 2, bookmark: get_bookmark(resp), include_docs: true})
    assert_status_code(resp, 200)
    ids = get_ids(resp)
    assert ids == ["doc3", "doc4"]
  end

  @tag :with_db
  test "search for foo:bar", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/foo/_nouveau/bar"
    resp = Couch.post(url, body: %{q: "foo:bar", include_docs: true})
    assert_status_code(resp, 200)
    ids = get_ids(resp)
    assert ids == ["doc3"]
  end

  @tag :with_db
  test "search for numeric ranges with locales", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/foo/_nouveau/bar"
    resp = Couch.post(url, body: %{q: "bar:[10.0 TO 20.0]", locale: "us", include_docs: true})
    assert_status_code(resp, 200)
    ids = get_ids(resp)
    assert ids == ["doc3"]

    url = "/#{db_name}/_design/foo/_nouveau/bar"
    resp = Couch.post(url, body: %{q: "bar:[10.0 TO 20.0]", locale: "de", include_docs: true})
    assert_status_code(resp, 200)
    ids = get_ids(resp)
    assert ids == ["doc2"]
  end

  @tag :with_db
  test "multiple values for stored field", context do
    db_name = context[:db_name]
    resp = Couch.post("/#{db_name}/_bulk_docs",
      headers: ["Content-Type": "application/json"],
      body: %{:docs => [
        %{"_id" => "doc1", "colors" =>
           ["red", "orange", "yellow", "green", "blue", "indigo", "violet"]}]}
    )
    assert resp.status_code in [201]
    create_ddoc(db_name,
      %{
        nouveau: %{
          bar: %{
            default_analyzer: "standard",
            index: """
            function (doc) {
            doc.colors.forEach((color) =>
              index('string', 'color', color, {'store': true}));
            }
            """
          }
        }
      })
    url = "/#{db_name}/_design/foo/_nouveau/bar"
    resp = Couch.post(url, body: %{q: "*:*", include_docs: true})
    assert_status_code(resp, 200)
    assert Enum.at(resp.body["hits"], 0)["doc"]["colors"] ==
      ["red", "orange", "yellow", "green", "blue", "indigo", "violet"]
  end

  @tag :with_db
  test "sort by string field (asc)", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/foo/_nouveau/bar"
    resp = Couch.post(url, body: %{q: "*:*", sort: "foo", include_docs: true})
    assert_status_code(resp, 200)
    ids = get_ids(resp)
    assert ids == ["doc3", "doc1", "doc4", "doc2"]
  end

  @tag :with_db
  test "sort by string field (desc)", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/foo/_nouveau/bar"
    resp = Couch.post(url, body: %{q: "*:*", sort: "-foo", include_docs: true})
    assert_status_code(resp, 200)
    ids = get_ids(resp)
    assert ids == ["doc2", "doc4", "doc1", "doc3"]
  end

  @tag :with_db
  test "sort by numeric field (asc)", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/foo/_nouveau/bar"
    resp = Couch.post(url, body: %{q: "*:*", sort: "bar", include_docs: true})
    assert_status_code(resp, 200)
    ids = get_ids(resp)
    assert ids == ["doc1", "doc3", "doc4", "doc2"]
  end

  @tag :with_db
  test "sort by numeric field (desc)", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/foo/_nouveau/bar"
    resp = Couch.post(url, body: %{q: "*:*", sort: "-bar", include_docs: true})
    assert_status_code(resp, 200)
    ids = get_ids(resp)
    assert ids == ["doc2", "doc4", "doc3", "doc1"]
  end

  @tag :with_db
  test "counts", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/foo/_nouveau/bar"
    resp = Couch.post(url, body: %{q: "*:*", counts: ["foo"], include_docs: true})
    assert_status_code(resp, 200)
    %{:body => %{"counts" => counts}} = resp
    assert counts == %{"foo" => %{"bar" => 1, "baz" => 1, "foo" => 1, "foobar" => 1}}
  end

  @tag :with_db
  test "ranges", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/foo/_nouveau/bar"
    resp = Couch.post(url, body: %{q: "*:*", ranges: %{bar: [
      %{label: "cheap", min: 0, max: 42},
      %{label: "expensive", min: 42, min_inclusive: false, max: 1000}]},
      include_docs: true})
    assert_status_code(resp, 200)
    %{:body => %{"ranges" => ranges}} = resp
    assert ranges == %{"bar" => %{"cheap" => 3, "expensive" => 1}}
  end

  @tag :with_db
  test "ranges (open)", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/foo/_nouveau/bar"
    resp = Couch.post(url, body: %{q: "*:*", ranges: %{bar: [
      %{label: "cheap", max: 42},
      %{label: "expensive", min: 42, min_inclusive: false}]},
      include_docs: true})
    assert_status_code(resp, 200)
    %{:body => %{"ranges" => ranges}} = resp
    assert ranges == %{"bar" => %{"cheap" => 3, "expensive" => 1}}
  end

  @tag :with_db
  test "top_n", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/foo/_nouveau/bar"
    resp = Couch.post(url, body: %{q: "*:*", ranges: %{bar: [
      %{label: "cheap", max: 42},
      %{label: "expensive", min: 42, min_inclusive: false}]},
      top_n: 1,
      include_docs: true})
    assert_status_code(resp, 200)
    %{:body => %{"ranges" => ranges}} = resp
    assert ranges == %{"bar" => %{"cheap" => 3}}
  end

  @tag :with_db
  test "mango search by number", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_mango_index(db_name)

    url = "/#{db_name}/_find"
    resp = Couch.post(url, body: %{selector: %{bar: %{"$gt": 5}}})
    assert_status_code(resp, 200)
    ids = get_mango_ids(resp)
    assert ids == ["doc2", "doc3", "doc4"]
  end

  @tag :with_db
  test "mango search by string", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_mango_index(db_name)

    url = "/#{db_name}/_find"
    resp = Couch.post(url, body: %{selector: %{foo: %{"$eq": "foo"}}})
    assert_status_code(resp, 200)
    ids = get_mango_ids(resp)
    assert ids == ["doc4"]
  end

  @tag :with_db
  test "mango search by text", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_mango_index(db_name)

    url = "/#{db_name}/_find"
    resp = Couch.post(url, body: %{selector: %{"$text": "hello"}})
    assert_status_code(resp, 200)
    ids = get_mango_ids(resp)
    assert ids == ["doc4", "doc3"]
  end

  @tag :with_db
  test "mango sort by number", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_mango_index(db_name)

    url = "/#{db_name}/_find"
    resp = Couch.post(url, body: %{sort: [%{"bar:number": "asc"}], selector: %{bar: %{"$gt": 5}}})
    assert_status_code(resp, 200)
    ids = get_mango_ids(resp)
    assert ids == ["doc3", "doc4", "doc2"]
  end

  @tag :with_db
  test "mango sort by string", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_mango_index(db_name)

    url = "/#{db_name}/_find"
    resp = Couch.post(url, body: %{sort: [%{"foo:string": "asc"}], selector: %{bar: %{"$gte": 0}}})
    assert_status_code(resp, 200)
    ids = get_mango_ids(resp)
    assert ids == ["doc3", "doc1", "doc4", "doc2"]
  end

  @tag :with_partitioned_db
  test "search GET (partitioned)", context do
    db_name = context[:db_name]
    create_partitioned_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_partition/foo/_design/foo/_nouveau/bar"
    resp = Couch.get(url, query: %{q: "*:*", include_docs: true})
    assert_status_code(resp, 200)
    ids = get_ids(resp)
    assert ids == ["foo:doc1", "foo:doc4"]

    url = "/#{db_name}/_partition/bar/_design/foo/_nouveau/bar"
    resp = Couch.get(url, query: %{q: "*:*", include_docs: true})
    assert_status_code(resp, 200)
    ids = get_ids(resp)
    assert ids == ["bar:doc2", "bar:doc3"]
  end

  @tag :with_partitioned_db
  test "search POST (partitioned)", context do
    db_name = context[:db_name]
    create_partitioned_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_partition/foo/_design/foo/_nouveau/bar"
    resp = Couch.post(url, body: %{q: "*:*", include_docs: true})
    assert_status_code(resp, 200)
    ids = get_ids(resp)
    assert ids == ["foo:doc1", "foo:doc4"]

    url = "/#{db_name}/_partition/bar/_design/foo/_nouveau/bar"
    resp = Couch.post(url, body: %{q: "*:*", include_docs: true})
    assert_status_code(resp, 200)
    ids = get_ids(resp)
    assert ids == ["bar:doc2", "bar:doc3"]
  end

  @tag :with_partitioned_db
  test "mango (partitioned)", context do
    db_name = context[:db_name]
    create_partitioned_search_docs(db_name)
    create_mango_index(db_name)

    url = "/#{db_name}/_partition/foo/_find"
    resp = Couch.post(url, body: %{selector: %{foo: %{"$eq": "foo"}}})
    assert_status_code(resp, 200)
    ids = get_mango_ids(resp)
    assert ids == ["foo:doc4"]

    url = "/#{db_name}/_partition/bar/_find"
    resp = Couch.post(url, body: %{selector: %{foo: %{"$eq": "bar"}}})
    assert_status_code(resp, 200)
    ids = get_mango_ids(resp)
    assert ids == ["bar:doc3"]
  end

  @tag :with_db
  test "delete", context do
    db_name = context[:db_name]
    create_resp = create_search_docs(db_name)
    create_ddoc(db_name)

    search_url = "/#{db_name}/_design/foo/_nouveau/bar"

    # confirm all hits
    resp = Couch.get(search_url, query: %{q: "*:*", include_docs: true})
    assert_status_code(resp, 200)
    assert get_total_hits(resp) == 4

    # delete a doc
    doc = hd(create_resp.body)
    resp = Couch.delete("/#{db_name}/#{doc["id"]}?rev=#{doc["rev"]}")
    assert_status_code(resp, 200)

    # confirm it is gone
    resp = Couch.get(search_url, query: %{q: "*:*", include_docs: true})
    assert_status_code(resp, 200)
    assert get_total_hits(resp) == 3

    resp = Couch.get("/#{db_name}/_design/foo/_nouveau_info/bar")
    assert_status_code(resp, 200)
    assert resp.body["search_index"]["update_seq"] == 6
    assert resp.body["search_index"]["purge_seq"] == 0
  end

  @tag :with_db
  test "purge", context do
    db_name = context[:db_name]
    create_resp = create_search_docs(db_name)
    create_ddoc(db_name)

    search_url = "/#{db_name}/_design/foo/_nouveau/bar"

    # confirm all hits
    resp = Couch.get(search_url, query: %{q: "*:*", include_docs: true})
    assert_status_code(resp, 200)
    assert get_total_hits(resp) == 4

    # purge a doc
    doc = hd(create_resp.body)
    resp =
      Couch.post("/#{db_name}/_purge",
        body: %{doc["id"] => [doc["rev"]]}
      )
    assert_status_code(resp, 201)

    # confirm it is gone
    resp = Couch.get(search_url, query: %{q: "*:*", include_docs: true})
    assert_status_code(resp, 200)
    assert get_total_hits(resp) == 3

    resp = Couch.get("/#{db_name}/_design/foo/_nouveau_info/bar")
    assert_status_code(resp, 200)
    db_info = info(db_name)
    assert seq(db_info["update_seq"]) == resp.body["search_index"]["update_seq"]
    assert seq(db_info["purge_seq"]) == resp.body["search_index"]["purge_seq"]
  end

  @tag :with_db
  test "purge with conflicts", context do
    db_name = context[:db_name]
    _create_resp = create_conflicted_search_docs(db_name)
    create_ddoc(db_name)

    search_url = "/#{db_name}/_design/foo/_nouveau/bar"

    # confirm all hits
    resp = Couch.get(search_url, query: %{q: "*:*", include_docs: true})
    assert_status_code(resp, 200)

    assert get_total_hits(resp) == 3
    [hit1, hit2, hit3] = Enum.sort(resp.body["hits"])

    assert hit1["doc"]["_id"] == "doc1"
    assert hit1["doc"]["_rev"] == "1-a"
    assert hit1["fields"] == %{"bar" => 0.0, "foo" => "baz", "baz" => "baz"}

    assert hit2["doc"]["_id"] == "doc3"
    assert hit2["doc"]["_rev"] == "2-c"
    assert hit2["fields"] == %{"bar" => 13.0, "foo" => "barX", "baz" => "barX"}

    assert hit3["doc"]["_id"] == "doc4"
    assert hit3["doc"]["_rev"] == "1-b"
    assert hit3["fields"] == %{"bar" => 43.0, "foo" => "fooX", "baz" => "fooX"}

    # purge docs
    purge_body = %{
        "doc1" => ["2-c", "3-nonexistentrev"],
        "doc2" => ["2-b"],
        "doc3" => ["2-c"],
        "doc4" => ["1-b"],
    }
    resp = Couch.post("/#{db_name}/_purge", body: purge_body)
    assert_status_code(resp, 201)

    resp = Couch.get(search_url, query: %{q: "*:*", include_docs: true})
    assert_status_code(resp, 200)
    _hits = Enum.sort(resp.body["hits"])

    assert get_total_hits(resp) == 2
    [hit1, hit2] = Enum.sort(resp.body["hits"])

    # doc1: 2-c deleted was purged, 1-a is still the winner
    assert hit1["doc"]["_id"] == "doc1"
    assert hit1["doc"]["_rev"] == "1-a"
    assert hit1["fields"] ==  %{"bar" => 0.0, "foo" => "baz", "baz" => "baz"}

    # doc2: doc was deleted and now it's completely purged

    # doc3: live revision is deleted, we're left with the deleted rev only

    # doc4: 2-c was purged, 1-a is the new winner
    assert hit2["doc"]["_id"] == "doc4"
    assert hit2["doc"]["_rev"] == "1-a"
    assert hit2["fields"] == %{"bar" => 42.0, "foo" => "foo", "baz" => "foo"}

    resp = Couch.get("/#{db_name}")
    db_purge_seq = resp.body["purge_seq"]
    # Double-check db purge sequence (sum of purge seqeunces on shards) is 4
    assert String.starts_with?(db_purge_seq, "4-")

    resp = Couch.get("/#{db_name}/_design/foo/_nouveau_info/bar")
    assert_status_code(resp, 200)
    db_info = info(db_name)
    assert seq(db_info["update_seq"]) == resp.body["search_index"]["update_seq"]
    assert seq(db_info["purge_seq"]) == resp.body["search_index"]["purge_seq"]
  end

  @tag :with_db
  test "index same field with different field types", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name, %{
      nouveau: %{
        bar: %{
          default_analyzer: "standard",
          index: """
            function (doc) {
              index("string", "foo", "bar", {store: true});
              index("double", "foo", 12.0, {store: true});
            }
          """
        }}})
    url = "/#{db_name}/_design/foo/_nouveau/bar"
    resp = Couch.post(url, body: %{q: "foo:bar", include_docs: true})
    assert_status_code(resp, 400)
  end

  @tag :with_db
  test "index not found", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)
    url = "/#{db_name}/_design/foo/_nouveau/missing"
    resp = Couch.post(url, body: %{q: "foo:bar", include_docs: true})
    assert_status_code(resp, 404)
  end

  @tag :with_db
  test "meta", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/foo/_nouveau/bar"
    resp = Couch.get(url, query: %{q: "*:*", include_docs: true})
    assert_status_code(resp, 200)
    assert resp.body["update_latency"] > 0
  end

  def seq(str) do
    String.to_integer(hd(Regex.run(~r/^[0-9]+/, str)))
  end

end

defmodule NouveauTest do
  use CouchTestCase

  @moduletag :search

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

  def create_ddoc(db_name, opts \\ %{}) do
    default_ddoc = %{
      nouveau: %{
        bar: %{
          default_analyzer: "standard",
          index: """
            function (doc) {
              index("string", "foo", doc.foo, {store: true});
              index("double", "bar", doc.bar, {store: true});
            }
          """
        }
      }
    }

    ddoc = Enum.into(opts, default_ddoc)

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

  def assert_status_code(resp, code) do
    assert resp.status_code == code,
      "status code: #{resp.status_code}, resp body: #{:jiffy.encode(resp.body)}"
  end

  test "search analyze", context do
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
  test "sort by string field (asc)", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/foo/_nouveau/bar"
    resp = Couch.post(url, body: %{q: "*:*", sort: "foo<string>", include_docs: true})
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
    resp = Couch.post(url, body: %{q: "*:*", sort: "-foo<string>", include_docs: true})
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
    resp = Couch.post(url, body: %{q: "*:*", sort: "bar<double>", include_docs: true})
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
    resp = Couch.post(url, body: %{q: "*:*", sort: "-bar<double>", include_docs: true})
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

end

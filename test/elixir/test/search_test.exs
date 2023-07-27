defmodule SearchTest do
  use CouchTestCase

  @moduletag :search

  @moduledoc """
  Test search
  """

  def create_search_docs(db_name) do
    resp = Couch.post("/#{db_name}/_bulk_docs",
      headers: ["Content-Type": "application/json"],
      body: %{:docs => [
                %{"item" => "apple",  "place" => "kitchen", "state" => "new",     "price" => 0.99},
                %{"item" => "banana", "place" => "kitchen", "state" => "new",     "price" => 1.49},
                %{"item" => "carrot", "place" => "kitchen", "state" => "old",     "price" => 0.75},
                %{"item" => "date",   "place" => "lobby",   "state" => "unknown", "price" => 1.25},
      ]}
    )
    assert resp.status_code in [201, 202]
  end

  def create_ddoc(db_name, opts \\ %{}) do
    default_ddoc = %{
      indexes: %{
        fruits: %{
          analyzer: %{name: "standard"},
          index: ~S"""
            function (doc) {
              index("item", doc.item, {facet: true});
              index("place", doc.place, {facet: true});
              index("state", doc.state, {facet: true});
              index("price", doc.price, {facet: true});
            }
            """
        }
      }
    }

    ddoc = Enum.into(opts, default_ddoc)

    resp = Couch.put("/#{db_name}/_design/inventory", body: ddoc)
    assert resp.status_code in [201, 202]
    assert Map.has_key?(resp.body, "ok") == true
  end

  def create_invalid_ddoc(db_name, opts \\ %{}) do
    invalid_ddoc = %{
      :indexes => [
        %{"name" => "foo",  "ddoc" => "bar", "type" => "text"},
      ]
    }

    ddoc = Enum.into(opts, invalid_ddoc)

    resp = Couch.put("/#{db_name}/_design/search", body: ddoc)
    assert resp.status_code in [201, 202]
    assert Map.has_key?(resp.body, "ok") == true
  end

  def get_items (resp) do
    %{:body => %{"rows" => rows}} = resp
    Enum.map(rows, fn row -> row["doc"]["item"] end)
  end

  @tag :with_db
  test "search returns all items for GET", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/inventory/_search/fruits"
    resp = Couch.get(url, query: %{q: "*:*", include_docs: true})
    assert resp.status_code == 200
    ids = get_items(resp)
    assert Enum.sort(ids) == Enum.sort(["apple", "banana", "carrot", "date"])
  end

  @tag :with_db
  test "drilldown single key single value for GET", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/inventory/_search/fruits"
    resp = Couch.get(url, query: %{q: "*:*", drilldown: :jiffy.encode(["place", "kitchen"]), include_docs: true})
    assert resp.status_code == 200
    ids = get_items(resp)
    assert Enum.sort(ids) == Enum.sort(["apple", "banana", "carrot"])
  end

  @tag :with_db
  test "drilldown single key multiple values for GET", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/inventory/_search/fruits"
    resp = Couch.get(url, query: %{q: "*:*", drilldown: :jiffy.encode(["state", "new", "unknown"]), include_docs: true})
    assert resp.status_code == 200
    ids = get_items(resp)
    assert Enum.sort(ids) == Enum.sort(["apple", "banana", "date"])
  end

  @tag :with_db
  test "drilldown multiple keys single values for GET", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/inventory/_search/fruits"
    resp = Couch.get(url, query: %{q: "*:*", drilldown: :jiffy.encode([["state", "old"], ["item", "apple"]]), include_docs: true})
    assert resp.status_code == 200
    ids = get_items(resp)
    assert Enum.sort(ids) == []
  end

  @tag :with_db
  test "drilldown multiple query definitions for GET", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/inventory/_search/fruits?q=*:*&drilldown=[\"state\",\"old\"]&drilldown=[\"item\",\"apple\"]&include_docs=true"
    resp = Couch.get(url)
    assert resp.status_code == 200
    ids = get_items(resp)
    assert Enum.sort(ids) == []
  end


  @tag :with_db
  test "search returns all items for POST", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/inventory/_search/fruits"
    resp = Couch.post(url, body: %{q: "*:*", include_docs: true})
    assert resp.status_code == 200
    ids = get_items(resp)
    assert Enum.sort(ids) == Enum.sort(["apple", "banana", "carrot", "date"])
  end

  @tag :with_db
  test "drilldown single key single value for POST", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/inventory/_search/fruits"
    resp = Couch.post(url, body: %{query: "*:*", drilldown: ["place", "kitchen"], include_docs: true})
    assert resp.status_code == 200
    ids = get_items(resp)
    assert Enum.sort(ids) == Enum.sort(["apple", "banana", "carrot"])
  end

  @tag :with_db
  test "drilldown single key multiple values for POST", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/inventory/_search/fruits"
    resp = Couch.post(url, body: %{query: "*:*", drilldown: ["state", "new", "unknown"], include_docs: true})
    assert resp.status_code == 200
    ids = get_items(resp)
    assert Enum.sort(ids) == Enum.sort(["apple", "banana", "date"])
  end

  @tag :with_db
  test "drilldown multiple keys single values for POST", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/inventory/_search/fruits"
    resp = Couch.post(url, body: %{q: "*:*", drilldown: [["state", "old"], ["item", "apple"]], include_docs: true})
    assert resp.status_code == 200
    ids = get_items(resp)
    assert Enum.sort(ids) == []
  end

  @tag :with_db
  test "drilldown three keys single values for POST", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/inventory/_search/fruits"
    resp = Couch.post(url, body: %{q: "*:*", drilldown: [["place", "kitchen"], ["state", "new"], ["item", "apple"]], include_docs: true})
    assert resp.status_code == 200
    ids = get_items(resp)
    assert Enum.sort(ids) == ["apple"]
  end

  @tag :with_db
  test "drilldown multiple keys multiple values for POST", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/inventory/_search/fruits"
    resp = Couch.post(url, body: %{q: "*:*", drilldown: [["state", "old", "new"], ["item", "apple"]], include_docs: true})
    assert resp.status_code == 200
    ids = get_items(resp)
    assert Enum.sort(ids) == ["apple"]
  end

  @tag :with_db
  test "drilldown multiple query definitions for POST", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/inventory/_search/fruits"
    resp = Couch.post(url, body: "{\"include_docs\": true, \"q\": \"*:*\", \"drilldown\": [\"state\", \"old\"], \"drilldown\": [\"item\", \"apple\"]}")
    assert resp.status_code == 200
    ids = get_items(resp)
    assert Enum.sort(ids) == ["apple"]
  end

  @tag :with_db
  test "clean up search index with invalid design document", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)
    create_invalid_ddoc(db_name)

    resp = Couch.post("/#{db_name}/_search_cleanup")
    assert resp.status_code in [201, 202]
  end

  @tag :with_db
  test "facet counts, non-empty", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/inventory/_search/fruits"
    counts = ["place"]
    resp = Couch.get(url, query: %{q: "*:*", limit: 0, counts: :jiffy.encode(counts)})
    assert resp.status_code == 200

    %{:body => %{"counts" => counts}} = resp
    assert counts == %{"place" => %{"kitchen" => 3, "lobby" => 1}}
  end

  @tag :with_db
  test "facet counts, empty", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/inventory/_search/fruits"
    counts = ["place"]
    resp = Couch.get(url, query: %{q: "item:tomato", limit: 0, counts: :jiffy.encode(counts)})
    assert resp.status_code == 200

    %{:body => %{"counts" => counts}} = resp
    assert counts == %{"place" => %{}}
  end

  @tag :with_db
  test "facet ranges, non-empty", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/inventory/_search/fruits"
    ranges = %{"price" => %{"cheap" => "[0 TO 0.99]", "expensive" => "[1.00 TO Infinity]"}}
    resp = Couch.get(url, query: %{q: "*:*", limit: 0, ranges: :jiffy.encode(ranges)})
    assert resp.status_code == 200

    %{:body => %{"ranges" => ranges}} = resp
    assert ranges == %{"price" => %{"cheap" => 2, "expensive" => 2}}
  end

  @tag :with_db
  test "facet ranges, empty", context do
    db_name = context[:db_name]
    create_search_docs(db_name)
    create_ddoc(db_name)

    url = "/#{db_name}/_design/inventory/_search/fruits"
    ranges = %{"price" => %{}}
    resp = Couch.get(url, query: %{q: "*:*", limit: 0, ranges: :jiffy.encode(ranges)})
    assert resp.status_code == 200

    %{:body => %{"ranges" => ranges}} = resp
    assert ranges == %{"price" => %{}}
  end
end

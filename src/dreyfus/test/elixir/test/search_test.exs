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
                %{"item" => "apple",  "place" => "kitchen", "state" => "new"},
                %{"item" => "banana", "place" => "kitchen", "state" => "new"},
                %{"item" => "carrot", "place" => "kitchen", "state" => "old"},
                %{"item" => "date",   "place" => "lobby",   "state" => "unknown"},
      ]}
    )
    assert resp.status_code in [201, 202]
  end

  def create_ddoc(db_name, opts \\ %{}) do
    default_ddoc = %{
      indexes: %{
        fruits: %{
          analyzer: %{name: "standard"},
          index: "function (doc) {\n  index(\"item\", doc.item, {facet: true});\n  index(\"place\", doc.place, {facet: true});\n  index(\"state\", doc.state, {facet: true});\n}"
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
end

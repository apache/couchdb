defmodule IndexInfoTest do
  use CouchTestCase

  @moduletag :index_info

  @moduledoc """
  Test GET and POST /{db}/_index_info
  """

  @pending_keys ["copies", "copies_expected", "maximum", "minimum", "preferred", "total"]
  # expected keys in every response
  @keys ["bookmark", "nouveau_indexes", "search_indexes", "total_rows", "view_indexes"]

  # A design doc with a view group, a lib, a search index and a nouveau
  # index.
  @cookbook %{
    _id: "_design/cookbook",
    language: "javascript",
    autoupdate: false,
    views: %{
      by_integer: %{map: "function(doc) { if (doc.integer) emit(doc.integer, 1); }"},
      by_string: %{
        map: "function(doc) { if (doc.string) emit(doc.string, 1); }",
        reduce: "_count"
      },
      lib: %{helper: "exports.x = 1;"}
    },
    indexes: %{
      strings: %{index: "function(doc) { if (doc.string) index('string', doc.string); }"}
    },
    nouveau: %{
      strings: %{
        index: "function(doc) { if (doc.string) index('string', 'string', doc.string); }"
      }
    }
  }

  # Make some indexes unavailable
  @unavailable [
    %{section: "nouveau", key: "enable", value: "false"},
    %{section: "dreyfus", key: "name", value: "clouseau_none@127.0.0.1"}
  ]

  defp index_info(db_name, query \\ []) do
    resp = Couch.get("/#{db_name}/_index_info", query: query)

    assert resp.status_code == 200,
           "Expected 200, got #{resp.status_code}: #{inspect(resp.body)}"

    resp.body
  end

  defp view_group_ddocs(body), do: Enum.map(body["view_indexes"], & &1["ddoc"])

  # how many view, search and nouveau entries we got
  defp counts(body) do
    lists = ["view_indexes", "search_indexes", "nouveau_indexes"]
    Enum.map(lists, &length(body[&1]))
  end

  defp view_group(body, ddoc) do
    [entry] = Enum.filter(body["view_indexes"], &(&1["ddoc"] == ddoc))
    entry
  end

  defp ddoc_with_view(id) do
    %{_id: id, views: %{all: %{map: "function(doc) { emit(doc._id, null); }"}}}
  end

  defp assert_meta_matches_design_docs(db_name, query \\ []) do
    resp = Couch.get("/#{db_name}/_design_docs", query: query)
    assert resp.status_code == 200
    body = index_info(db_name, query)
    assert body["total_rows"] == resp.body["total_rows"]
    body
  end

  defp create_paging_ddocs(db_name) do
    for id <- ["_design/a", "_design/b", "_design/c"] do
      {:ok, _} = create_doc(db_name, ddoc_with_view(id))
    end

    {:ok, _} =
      create_doc(db_name, %{
        _id: "_design/b2",
        filters: %{all: "function(doc) { return true; }"}
      })
  end

  @tag :with_db
  test "index info lists every index with its per index info object", context do
    db_name = context[:db_name]
    {:ok, _} = create_doc(db_name, @cookbook)
    {:ok, _} = create_doc(db_name, ddoc_with_view("_design/zzz"))
    bulk_save(db_name, make_docs(1..20))
    copies = shard_copies(db_name)

    # These are not available, we want to test that scenario
    run_on_modified_server(@unavailable, fn ->
      body = index_info(db_name)
      assert Enum.sort(Map.keys(body)) == @keys
      # two design docs, all returned, no next page => bookmark is null
      assert body["total_rows"] == 2
      assert body["bookmark"] == nil
      assert_meta_matches_design_docs(db_name)

      # one view group entry per design doc sorted by ddoc id
      assert view_group_ddocs(body) == ["_design/cookbook", "_design/zzz"]
      cookbook = view_group(body, "_design/cookbook")
      assert Enum.sort(Map.keys(cookbook)) == ["ddoc", "info", "ok", "views"]
      assert cookbook["ok"] == true
      # the lib is not a view
      assert cookbook["views"] == ["by_integer", "by_string"]

      pending = cookbook["info"]["updates_pending"]
      assert Enum.sort(Map.keys(pending)) == @pending_keys
      assert pending["copies"] == copies
      assert pending["copies_expected"] == copies
      # autoupdate = false this should not have built yet
      assert pending["minimum"] > 0

      [search] = body["search_indexes"]
      assert Enum.sort(Map.keys(search)) == ["ddoc", "error", "name", "ok", "reason"]
      assert search["ddoc"] == "_design/cookbook"
      assert search["name"] == "strings"
      assert search["ok"] == false
      assert search["error"] == "service unavailable"
      assert search["reason"] == "Search is not available"

      [nouveau] = body["nouveau_indexes"]
      assert Enum.sort(Map.keys(nouveau)) == ["ddoc", "error", "name", "ok", "reason"]
      assert nouveau["ddoc"] == "_design/cookbook"
      assert nouveau["name"] == "strings"
      assert nouveau["ok"] == false
      assert nouveau["error"] == "service unavailable"
      assert nouveau["reason"] == "nouveau is not enabled"

      # wehn built, info is exactly _info produces
      resp = Couch.get("/#{db_name}/_design/cookbook/_view/by_integer")
      assert resp.status_code == 200

      retry_until(fn ->
        info = view_group(index_info(db_name), "_design/cookbook")["info"]
        resp = Couch.get("/#{db_name}/_design/cookbook/_info")
        assert resp.status_code == 200
        info["updates_pending"]["maximum"] == 0 and info == resp.body["view_index"]
      end)
    end)
  end

  @tag :with_db
  test "index info reports mango indexes by the ddoc and name _index uses", context do
    db_name = context[:db_name]

    resp =
      Couch.post("/#{db_name}/_index",
        body: %{index: %{fields: ["integer"]}, name: "by-integer", type: "json"}
      )

    assert resp.status_code == 200
    assert resp.body["result"] == "created"
    bulk_save(db_name, make_docs(1..5))

    resp = Couch.get("/#{db_name}/_index")
    assert resp.status_code == 200
    [mango] = for idx <- resp.body["indexes"], idx["type"] == "json", do: idx

    body = index_info(db_name)
    assert body["search_indexes"] == []
    assert body["nouveau_indexes"] == []
    entry = view_group(body, mango["ddoc"])
    assert entry["ok"] == true
    assert entry["views"] == [mango["name"]]
    assert entry["info"]["language"] == "query"
  end

  @tag :with_db
  test "index info pages design docs like _design_docs", context do
    db_name = context[:db_name]
    create_paging_ddocs(db_name)

    assert view_group_ddocs(index_info(db_name)) == [
             "_design/a",
             "_design/b",
             "_design/c"
           ]

    assert view_group_ddocs(index_info(db_name, limit: 1)) == ["_design/a"]
    assert view_group_ddocs(index_info(db_name, limit: 3)) == ["_design/a", "_design/b"]
    assert view_group_ddocs(index_info(db_name, skip: 1, limit: 1)) == ["_design/b"]

    b = :jiffy.encode("_design/b")

    assert view_group_ddocs(index_info(db_name, startkey: b)) == [
             "_design/b",
             "_design/c"
           ]

    assert view_group_ddocs(index_info(db_name, start_key: b)) == [
             "_design/b",
             "_design/c"
           ]

    # continuing from the last ddoc seen, as with _all_docs
    assert view_group_ddocs(index_info(db_name, startkey: b, skip: 1, limit: 1)) == []
    assert view_group_ddocs(index_info(db_name, startkey: b, skip: 2)) == ["_design/c"]
    assert view_group_ddocs(index_info(db_name, endkey: b)) == ["_design/a", "_design/b"]
    assert view_group_ddocs(index_info(db_name, end_key: b)) == ["_design/a", "_design/b"]

    assert view_group_ddocs(index_info(db_name, endkey: b, inclusive_end: false)) == [
             "_design/a"
           ]

    assert view_group_ddocs(index_info(db_name, descending: true)) ==
             ["_design/c", "_design/b", "_design/a"]

    # the second design doc in reverse order is _design/b2, which has no indexes
    assert view_group_ddocs(index_info(db_name, descending: true, limit: 2)) == [
             "_design/c"
           ]

    assert view_group_ddocs(index_info(db_name, descending: true, limit: 3)) ==
             ["_design/c", "_design/b"]

    assert view_group_ddocs(index_info(db_name, descending: true, startkey: b)) ==
             ["_design/b", "_design/a"]

    # total_rows counts design docs, not index entries, and is the number
    # _design_docs reports for the same parameters
    body = assert_meta_matches_design_docs(db_name)
    assert body["total_rows"] == 4
    assert length(body["view_indexes"]) == 3

    for query <- [[limit: 1], [skip: 1, limit: 1], [startkey: b], [descending: true]] do
      assert_meta_matches_design_docs(db_name, query)
    end

    # bookmark is the design doc after the page. Even if we have an empty ddoc
    # with no indexes we still get its ID as the bookmark so we can continue
    # paginating.
    page1 = index_info(db_name, limit: 2)
    assert view_group_ddocs(page1) == ["_design/a", "_design/b"]
    assert page1["bookmark"] == "_design/b2"
    page2 = index_info(db_name, limit: 2, startkey: :jiffy.encode(page1["bookmark"]))
    assert view_group_ddocs(page2) == ["_design/c"]
    assert page2["bookmark"] == nil

    # the documented paging loop: pass each bookmark back as the startkey
    # until it is null
    seen =
      Enum.reduce_while(Stream.cycle([nil]), {[], nil}, fn _, {acc, bookmark} ->
        query =
          if bookmark, do: [limit: 1, startkey: :jiffy.encode(bookmark)], else: [limit: 1]

        page = index_info(db_name, query)
        acc = acc ++ view_group_ddocs(page)

        case page["bookmark"] do
          nil -> {:halt, acc}
          next -> {:cont, {acc, next}}
        end
      end)

    assert seen == ["_design/a", "_design/b", "_design/c"]

    # limit 0 bookmarks the first design doc, and the bookmark follows
    # descending order too
    assert index_info(db_name, limit: 0)["bookmark"] == "_design/a"
    desc = index_info(db_name, descending: true, limit: 2)
    assert view_group_ddocs(desc) == ["_design/c"]
    assert desc["bookmark"] == "_design/b"

    desc_startkey = index_info(db_name, descending: true, limit: 2, startkey: b)
    assert view_group_ddocs(desc_startkey) == ["_design/b", "_design/a"]

    # all three lists are always present, only the requested types are gathered
    body = index_info(db_name, type: "view")
    assert Enum.sort(Map.keys(body)) == @keys
    assert length(body["view_indexes"]) == 3
    assert body["search_indexes"] == []
    assert body["nouveau_indexes"] == []
    body = index_info(db_name, type: "search,nouveau")
    assert Enum.sort(Map.keys(body)) == @keys
    assert body["view_indexes"] == []

    resp = Couch.get("/#{db_name}/_index_info", query: [type: "foo"])
    assert resp.status_code == 400
    assert resp.body["error"] == "query_parse_error"

    resp = Couch.get("/#{db_name}/_index_info", query: [limit: "abc"])
    assert resp.status_code == 400
    assert resp.body["error"] == "query_parse_error"

    assert Couch.put("/#{db_name}/_index_info", body: %{}).status_code == 405
    assert Couch.get("/#{db_name}/_index_info/extra").status_code == 404
    assert Couch.get("/#{db_name}_missing/_index_info").status_code == 404
  end

  @tag :with_db
  test "index info takes keys like _design_docs and caps the design docs", context do
    db_name = context[:db_name]
    create_paging_ddocs(db_name)

    resp =
      Couch.post("/#{db_name}/_index_info",
        body: %{keys: ["_design/c", "_design/a", "_design/nope"]}
      )

    assert resp.status_code == 200
    assert Enum.sort(Map.keys(resp.body)) == @keys
    assert view_group_ddocs(resp.body) == ["_design/a", "_design/c"]
    assert resp.body["total_rows"] == 4
    assert resp.body["bookmark"] == nil

    keys = %{keys: ["_design/c", "_design/a", "_design/nope"]}
    ref = Couch.post("/#{db_name}/_design_docs", body: keys)
    assert ref.status_code == 200
    assert resp.body["total_rows"] == ref.body["total_rows"]

    # keys and a startkey is a 400 error
    query = [startkey: :jiffy.encode("_design/a")]
    body = %{keys: ["_design/b"]}
    resp = Couch.post("/#{db_name}/_index_info", query: query, body: body)
    ref = Couch.post("/#{db_name}/_design_docs", query: query, body: body)
    assert resp.status_code == 400
    assert {resp.status_code, resp.body} == {ref.status_code, ref.body}

    # the query parameters apply to POST too
    resp =
      Couch.post("/#{db_name}/_index_info",
        query: [type: "search"],
        body: %{keys: ["_design/a"]}
      )

    assert resp.status_code == 200
    assert resp.body["view_indexes"] == []
    assert resp.body["search_indexes"] == []
    assert resp.body["nouveau_indexes"] == []
    assert resp.body["total_rows"] == 4

    # and keys work as a query parameter, as for _design_docs
    assert view_group_ddocs(index_info(db_name, keys: :jiffy.encode(["_design/b"]))) == [
             "_design/b"
           ]

    resp = Couch.post("/#{db_name}/_index_info", body: %{keys: ["_design/b"]})
    assert resp.status_code == 200
    assert view_group_ddocs(resp.body) == ["_design/b"]
    assert resp.body["bookmark"] == nil

    # other _design_docs params are ok, too
    resp = Couch.post("/#{db_name}/_index_info", body: %{})
    assert resp.status_code == 200
    assert resp.body == index_info(db_name)
    resp = Couch.post("/#{db_name}/_index_info", body: %{limit: 1, descending: true})
    assert resp.status_code == 200
    assert resp.body == index_info(db_name, limit: 1, descending: true)

    resp = Couch.post("/#{db_name}/_index_info", body: %{keys: "_design/a"})
    assert resp.status_code == 400
    assert resp.body["reason"] == "`keys` member must be an array."

    # a request may cover at most max_ddoc_number_for_index_info_req design
    # docs, ones without indexes included; page or send fewer keys
    cap = [%{section: "chttpd", key: "max_ddoc_number_for_index_info_req", value: "2"}]

    run_on_modified_server(cap, fn ->
      resp = Couch.get("/#{db_name}/_index_info")
      assert resp.status_code == 400
      assert resp.body["error"] == "bad_request"
      assert resp.body["reason"] == "too_many_design_docs"

      page = index_info(db_name, limit: 2)
      assert view_group_ddocs(page) == ["_design/a", "_design/b"]
      assert page["bookmark"] == "_design/b2"
      next = :jiffy.encode(page["bookmark"])

      assert view_group_ddocs(index_info(db_name, limit: 2, startkey: next)) == [
               "_design/c"
             ]

      resp =
        Couch.post("/#{db_name}/_index_info",
          body: %{keys: ["_design/a", "_design/b", "_design/c"]}
        )

      assert resp.status_code == 400
      assert resp.body["reason"] == "too_many_keys"

      resp =
        Couch.post("/#{db_name}/_index_info", body: %{keys: ["_design/a", "_design/c"]})

      assert resp.status_code == 200
      assert view_group_ddocs(resp.body) == ["_design/a", "_design/c"]
    end)
  end

  @tag :with_db
  test "index info type in the body or query params", context do
    db_name = context[:db_name]
    {:ok, _} = create_doc(db_name, @cookbook)
    {:ok, _} = create_doc(db_name, ddoc_with_view("_design/zzz"))

    run_on_modified_server(@unavailable, fn ->
      # 2 views, 1 search and 1 nouveau index
      assert counts(index_info(db_name)) == [2, 1, 1]
      assert counts(index_info(db_name, type: "search")) == [0, 1, 0]
      assert counts(index_info(db_name, type: "view,nouveau")) == [2, 0, 1]

      # for bodies we expect a list
      for {type, expected} <- [
            {["view"], [2, 0, 0]},
            {["nouveau", "view"], [2, 0, 1]},
            {["view", "search", "nouveau", "view"], [2, 1, 1]}
          ] do
        resp = Couch.post("/#{db_name}/_index_info", body: %{type: type})
        assert resp.status_code == 200, "type #{inspect(type)}: #{inspect(resp.body)}"
        assert Enum.sort(Map.keys(resp.body)) == @keys
        assert counts(resp.body) == expected, "type #{inspect(type)}"
      end

      # along with the other parameters in the body
      body = %{type: ["search"], keys: ["_design/cookbook", "_design/zzz"]}
      resp = Couch.post("/#{db_name}/_index_info", body: body)
      assert resp.status_code == 200
      assert counts(resp.body) == [0, 1, 0]
      [search] = resp.body["search_indexes"]
      assert {search["ddoc"], search["name"]} == {"_design/cookbook", "strings"}

      # query wins if we got both
      query = [type: "view", limit: 1]
      body = %{type: ["search"], limit: 2}
      resp = Couch.post("/#{db_name}/_index_info", query: query, body: body)
      assert resp.status_code == 200
      assert counts(resp.body) == [1, 0, 0]
      assert view_group_ddocs(resp.body) == ["_design/cookbook"]
    end)

    invalid = "Invalid type: foo. Must be view, search or nouveau"
    not_a_list = "`type` must be an array"

    for {type, reason} <- [
          {["foo"], invalid},
          {["view", "foo"], invalid},
          {[], "`type` must not be empty"},
          {"search", not_a_list},
          {"view,search", not_a_list},
          {"", not_a_list},
          {1, not_a_list},
          {nil, not_a_list},
          {["view", 1], not_a_list},
          {%{view: true}, not_a_list}
        ] do
      resp = Couch.post("/#{db_name}/_index_info", body: %{type: type})
      assert resp.status_code == 400, "type #{inspect(type)}: #{inspect(resp.body)}"
      assert resp.body["error"] == "query_parse_error"
      assert resp.body["reason"] == reason
    end

    # an invalid type in the body is a 400 error
    body = %{type: ["foo"]}
    resp = Couch.post("/#{db_name}/_index_info", query: [type: "view"], body: body)
    assert resp.status_code == 400
    assert resp.body["reason"] == invalid
  end
end

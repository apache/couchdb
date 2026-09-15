defmodule IndexInfoTest do
  use CouchTestCase

  @moduletag :index_info

  @moduledoc """
  Test GET and POST /{db}/_index_info
  """

  @pending_keys ["copies", "copies_expected", "maximum", "minimum", "preferred", "total"]
  @lists ["nouveau_indexes", "search_indexes", "view_indexes"]
  # every response has the three lists plus the design doc paging numbers
  @keys ["nouveau_indexes", "offset", "search_indexes", "total_rows", "view_indexes"]

  # A design doc with a view group (plus a lib), a search index and a nouveau
  # index. Search and nouveau are not available on the test cluster, so their
  # entries exercise the error case.
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

  defp index_info(db_name, query \\ []) do
    resp = Couch.get("/#{db_name}/_index_info", query: query)
    assert resp.status_code == 200, "Expected 200, got #{resp.status_code}: #{inspect(resp.body)}"
    resp.body
  end

  defp view_group_ddocs(body), do: Enum.map(body["view_indexes"], & &1["ddoc"])

  defp view_group(body, ddoc) do
    [entry] = Enum.filter(body["view_indexes"], &(&1["ddoc"] == ddoc))
    entry
  end

  defp ddoc_with_view(id) do
    %{_id: id, views: %{all: %{map: "function(doc) { emit(doc._id, null); }"}}}
  end

  # total_rows and offset are the ones _design_docs reports for the same
  # parameters, whatever they are: both endpoints select design docs the
  # same way
  defp assert_meta_matches_design_docs(db_name, query \\ []) do
    resp = Couch.get("/#{db_name}/_design_docs", query: query)
    assert resp.status_code == 200
    body = index_info(db_name, query)
    assert body["total_rows"] == resp.body["total_rows"]
    assert body["offset"] == resp.body["offset"]
    body
  end

  # _design/a, _design/b and _design/c with a view each, plus _design/b2
  # without indexes, which still takes a paging slot
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

    # Search and nouveau must not be available so their entries are errors:
    # the test config enables nouveau without running a server, and a
    # clouseau could be reachable on the host
    unavailable = [
      %{section: "nouveau", key: "enable", value: "false"},
      %{section: "dreyfus", key: "name", value: "clouseau_none@127.0.0.1"}
    ]

    run_on_modified_server(unavailable, fn ->
      body = index_info(db_name)
      assert Enum.sort(Map.keys(body)) == @keys
      # two design docs, and the paging numbers _design_docs would report
      assert body["total_rows"] == 2
      assert_meta_matches_design_docs(db_name)

      # one view group entry per design doc with views, sorted by ddoc id
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
      # autoupdate is off and the views were never queried: behind on every copy
      assert pending["minimum"] > 0

      # error entries have the same identity fields and no info
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

      # once built, info is exactly what the design doc's _info returns
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

    assert view_group_ddocs(index_info(db_name)) == ["_design/a", "_design/b", "_design/c"]
    assert view_group_ddocs(index_info(db_name, limit: 1)) == ["_design/a"]
    assert view_group_ddocs(index_info(db_name, limit: 3)) == ["_design/a", "_design/b"]
    assert view_group_ddocs(index_info(db_name, skip: 1, limit: 1)) == ["_design/b"]

    b = :jiffy.encode("_design/b")
    assert view_group_ddocs(index_info(db_name, startkey: b)) == ["_design/b", "_design/c"]
    assert view_group_ddocs(index_info(db_name, start_key: b)) == ["_design/b", "_design/c"]
    # continuing from the last ddoc seen, as with _all_docs
    assert view_group_ddocs(index_info(db_name, startkey: b, skip: 1, limit: 1)) == []
    assert view_group_ddocs(index_info(db_name, startkey: b, skip: 2)) == ["_design/c"]
    assert view_group_ddocs(index_info(db_name, endkey: b)) == ["_design/a", "_design/b"]
    assert view_group_ddocs(index_info(db_name, end_key: b)) == ["_design/a", "_design/b"]
    assert view_group_ddocs(index_info(db_name, endkey: b, inclusive_end: false)) == ["_design/a"]

    assert view_group_ddocs(index_info(db_name, descending: true)) ==
             ["_design/c", "_design/b", "_design/a"]

    # the second design doc in reverse order is _design/b2, which has no indexes
    assert view_group_ddocs(index_info(db_name, descending: true, limit: 2)) == ["_design/c"]

    assert view_group_ddocs(index_info(db_name, descending: true, limit: 3)) ==
             ["_design/c", "_design/b"]

    assert view_group_ddocs(index_info(db_name, descending: true, startkey: b)) ==
             ["_design/b", "_design/a"]

    # total_rows counts design docs, not index entries, and both numbers are
    # the ones _design_docs reports for the same parameters
    body = assert_meta_matches_design_docs(db_name)
    assert body["total_rows"] == 4
    assert length(body["view_indexes"]) == 3

    for query <- [[limit: 1], [skip: 1, limit: 1], [startkey: b], [descending: true]] do
      assert_meta_matches_design_docs(db_name, query)
    end

    # the documented paging loop: page by design doc count, since a design
    # doc without indexes (here _design/b2) contributes no entry and so the
    # last design doc of a page is not always visible in it
    seen =
      Enum.reduce_while(Stream.iterate(0, &(&1 + 2)), [], fn skip, acc ->
        page = index_info(db_name, limit: 2, skip: skip)
        acc = acc ++ view_group_ddocs(page)

        case skip + 2 >= page["total_rows"] do
          true -> {:halt, acc}
          false -> {:cont, acc}
        end
      end)

    assert seen == ["_design/a", "_design/b", "_design/c"]

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

    # keys in the body, as for _design_docs and _dbs_info; entries stay sorted
    # by ddoc and ids of missing design docs contribute nothing
    resp =
      Couch.post("/#{db_name}/_index_info",
        body: %{keys: ["_design/c", "_design/a", "_design/nope"]}
      )

    assert resp.status_code == 200
    assert Enum.sort(Map.keys(resp.body)) == @keys
    assert view_group_ddocs(resp.body) == ["_design/a", "_design/c"]
    # design docs are still counted, and the paging numbers are the ones
    # _design_docs reports for the same request
    assert resp.body["total_rows"] == 4

    keys = %{keys: ["_design/c", "_design/a", "_design/nope"]}
    ref = Couch.post("/#{db_name}/_design_docs", body: keys)
    assert ref.status_code == 200
    assert resp.body["total_rows"] == ref.body["total_rows"]
    assert resp.body["offset"] == ref.body["offset"]

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
    assert view_group_ddocs(index_info(db_name, keys: :jiffy.encode(["_design/b"]))) == ["_design/b"]

    resp = Couch.post("/#{db_name}/_index_info", body: %{})
    assert resp.status_code == 400
    assert resp.body["reason"] == "`keys` member must exist."

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

      assert view_group_ddocs(index_info(db_name, limit: 2)) == ["_design/a", "_design/b"]

      assert view_group_ddocs(index_info(db_name, startkey: :jiffy.encode("_design/b2"))) ==
               ["_design/c"]

      resp =
        Couch.post("/#{db_name}/_index_info",
          body: %{keys: ["_design/a", "_design/b", "_design/c"]}
        )

      assert resp.status_code == 400
      assert resp.body["reason"] == "too_many_keys"

      resp = Couch.post("/#{db_name}/_index_info", body: %{keys: ["_design/a", "_design/c"]})
      assert resp.status_code == 200
      assert view_group_ddocs(resp.body) == ["_design/a", "_design/c"]
    end)
  end
end

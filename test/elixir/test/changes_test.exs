defmodule ChangesTest do
  use CouchTestCase

  @moduletag :changes
  @moduletag kind: :single_node

  @moduledoc """
  Test CouchDB /{db}/_changes
  """

  @tag :with_db
  test "Changes feed negative heartbeat", context do
    db_name = context[:db_name]

    resp =
      Couch.get(
        "/#{db_name}/_changes",
        query: %{
          :feed => "continuous",
          :heartbeat => -1000
        }
      )

    assert resp.status_code == 400
    assert resp.body["error"] == "bad_request"

    assert resp.body["reason"] ==
             "The heartbeat value should be a positive integer (in milliseconds)."
  end

  @tag :with_db
  test "Changes feed non-integer heartbeat", context do
    db_name = context[:db_name]

    resp =
      Couch.get(
        "/#{db_name}/_changes",
        query: %{
          :feed => "continuous",
          :heartbeat => "a1000"
        }
      )

    assert resp.status_code == 400
    assert resp.body["error"] == "bad_request"

    assert resp.body["reason"] ==
             "Invalid heartbeat value. Expecting a positive integer value (in milliseconds)."
  end

  @tag :with_db
  test "function filtered changes", context do
    db_name = context[:db_name]
    create_filters_view(db_name)

    resp = Couch.get("/#{db_name}/_changes?filter=changes_filter/bop")
    assert Enum.empty?(resp.body["results"]), "db must be empty"

    {:ok, doc_resp} = create_doc(db_name, %{bop: "foom"})
    rev = doc_resp.body["rev"]
    id = doc_resp.body["id"]
    create_doc(db_name, %{bop: false})

    resp = Couch.get("/#{db_name}/_changes?filter=changes_filter/bop")
    assert length(resp.body["results"]) == 1
    change_rev = get_change_rev_at(resp.body["results"], 0)
    assert change_rev == rev

    doc = open_doc(db_name, id)
    doc = Map.put(doc, "newattr", "a")

    doc = save_doc(db_name, doc)

    resp = Couch.get("/#{db_name}/_changes?filter=changes_filter/bop")
    assert length(resp.body["results"]) == 1
    new_change_rev = get_change_rev_at(resp.body["results"], 0)
    assert new_change_rev == doc["_rev"]
    assert new_change_rev != change_rev

    resp = Couch.get("/#{db_name}/_changes?filter=changes_filter/dynamic&field=woox")
    assert Enum.empty?(resp.body["results"]), "db must be empty"

    resp = Couch.get("/#{db_name}/_changes?filter=changes_filter/dynamic&field=bop")
    assert length(resp.body["results"]) == 1, "db must have one change"
    new_change_rev = get_change_rev_at(resp.body["results"], 0)
    assert new_change_rev == doc["_rev"]
  end

  @tag :with_db
  test "non-existing desing doc for filtered changes", context do
    db_name = context[:db_name]
    resp = Couch.get("/#{db_name}/_changes?filter=nothingtosee/bop")
    assert resp.status_code == 404
  end

  @tag :with_db
  test "non-existing function for filtered changes", context do
    db_name = context[:db_name]
    create_filters_view(db_name)
    resp = Couch.get("/#{db_name}/_changes?filter=changes_filter/movealong")
    assert resp.status_code == 404
  end

  @tag :with_db
  test "non-existing desing doc and funcion for filtered changes", context do
    db_name = context[:db_name]
    resp = Couch.get("/#{db_name}/_changes?filter=nothingtosee/movealong")
    assert resp.status_code == 404
  end

  @tag :with_db
  test "map function filtered changes", context do
    db_name = context[:db_name]
    create_filters_view(db_name)
    create_doc(db_name, %{_id: "blah", bop: "plankton"})
    resp = Couch.get("/#{db_name}/_changes?filter=_view&view=changes_filter/blah")
    assert length(resp.body["results"]) == 1
    assert Enum.at(resp.body["results"], 0)["id"] == "blah"
  end

  @tag :with_db
  test "changes limit", context do
    db_name = context[:db_name]

    create_doc(db_name, %{_id: "blah", bop: "plankton"})
    create_doc(db_name, %{_id: "blah2", bop: "plankton"})
    create_doc(db_name, %{_id: "blah3", bop: "plankton"})

    resp = Couch.get("/#{db_name}/_changes?limit=1")
    assert length(resp.body["results"]) == 1

    resp = Couch.get("/#{db_name}/_changes?limit=2")
    assert length(resp.body["results"]) == 2
  end

  @tag :with_db
  test "erlang function filtered changes", context do
    db_name = context[:db_name]
    create_erlang_filters_view(db_name)

    resp = Couch.get("/#{db_name}/_changes?filter=erlang/foo")
    assert Enum.empty?(resp.body["results"])

    create_doc(db_name, %{_id: "doc1", value: 1})
    create_doc(db_name, %{_id: "doc2", value: 2})
    create_doc(db_name, %{_id: "doc3", value: 3})
    create_doc(db_name, %{_id: "doc4", value: 4})

    resp = Couch.get("/#{db_name}/_changes?filter=erlang/foo")

    changes_ids =
      resp.body["results"]
      |> Enum.map(fn p -> p["id"] end)

    assert Enum.member?(changes_ids, "doc2")
    assert Enum.member?(changes_ids, "doc4")
    assert length(resp.body["results"]) == 2
  end

  @tag :with_db
  test "changes filtering on docids", context do
    db_name = context[:db_name]
    doc_ids = %{doc_ids: ["doc1", "doc3", "doc4"]}

    resp =
      Couch.post("/#{db_name}/_changes?filter=_doc_ids",
        body: doc_ids,
        headers: ["Content-Type": "application/json"]
      )

    assert Enum.empty?(resp.body["results"])

    create_doc(db_name, %{_id: "doc1", value: 1})
    create_doc(db_name, %{_id: "doc2", value: 2})

    resp =
      Couch.post("/#{db_name}/_changes?filter=_doc_ids",
        body: doc_ids,
        headers: ["Content-Type": "application/json"]
      )

    assert length(resp.body["results"]) == 1
    assert Enum.at(resp.body["results"], 0)["id"] == "doc1"

    create_doc(db_name, %{_id: "doc3", value: 3})

    resp =
      Couch.post("/#{db_name}/_changes?filter=_doc_ids",
        body: doc_ids,
        headers: ["Content-Type": "application/json"]
      )

    assert length(resp.body["results"]) == 2

    changes_ids =
      resp.body["results"]
      |> Enum.map(fn p -> p["id"] end)

    assert Enum.member?(changes_ids, "doc1")
    assert Enum.member?(changes_ids, "doc3")

    encoded_doc_ids = doc_ids.doc_ids |> :jiffy.encode()

    resp =
      Couch.get("/#{db_name}/_changes",
        query: %{filter: "_doc_ids", doc_ids: encoded_doc_ids}
      )

    assert length(resp.body["results"]) == 2

    changes_ids =
      resp.body["results"]
      |> Enum.map(fn p -> p["id"] end)

    assert Enum.member?(changes_ids, "doc1")
    assert Enum.member?(changes_ids, "doc3")
  end

  @tag :with_db
  test "changes filtering on design docs", context do
    db_name = context[:db_name]

    create_erlang_filters_view(db_name)
    create_doc(db_name, %{_id: "doc1", value: 1})

    resp = Couch.get("/#{db_name}/_changes?filter=_design")
    assert length(resp.body["results"]) == 1
    assert Enum.at(resp.body["results"], 0)["id"] == "_design/erlang"
  end

  @tag :with_db
  test "COUCHDB-1037-empty result for ?limit=1&filter=foo/bar in some cases",
       context do
    db_name = context[:db_name]

    filter_fun = """
     function(doc, req) {
        return (typeof doc.integer === "number");
      }
    """

    ddoc = %{
      _id: "_design/testdocs",
      language: "javascript",
      filters: %{
        testdocsonly: filter_fun
      }
    }

    create_doc(db_name, ddoc)

    ddoc = %{
      _id: "_design/foobar",
      foo: "bar"
    }

    create_doc(db_name, ddoc)
    bulk_save(db_name, make_docs(0..4))

    resp = Couch.get("/#{db_name}/_changes")
    assert length(resp.body["results"]) == 7

    resp = Couch.get("/#{db_name}/_changes?limit=1&filter=testdocs/testdocsonly")
    assert length(resp.body["results"]) == 1
    # we can't guarantee ordering
    assert Regex.match?(~r/[0-4]/, Enum.at(resp.body["results"], 0)["id"])

    resp = Couch.get("/#{db_name}/_changes?limit=2&filter=testdocs/testdocsonly")
    assert length(resp.body["results"]) == 2
    # we can't guarantee ordering
    assert Regex.match?(~r/[0-4]/, Enum.at(resp.body["results"], 0)["id"])
    assert Regex.match?(~r/[0-4]/, Enum.at(resp.body["results"], 1)["id"])
  end

  @tag :with_db
  test "COUCHDB-1256", context do
    db_name = context[:db_name]
    {:ok, resp} = create_doc(db_name, %{_id: "foo", a: 123})
    create_doc(db_name, %{_id: "bar", a: 456})
    foo_rev = resp.body["rev"]

    Couch.put("/#{db_name}/foo?new_edits=false",
      headers: ["Content-Type": "application/json"],
      body: %{_rev: foo_rev, a: 456}
    )

    resp = Couch.get("/#{db_name}/_changes?style=all_docs")
    assert length(resp.body["results"]) == 2

    resp =
      Couch.get("/#{db_name}/_changes",
        query: %{style: "all_docs", since: Enum.at(resp.body["results"], 0)["seq"]}
      )

    assert length(resp.body["results"]) == 1
  end

  @tag :with_db
  test "COUCHDB-1923", context do
    db_name = context[:db_name]
    attachment_data = "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="

    docs =
      make_docs(20..29, %{
        _attachments: %{
          "foo.txt": %{
            content_type: "text/plain",
            data: attachment_data
          },
          "bar.txt": %{
            content_type: "text/plain",
            data: attachment_data
          }
        }
      })

    bulk_save(db_name, docs)

    resp = Couch.get("/#{db_name}/_changes?include_docs=true")
    assert length(resp.body["results"]) == 10

    first_doc = Enum.at(resp.body["results"], 0)["doc"]

    assert first_doc["_attachments"]["foo.txt"]["stub"]
    assert not Enum.member?(first_doc["_attachments"]["foo.txt"], "data")
    assert not Enum.member?(first_doc["_attachments"]["foo.txt"], "encoding")
    assert not Enum.member?(first_doc["_attachments"]["foo.txt"], "encoded_length")
    assert first_doc["_attachments"]["bar.txt"]["stub"]
    assert not Enum.member?(first_doc["_attachments"]["bar.txt"], "data")
    assert not Enum.member?(first_doc["_attachments"]["bar.txt"], "encoding")
    assert not Enum.member?(first_doc["_attachments"]["bar.txt"], "encoded_length")

    resp = Couch.get("/#{db_name}/_changes?include_docs=true&attachments=true")
    assert length(resp.body["results"]) == 10

    first_doc = Enum.at(resp.body["results"], 0)["doc"]

    assert not Enum.member?(first_doc["_attachments"]["foo.txt"], "stub")
    assert first_doc["_attachments"]["foo.txt"]["data"] == attachment_data
    assert not Enum.member?(first_doc["_attachments"]["foo.txt"], "encoding")
    assert not Enum.member?(first_doc["_attachments"]["foo.txt"], "encoded_length")

    assert not Enum.member?(first_doc["_attachments"]["bar.txt"], "stub")
    assert first_doc["_attachments"]["bar.txt"]["data"] == attachment_data
    assert not Enum.member?(first_doc["_attachments"]["bar.txt"], "encoding")
    assert not Enum.member?(first_doc["_attachments"]["bar.txt"], "encoded_length")

    resp = Couch.get("/#{db_name}/_changes?include_docs=true&att_encoding_info=true")
    assert length(resp.body["results"]) == 10

    first_doc = Enum.at(resp.body["results"], 0)["doc"]

    assert first_doc["_attachments"]["foo.txt"]["stub"]
    assert not Enum.member?(first_doc["_attachments"]["foo.txt"], "data")
    assert first_doc["_attachments"]["foo.txt"]["encoding"] == "gzip"
    assert first_doc["_attachments"]["foo.txt"]["encoded_length"] == 47
    assert first_doc["_attachments"]["bar.txt"]["stub"]
    assert not Enum.member?(first_doc["_attachments"]["bar.txt"], "data")
    assert first_doc["_attachments"]["bar.txt"]["encoding"] == "gzip"
    assert first_doc["_attachments"]["bar.txt"]["encoded_length"] == 47
  end

  defp create_erlang_filters_view(db_name) do
    erlang_fun = """
     fun({Doc}, Req) ->
      case couch_util:get_value(<<"value">>, Doc) of
        undefined -> false;
           Value -> (Value rem 2) =:= 0;
        _ -> false
      end
     end.
    """

    ddoc = %{
      _id: "_design/erlang",
      language: "erlang",
      filters: %{
        foo: erlang_fun
      }
    }

    create_doc(db_name, ddoc)
  end

  defp create_filters_view(db_name) do
    dynamic_fun = """
    function(doc, req) {
      var field = req.query.field;
      return doc[field];
    }
    """

    userctx_fun = """
    function(doc, req) {
      var field = req.query.field;
      return doc[field];
    }
    """

    blah_fun = """
    function(doc) {
              if (doc._id == "blah") {
              emit(null, null);
            }
          }
    """

    ddoc = %{
      _id: "_design/changes_filter",
      filters: %{
        bop: "function(doc, req) { return (doc.bop);}",
        dynamic: dynamic_fun,
        userCtx: userctx_fun,
        conflicted: "function(doc, req) { return (doc._conflicts);}"
      },
      options: %{
        local_seq: true
      },
      views: %{
        local_seq: %{
          map: "function(doc) {emit(doc._local_seq, null)}"
        },
        blah: %{
          map: blah_fun
        }
      }
    }

    create_doc(db_name, ddoc)
  end

  defp get_change_rev_at(results, idx) do
    results
    |> Enum.at(idx)
    |> Map.fetch!("changes")
    |> Enum.at(0)
    |> Map.fetch!("rev")
  end

  defp open_doc(db_name, id) do
    resp = Couch.get("/#{db_name}/#{id}")
    assert resp.status_code == 200
    resp.body
  end

  defp save_doc(db_name, body) do
    resp = Couch.put("/#{db_name}/#{body["_id"]}", body: body)
    assert resp.status_code in [201, 202]
    assert resp.body["ok"]
    Map.put(body, "_rev", resp.body["rev"])
  end
end

defmodule ViewUpdateSeqTest do
  use CouchTestCase

  @moduletag :view_update_seq
  @moduletag kind: :single_node

  @moduledoc """
  This is a port of the view_update_seq.js test suite.
  """

  @design_doc %{
    _id: "_design/test",
    language: "javascript",
    autoupdate: false,
    views: %{
      all_docs: %{
        map: "function(doc) { emit(doc.integer, doc.string) }"
      },
      summate: %{
        map:
          "function (doc) { if (typeof doc.integer === 'number') { emit(doc.integer, doc.integer)}; }",
        reduce: "function (keys, values) { return sum(values); };"
      }
    }
  }

  defp seq_int(seq) do
    {int, _} =
      seq
      |> String.split("-")
      |> Enum.at(0)
      |> Integer.parse()

    int
  end

  @tag :with_db
  test "db info update seq", context do
    db_name = context[:db_name]

    info = info(db_name)
    assert seq_int(info["update_seq"]) == 0

    create_doc(db_name, @design_doc)

    info = info(db_name)
    assert seq_int(info["update_seq"]) == 1
  end

  @tag :with_db
  test "_all_docs update seq", context do
    db_name = context[:db_name]

    resp = Couch.get("/#{db_name}/_all_docs", query: %{:update_seq => true})
    assert seq_int(resp.body["update_seq"]) == 0

    create_doc(db_name, @design_doc)

    resp = Couch.get("/#{db_name}/_all_docs", query: %{:update_seq => true})
    assert length(resp.body["rows"]) == 1
    assert seq_int(resp.body["update_seq"]) == 1

    docs = make_docs(0..99)
    bulk_save(db_name, docs)

    resp = Couch.get("/#{db_name}/_all_docs", query: %{:limit => 1})
    assert length(resp.body["rows"]) == 1
    assert Map.has_key?(resp.body, "update_seq") == false

    resp = Couch.get("/#{db_name}/_all_docs", query: %{:limit => 1, :update_seq => true})
    assert length(resp.body["rows"]) == 1
    assert seq_int(resp.body["update_seq"]) == 101
  end

  @tag :with_db
  test "view update seq", context do
    db_name = context[:db_name]

    create_doc(db_name, @design_doc)
    docs = make_docs(0..99)
    bulk_save(db_name, docs)

    resp = view(db_name, "test/all_docs", %{:limit => 1, :update_seq => true})
    assert length(resp.body["rows"]) == 1
    assert seq_int(resp.body["update_seq"]) == 101

    resp = view(db_name, "test/all_docs", %{:limit => 1, :update_seq => false})
    assert length(resp.body["rows"]) == 1
    assert Map.has_key?(resp.body, "update_seq") == false

    resp = view(db_name, "test/summate", %{:update_seq => true})
    assert length(resp.body["rows"]) == 1
    assert seq_int(resp.body["update_seq"]) == 101

    save(db_name, %{"_id" => "A", "integer" => 1})

    resp =
      view(db_name, "test/all_docs", %{:limit => 1, :stale => "ok", :update_seq => true})

    assert length(resp.body["rows"]) == 1
    assert seq_int(resp.body["update_seq"]) == 101

    save(db_name, %{"_id" => "AA", "integer" => 2})

    resp =
      view(db_name, "test/all_docs", %{
        :limit => 1,
        :stale => "update_after",
        :update_seq => true
      })

    assert length(resp.body["rows"]) == 1
    assert seq_int(resp.body["update_seq"]) == 101

    retry_until(fn ->
      resp =
        view(db_name, "test/all_docs", %{:limit => 1, :stale => "ok", :update_seq => true})

      assert length(resp.body["rows"]) == 1
      seq_int(resp.body["update_seq"]) == 103
    end)

    resp =
      view(db_name, "test/all_docs", %{:limit => 1, :stale => "ok", :update_seq => true})

    assert length(resp.body["rows"]) == 1
    assert seq_int(resp.body["update_seq"]) == 103

    resp = view(db_name, "test/all_docs", %{:limit => 1, :update_seq => true})

    assert length(resp.body["rows"]) == 1
    assert seq_int(resp.body["update_seq"]) == 103

    resp = view(db_name, "test/all_docs", %{:update_seq => true}, ["0", "1"])
    assert seq_int(resp.body["update_seq"]) == 103

    resp = view(db_name, "test/all_docs", %{:update_seq => true}, ["0", "1"])
    assert seq_int(resp.body["update_seq"]) == 103

    resp = view(db_name, "test/summate", %{:group => true, :update_seq => true}, [0, 1])
    assert seq_int(resp.body["update_seq"]) == 103
  end
end

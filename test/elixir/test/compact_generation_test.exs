defmodule CompactGenerationTest do
  use CouchTestCase

  @moduletag :compact

  @moduledoc """
  Test generational CouchDB compaction
  """

  @db_name "testdb"

  test "increase the max generation" do
    db_path = setup_db(0)

    make_doc("the-doc", %{ value: "DOC BODY" })
    make_att("the-doc", "the-att", "ATT DATA")

    gen_compact(0)
    check_files(db_path, "DOC BODY", [1])
    check_files(db_path, "ATT DATA", [1])

    Couch.put("/#{@db_name}/_max_generation", body: "2")

    gen_compact(0)
    check_files(db_path, "DOC BODY", [0, 1, 0])
    check_files(db_path, "ATT DATA", [0, 1, 0])
  end

  test "promote docs and atts into gen-1" do
    db_path = setup_db(1)

    make_doc("the-doc", %{ value: "DOC BODY" })
    make_att("the-doc", "the-att", "ATT DATA")

    check_files(db_path, "DOC BODY", [2, 0])
    check_files(db_path, "ATT DATA", [1, 0])

    gen_compact(0)
    check_files(db_path, "DOC BODY", [0, 1])
    check_files(db_path, "ATT DATA", [0, 1])
  end

  test "read doc and attachment after compaction" do
    db_path = setup_db(2)

    make_doc("the-doc", %{ value: "DOC BODY" })
    make_att("the-doc", "the-att", "ATT DATA")

    gen_compact(0)
    gen_compact(1)

    %{ "value" => value } = Couch.get("/#{@db_name}/the-doc").body
    assert value == "DOC BODY"

    att = Couch.get("/#{@db_name}/the-doc/the-att").body
    assert att == "ATT DATA"
  end

  test "read doc after open file timeout" do
    db_path = setup_db(2)

    make_doc("the-doc", %{ value: "DOC BODY" })
    make_att("the-doc", "the-att", "ATT DATA")

    gen_compact(0)
    gen_compact(1)
    Process.sleep(70000)

    %{ "value" => value } = Couch.get("/#{@db_name}/the-doc").body
    assert value == "DOC BODY"

    att = Couch.get("/#{@db_name}/the-doc/the-att").body
    assert att == "ATT DATA"
  end

  test "add an attachment to a compacted doc" do
    db_path = setup_db(1)

    make_doc("the-doc", %{ value: "DOC BODY" })
    gen_compact(0)
    make_att("the-doc", "the-att", "ATT DATA")

    check_files(db_path, "DOC BODY", [1, 1])
    check_files(db_path, "ATT DATA", [1, 0])
  end

  test "compact an attachment added to a compacted doc" do
    db_path = setup_db(1)

    make_doc("the-doc", %{ value: "DOC BODY" })
    gen_compact(0)
    make_att("the-doc", "the-att", "ATT DATA")
    gen_compact(0)

    check_files(db_path, "DOC BODY", [0, 2])
    check_files(db_path, "ATT DATA", [0, 1])
  end

  test "compact a doc and att an additional generation" do
    db_path = setup_db(2)

    make_doc("the-doc", %{ value: "DOC BODY" })
    gen_compact(0)
    make_att("the-doc", "the-att", "ATT DATA")
    gen_compact(0)
    gen_compact(1)

    check_files(db_path, "DOC BODY", [0, 0, 1])
    check_files(db_path, "ATT DATA", [0, 0, 1])
  end

  test "update a compacted doc" do
    db_path = setup_db(1)

    make_doc("the-doc", %{ value: "DOC BODY" })
    make_att("the-doc", "the-att", "ATT DATA")
    gen_compact(0)
    update_doc("the-doc", %{ value: "NEW TEXT" })

    check_files(db_path, "DOC BODY", [0, 1])
    check_files(db_path, "ATT DATA", [0, 1])
    check_files(db_path, "NEW TEXT", [1 ,0])
  end

  test "compact an updated doc" do
    db_path = setup_db(1)

    make_doc("the-doc", %{ value: "DOC BODY" })
    make_att("the-doc", "the-att", "ATT DATA")
    update_doc("the-doc", %{ value: "NEW TEXT" })
    gen_compact(0)

    check_files(db_path, "DOC BODY", [0, 0])
    check_files(db_path, "ATT DATA", [0, 1])
    check_files(db_path, "NEW TEXT", [0, 1])
  end

  test "compact, update and compact a doc" do
    db_path = setup_db(1)

    make_doc("the-doc", %{ value: "DOC BODY" })
    make_att("the-doc", "the-att", "ATT DATA")
    gen_compact(0)
    update_doc("the-doc", %{ value: "NEW TEXT" })
    gen_compact(0)

    check_files(db_path, "DOC BODY", [0, 1])
    check_files(db_path, "ATT DATA", [0, 1])
    check_files(db_path, "NEW TEXT", [0, 1])
  end

  test "compact, update then compact twice" do
    db_path = setup_db(2)

    make_doc("the-doc", %{ value: "DOC BODY" })
    make_att("the-doc", "the-att", "ATT DATA")
    gen_compact(0)
    update_doc("the-doc", %{ value: "NEW TEXT" })
    gen_compact(0)
    gen_compact(1)

    check_files(db_path, "DOC BODY", [0, 0, 0])
    check_files(db_path, "ATT DATA", [0, 0, 1])
    check_files(db_path, "NEW TEXT", [0, 0, 1])
  end

  test "compact through multiple generations" do
    db_path = setup_db(3)

    make_doc("the-doc", %{ value: "DOC BODY" })
    make_att("the-doc", "the-att", "ATT DATA")
    gen_compact(0)
    gen_compact(1)
    make_att("the-doc", "extra-att", "EXTRA STUFF")

    check_files(db_path, "DOC BODY",    [1, 0, 1, 0])
    check_files(db_path, "ATT DATA",    [0, 0, 1, 0])
    check_files(db_path, "EXTRA STUFF", [1, 0, 0, 0])

    gen_compact(0)

    check_files(db_path, "DOC BODY",    [0, 1, 1, 0])
    check_files(db_path, "ATT DATA",    [0, 0, 1, 0])
    check_files(db_path, "EXTRA STUFF", [0, 1, 0, 0])

    gen_compact(1)

    check_files(db_path, "DOC BODY",    [0, 0, 2, 0])
    check_files(db_path, "ATT DATA",    [0, 0, 1, 0])
    check_files(db_path, "EXTRA STUFF", [0, 0, 1, 0])

    gen_compact(2)

    check_files(db_path, "DOC BODY",    [0, 0, 0, 1])
    check_files(db_path, "ATT DATA",    [0, 0, 0, 1])
    check_files(db_path, "EXTRA STUFF", [0, 0, 0, 1])
  end

  test "compact max generation (0)" do
    db_path = setup_db(0)

    make_doc("the-doc", %{ value: "DOC BODY" })
    make_att("the-doc", "the-att", "ATT DATA")

    check_files(db_path, "DOC BODY", [2])
    check_files(db_path, "ATT DATA", [1])

    gen_compact(0)

    check_files(db_path, "DOC BODY", [1])
    check_files(db_path, "ATT DATA", [1])
  end

  test "compact max generation (1)" do
    db_path = setup_db(1)

    make_doc("the-doc", %{ value: "DOC BODY" })
    make_att("the-doc", "the-att", "ATT DATA")

    check_files(db_path, "DOC BODY", [2, 0])
    check_files(db_path, "ATT DATA", [1, 0])

    gen_compact(0)

    check_files(db_path, "DOC BODY", [0, 1])
    check_files(db_path, "ATT DATA", [0, 1])

    gen_compact(1)

    check_files(db_path, "DOC BODY", [0, 1])
    check_files(db_path, "ATT DATA", [0, 1])
  end

  test "retention in gen 0" do
    db_path = setup_db(2)

    make_doc("doc-1", %{ the: ["first", "doc"] })
    make_att("doc-1", "att-1", "something")

    check_files(db_path, "first",     [2, 0, 0])
    check_files(db_path, "something", [1, 0, 0])

    gen_compact(0)

    check_files(db_path, "first",     [0, 1, 0])
    check_files(db_path, "something", [0, 1, 0])

    make_doc("doc-2", %{ a: ["second", "doc"] })
    make_att("doc-2", "att-2", "anything")

    check_files(db_path, "first",     [0, 1, 0])
    check_files(db_path, "something", [0, 1, 0])
    check_files(db_path, "second",    [2, 0, 0])
    check_files(db_path, "anything",  [1, 0, 0])

    gen_compact(1)
    gen_compact(1)
    gen_compact(1)

    check_files(db_path, "first",     [0, 0, 1])
    check_files(db_path, "something", [0, 0, 1])
    check_files(db_path, "second",    [1, 0, 0])
    check_files(db_path, "anything",  [1, 0, 0])
  end

  test "compact final generation" do
    db_path = setup_db(3)
    make_doc("the-doc", %{ value: "DOC BODY" })

    gen_compact(0)
    gen_compact(1)
    gen_compact(2)

    check_files(db_path, "DOC BODY", [0, 0, 0, 1])

    make_att("the-doc", "the-att", "ATT DATA")

    check_files(db_path, "DOC BODY", [1, 0, 0, 1])
    check_files(db_path, "ATT DATA", [1, 0, 0, 0])

    gen_compact(0)

    check_files(db_path, "DOC BODY", [0, 1, 0, 1])
    check_files(db_path, "ATT DATA", [0, 1, 0, 0])

    gen_compact(1)

    check_files(db_path, "DOC BODY", [0, 0, 1, 1])
    check_files(db_path, "ATT DATA", [0, 0, 1, 0])

    gen_compact(2)

    check_files(db_path, "DOC BODY", [0, 0, 0, 2])
    check_files(db_path, "ATT DATA", [0, 0, 0, 1])

    gen_compact(3)

    check_files(db_path, "DOC BODY", [0, 0, 0, 1])
    check_files(db_path, "ATT DATA", [0, 0, 0, 1])
  end

  test "copy compacted doc on moving attachment" do
    db_path = setup_db(3)
    make_doc("the-doc", %{ value: "DOC BODY" })

    gen_compact(0)
    gen_compact(1)

    check_files(db_path, "DOC BODY", [0, 0, 1, 0])

    make_att("the-doc", "the-att", "ATT DATA")

    check_files(db_path, "DOC BODY", [1, 0, 1, 0])
    check_files(db_path, "ATT DATA", [1, 0, 0, 0])

    gen_compact(0)

    check_files(db_path, "DOC BODY", [0, 1, 1, 0])
    check_files(db_path, "ATT DATA", [0, 1, 0, 0])

    update_doc("the-doc", %{ value: "NEW TEXT" })

    check_files(db_path, "DOC BODY", [0, 1, 1, 0])
    check_files(db_path, "ATT DATA", [0, 1, 0, 0])
    check_files(db_path, "NEW TEXT", [1, 0, 0, 0])

    gen_compact(1)

    check_files(db_path, "DOC BODY", [0, 0, 1, 0])
    check_files(db_path, "ATT DATA", [0, 0, 1, 0])
    check_files(db_path, "NEW TEXT", [1, 0, 0, 0])

    gen_compact(0)

    check_files(db_path, "DOC BODY", [0, 0, 1, 0])
    check_files(db_path, "ATT DATA", [0, 0, 1, 0])
    check_files(db_path, "NEW TEXT", [0, 1, 0, 0])

    gen_compact(2)

    check_files(db_path, "DOC BODY", [0, 0, 0, 0])
    check_files(db_path, "ATT DATA", [0, 0, 0, 1])
    check_files(db_path, "NEW TEXT", [0, 2, 0, 0])
  end

  test "compact with partition limit" do
    Couch.put("/_node/_local/_config/couchdb/max_partition_size", body: "\"10240\"")

    Couch.delete("/#{@db_name}")
    resp = Couch.put("/#{@db_name}?partitioned=true&q=1&gen=2")
    assert resp.status_code == 201

    docs = for x <- 1..15, do: %{ _id: "foo:#{x}", value: String.pad_leading("", 1024, "0") }
    resp = Couch.post("/#{@db_name}/_bulk_docs", body: %{ docs: docs })
    assert resp.status_code == 201

    gen_compact(0)

    resp = Couch.post("/#{@db_name}/_bulk_docs", body: %{ docs: [%{_id: "foo:bar"}, %{_id: "baz:bang"}] })
    assert resp.status_code == 201

    resp = Couch.get("/#{@db_name}/_all_docs")
    ids = for doc <- resp.body["rows"], do: doc["id"]

    assert ids == ["baz:bang", "foo:1", "foo:10", "foo:11", "foo:12", "foo:13",
      "foo:14", "foo:15", "foo:2", "foo:3", "foo:4", "foo:5", "foo:6", "foo:7",
      "foo:8", "foo:9"]
  end

  test "transfer space used from gen 0 to gen 1" do
    setup_db(1)

    make_doc("the-doc", %{ value: "DOC BODY" })
    make_att("the-doc", "the-att", String.pad_leading("", 4096, "0"))

    %{ "file" => f, "external" => e, "active" => a } = Couch.get("/#{@db_name}").body["sizes"]

    assert f > 0
    assert e > 0
    assert a > 0

    gen_compact(0)

    [
      %{ "file" => f0, "external" => e0, "active" => a0 },
      %{ "file" => f1, "external" => e1, "active" => a1 }
    ] = Couch.get("/#{@db_name}").body["sizes"]

    assert f0 < f
    assert e0 < e
    assert a0 < a

    assert f1 > 0
    assert e1 > 0
    assert a1 > 0
  end

  defp setup_db(max_gen) do
    Couch.delete("/#{@db_name}")
    resp = Couch.put("/#{@db_name}?q=1&gen=#{max_gen}")
    assert resp.status_code == 201
    {
      Couch.get("/_node/_local/_config/couchdb").body["database_dir"],
      Couch.get("/#{@db_name}").body["instance_start_time"]
    }
  end

  defp make_doc(doc_id, doc) do
    resp = Couch.put("/#{@db_name}/#{doc_id}", body: doc)
    assert resp.status_code == 201
  end

  defp update_doc(doc_id, new_doc) do
    old_doc = Couch.get("/#{@db_name}/#{doc_id}").body
    resp = Couch.put("/#{@db_name}/#{doc_id}", body: Map.merge(old_doc, new_doc))
    assert resp.status_code == 201
  end

  defp make_att(doc_id, att_id, data) do
    rev = Couch.get("/#{@db_name}/#{doc_id}").body["_rev"]
    headers = ["Content-Type": "application/octet-stream"]
    resp = Couch.put("/#{@db_name}/#{doc_id}/#{att_id}?rev=#{rev}", headers: headers, body: data)
    assert resp.status_code == 201
  end

  defp gen_compact(gen) do
    compact(@db_name, gen)
  end

  defp check_files(dir, needle, counts) do
    check_files(dir, needle, counts, 0)
  end

  defp check_files(_dir, _needle, [], _gen) do
    :ok
  end

  defp check_files(dir, needle, [count | rest], gen) do
    check_file(dir, gen, needle, count)
    check_files(dir, needle, rest, gen + 1)
  end

  defp check_file(dir, gen, needle, expect_count) do
    path = db_path(dir, gen)
    {:ok, file_data} = File.read(path)
    chunks = file_data |> String.split(needle) |> length()

    assert chunks - 1 == expect_count,
      "in file '#{path}', expected '#{needle}' to appear #{expect_count} times, but it was found #{chunks - 1} times"
  end

  defp db_path({data_dir, suffix}, gen) do
    Path.join([data_dir, "shards", "00000000-ffffffff", db_file(suffix, gen)])
  end

  defp db_file(suffix, 0) do
    "#{@db_name}.#{suffix}.couch"
  end

  defp db_file(suffix, gen) do
    "#{@db_name}.#{suffix}.#{gen}.couch"
  end
end

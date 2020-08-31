defmodule JsonpTest do
  use CouchTestCase

  @moduletag :jsonp
  @moduletag kind: :single_node

  @tag :with_db
  test "jsonp not configured callbacks", context do
    db_name = context[:db_name]
    {:ok, _} = create_doc(db_name, %{_id: "0", a: 0, b: 0})

    resp = Couch.get("/#{db_name}/0?callback=jsonp_no_chunk")
    assert resp.status_code == 200
    assert resp.headers.hdrs["content-type"] == "application/json"
  end

  @tag :with_db
  test "jsonp unchunked callbacks", context do
    db_name = context[:db_name]

    server_config = [
      %{
        :section => "httpd",
        :key => "allow_jsonp",
        :value => "true"
      }
    ]

    {:ok, create_resp} = create_doc(db_name, %{_id: "0", a: 0, b: 0})

    run_on_modified_server(server_config, fn ->
      resp = Couch.get("/#{db_name}/0?callback=jsonp_no_chunk")

      assert resp.status_code == 200
      assert resp.headers.hdrs["content-type"] == "application/javascript"

      {callback_fun, callback_param} = parse_callback(resp.body)

      assert callback_fun == "jsonp_no_chunk"
      assert create_resp.body["id"] == callback_param["_id"]
      assert create_resp.body["rev"] == callback_param["_rev"]

      resp = Couch.get("/#{db_name}/0?callback=jsonp_no_chunk\"")
      assert resp.status_code == 400
    end)
  end

  @tag :with_db
  test "jsonp chunked callbacks", context do
    db_name = context[:db_name]

    server_config = [
      %{
        :section => "httpd",
        :key => "allow_jsonp",
        :value => "true"
      }
    ]

    design_doc = %{
      _id: "_design/test",
      language: "javascript",
      views: %{
        all_docs: %{map: "function(doc) {if(doc.a) emit(null, doc.a);}"}
      }
    }

    {:ok, _} = create_doc(db_name, design_doc)
    {:ok, _} = create_doc(db_name, %{_id: "0", a: 0, b: 0})
    {:ok, _} = create_doc(db_name, %{_id: "1", a: 1, b: 1})

    run_on_modified_server(server_config, fn ->
      resp = Couch.get("/#{db_name}/_design/test/_view/all_docs?callback=jsonp_chunk")
      assert resp.status_code == 200
      assert resp.headers.hdrs["content-type"] == "application/javascript"

      {callback_fun, callback_param} = parse_callback(resp.body)

      assert callback_fun == "jsonp_chunk"
      assert callback_param["total_rows"] == 1

      resp = Couch.get("/#{db_name}/_design/test/_view/all_docs?callback=jsonp_chunk'")
      assert resp.status_code == 400

      resp = Couch.get("/#{db_name}/_changes?callback=jsonp_chunk")
      assert resp.status_code == 200
      assert resp.headers.hdrs["content-type"] == "application/javascript"

      {callback_fun, callback_param} = parse_callback(resp.body)
      assert callback_fun == "jsonp_chunk"
      assert length(callback_param["results"]) == 3

    end)
  end

  defp parse_callback(msg) do
    captures = Regex.scan(~r/\/\* CouchDB \*\/(\w+)\((.*)\)/s, msg)

    callback_fun =
      captures
      |> Enum.map(fn p -> Enum.at(p, 1) end)
      |> Enum.at(0)

    param =
      captures
      |> Enum.map(fn p -> Enum.at(p, 2) end)
      |> Enum.filter(fn p -> String.trim(p) != "" end)
      |> Enum.map(fn p ->
        p
        |> IO.iodata_to_binary()
        |> :jiffy.decode([:return_maps])
      end)
      |> Enum.at(0)

    {callback_fun, param}
  end
end

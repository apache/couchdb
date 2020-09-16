defmodule HttpTest do
  use CouchTestCase

  @moduletag :http
  @moduletag kind: :single_node

  @tag :with_db
  test "location header", context do
    db_name = context[:db_name]
    resp = Couch.put("/#{db_name}/test", body: %{})
    db_url = Couch.process_url("/" <> db_name)
    assert resp.headers.hdrs["location"] == db_url <> "/test"
  end

  @tag :with_db
  test "location header should include X-Forwarded-Host", context do
    db_name = context[:db_name]

    resp =
      Couch.put("/#{db_name}/test2",
        body: %{},
        headers: ["X-Forwarded-Host": "mysite.com"]
      )

    assert resp.headers.hdrs["location"] == "http://mysite.com/#{db_name}/test2"
  end

  @tag :with_db
  test "location header should include custom header", context do
    db_name = context[:db_name]

    server_config = [
      %{
        :section => "httpd",
        :key => "x_forwarded_host",
        :value => "X-Host"
      }
    ]

    run_on_modified_server(server_config, fn ->
      resp =
        Couch.put("/#{db_name}/test3",
          body: %{},
          headers: ["X-Host": "mysite2.com"]
        )

      assert resp.headers.hdrs["location"] == "http://mysite2.com/#{db_name}/test3"
    end)
  end

  @tag :with_db
  test "COUCHDB-708: newlines document names", context do
    db_name = context[:db_name]

    resp =
      Couch.put("/#{db_name}/docid%0A/attachment.txt",
        body: %{},
        headers: ["Content-Type": "text/plain;charset=utf-8"]
      )

    db_url = Couch.process_url("/" <> db_name)
    assert resp.headers.hdrs["location"] == db_url <> "/docid%0A/attachment.txt"

    resp =
      Couch.put("/#{db_name}/docidtest%0A",
        body: %{},
        headers: ["Content-Type": "text/plain;charset=utf-8"]
      )

    db_url = Couch.process_url("/" <> db_name)
    assert resp.headers.hdrs["location"] == db_url <> "/docidtest%0A"

    resp =
      Couch.post("/#{db_name}/",
        body: %{_id: "docidtestpost%0A"},
        headers: ["Content-Type": "application/json"]
      )

    db_url = Couch.process_url("/" <> db_name)
    assert resp.headers.hdrs["location"] == db_url <> "/docidtestpost%250A"
  end
end

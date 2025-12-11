defmodule DiskMonitorTest do
  use CouchTestCase

  @moduletag :disk_monitor

  setup_all do
    set_config({"disk_monitor", "enable", "true"})
    :ok
  end

  @tag :with_db
  test "block background view indexing", context do
    set_config({"disk_monitor", "background_view_indexing_threshold", "0"})
    set_config({"disk_monitor", "interactive_view_indexing_threshold", "100"})

    db_name = context[:db_name]
    resp = Couch.post("/#{db_name}", body: %{:_id => "foo"})
    assert resp.status_code == 201

    map_doc = %{:views => %{:bar => %{:map => "function(doc) { emit(); }"}}}
    assert Couch.put("/#{db_name}/_design/foo", body: map_doc).body["ok"]
    :timer.sleep(500)
    resp = Couch.get("/#{db_name}/_design/foo/_view/bar?stale=ok")
    assert resp.body["total_rows"] == 0
    resp = Couch.get("/#{db_name}/_design/foo/_view/bar")
    assert resp.body["total_rows"] == 1
  end

  @tag :with_db
  test "block interactive view indexing", context do
    set_config({"disk_monitor", "background_view_indexing_threshold", "100"})
    set_config({"disk_monitor", "interactive_view_indexing_threshold", "0"})

    db_name = context[:db_name]
    resp = Couch.post("/#{db_name}", body: %{:_id => "foo"})
    assert resp.status_code == 201

    map_doc = %{:views => %{:bar => %{:map => "function(doc) { emit(); }"}}}
    assert Couch.put("/#{db_name}/_design/foo", body: map_doc).body["ok"]
    resp = Couch.get("/#{db_name}/_design/foo/_view/bar")
    assert resp.status_code == 507
    resp = Couch.get("/#{db_name}/_design/foo/_view/bar?stale=ok")
    assert resp.status_code == 200
  end

  @tag :with_db
  test "block interactive database writes", context do
    set_config({"disk_monitor", "interactive_database_writes_threshold", "0"})

    db_name = context[:db_name]
    resp = Couch.post("/#{db_name}", body: %{:_id => "foo"})
    assert resp.status_code == 507
  end

end

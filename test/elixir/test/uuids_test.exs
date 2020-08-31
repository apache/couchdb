defmodule UUIDsTest do
  use CouchTestCase

  @moduledoc """
  Test CouchDB UUIDs API
  This is a port of the uuids.js suite
  """

  @moduletag :docs
  @moduletag kind: :single_node

  test "cache busting headers are set" do
    resp = Couch.get("/_uuids")
    assert resp.status_code == 200
    assert Regex.match?(~r/no-cache/, resp.headers["Cache-Control"])
    assert resp.headers["Pragma"] == "no-cache"
    assert String.length(resp.headers["ETag"]) > 0
  end

  test "can return single uuid" do
    resp = Couch.get("/_uuids")
    assert resp.status_code == 200
    [uuid1] = resp.body["uuids"]

    resp = Couch.get("/_uuids", query: %{:count => 1})
    assert resp.status_code == 200
    [uuid2] = resp.body["uuids"]

    assert uuid1 != uuid2
  end

  test "no duplicates in 1,000 UUIDs" do
    resp = Couch.get("/_uuids", query: %{:count => 1000})
    assert resp.status_code == 200
    uuids = resp.body["uuids"]

    assert length(Enum.uniq(uuids)) == length(uuids)
  end

  test "Method Not Allowed error on POST" do
    resp = Couch.post("/_uuids", query: %{:count => 1000})
    assert resp.status_code == 405
  end

  test "Bad Request error when exceeding max UUID count" do
    resp = Couch.get("/_uuids", query: %{:count => 1001})
    assert resp.status_code == 400
  end

  @tag config: [
         {"uuids", "algorithm", "sequential"}
       ]
  test "sequential uuids are sequential" do
    resp = Couch.get("/_uuids", query: %{:count => 1000})
    assert resp.status_code == 200

    Enum.reduce(resp.body["uuids"], fn curr, acc ->
      assert String.length(curr) == 32
      assert acc < curr
      curr
    end)
  end

  @tag config: [
         {"uuids", "algorithm", "utc_random"}
       ]
  test "utc_random uuids are roughly random" do
    resp = Couch.get("/_uuids", query: %{:count => 1000})
    assert resp.status_code == 200
    uuids = resp.body["uuids"]

    assert String.length(Enum.at(uuids, 1)) == 32

    # Assert no collisions
    assert length(Enum.uniq(uuids)) == length(uuids)

    # Assert rough ordering of UUIDs
    u1 = String.slice(Enum.at(uuids, 1), 0..13)
    u2 = String.slice(Enum.at(uuids, -1), 0..13)
    assert u1 < u2
  end

  @utc_id_suffix "frog"
  @tag config: [
         {"uuids", "algorithm", "utc_id"},
         {"uuids", "utc_id_suffix", @utc_id_suffix}
       ]
  test "utc_id uuids are correct" do
    resp = Couch.get("/_uuids", query: %{:count => 10})
    assert resp.status_code == 200

    Enum.reduce(resp.body["uuids"], fn curr, acc ->
      assert String.length(curr) == 14 + String.length(@utc_id_suffix)
      assert String.slice(curr, 14..-1) == @utc_id_suffix
      assert curr > acc
      curr
    end)
  end
end

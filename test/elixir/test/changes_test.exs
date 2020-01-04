defmodule ChangesTest do
  use CouchTestCase

  @moduletag :changes

  @moduledoc """
  Test CouchDB /{db}/_changes
  """

  @tag :with_db
  test "Changes feed negative heartbeat", context do
    db_name = context[:db_name]

    resp = Couch.get(
      "/#{db_name}/_changes",
      query: %{
        :feed => "continuous",
        :heartbeat => -1000
      }
    )

    assert resp.status_code == 400
    assert resp.body["error"] == "bad_request"
    assert resp.body["reason"] == "The heartbeat value should be a positive integer (in milliseconds)."
  end

  @tag :with_db
  test "Changes feed non-integer heartbeat", context do
    db_name = context[:db_name]

    resp = Couch.get(
      "/#{db_name}/_changes",
      query: %{
        :feed => "continuous",
        :heartbeat => "a1000"
      }
    )

    assert resp.status_code == 400
    assert resp.body["error"] == "bad_request"
    assert resp.body["reason"] == "Invalid heartbeat value. Expecting a positive integer value (in milliseconds)."
  end
end

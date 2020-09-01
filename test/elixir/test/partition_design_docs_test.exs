defmodule PartitionDesignDocsTest do
  use CouchTestCase

  @moduledoc """
  Test Partition functionality for partition design docs
  """

  @moduletag :partition
  @moduletag kind: :cluster

  @tag :with_partitioned_db
  test "/_partition/:pk/_design/doc 404", context do
    db_name = context[:db_name]

    url = "/#{db_name}/_partition/fakekey/_design/mrtest/_view/some"
    resp = Couch.get(url)
    assert resp.status_code == 404
  end
end

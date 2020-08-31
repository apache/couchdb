defmodule MethodOverrideTest do
  use CouchTestCase

  @moduletag :http
  @moduletag kind: :single_node

  @moduledoc """
   Allow broken HTTP clients to fake a full method vocabulary with an
   X-HTTP-METHOD-OVERRIDE header
  """

  @tag :with_db
  test "method override PUT", context do
    db_name = context[:db_name]

    resp =
      Couch.post("/#{db_name}/fnord",
        body: %{bob: "connie"},
        headers: ["X-HTTP-Method-Override": "PUT"]
      )

    assert resp.status_code == 201

    resp = Couch.get("/#{db_name}/fnord")
    assert resp.body["bob"] == "connie"
  end

  @tag :with_db
  test "method override DELETE", context do
    db_name = context[:db_name]
    {:ok, resp} = create_doc(db_name, %{_id: "fnord", bob: "connie"})

    resp =
      Couch.post("/#{db_name}/fnord?rev=#{resp.body["rev"]}",
        headers: ["X-HTTP-Method-Override": "DELETE"]
      )

    assert resp.status_code == 200

    resp = Couch.get("/#{db_name}/fnord")
    assert resp.status_code == 404
  end

  @tag :with_db
  test "Method Override is ignored when original Method isn't POST", context do
    db_name = context[:db_name]

    resp =
      Couch.get("/#{db_name}/fnord2",
        body: %{bob: "connie"},
        headers: ["X-HTTP-Method-Override": "PUT"]
      )

    assert resp.status_code == 404
  end
end

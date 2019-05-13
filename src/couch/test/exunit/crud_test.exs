defmodule Couch.Test.CRUD do
  use ExUnit.Case
  alias Couch.Test.Adapter
  alias Couch.Test.Utils, as: Utils

  alias Couch.Test.Setup

  require Record

  test_groups = [
    "using Clustered API": Adapter.Clustered,
    "using Backdoor API": Adapter.Backdoor,
    "using Fabric API": Adapter.Fabric
  ]

  for {describe, adapter} <- test_groups do
    describe "Database CRUD #{describe}" do
      @describetag setup:
                     %Setup{}
                     |> Setup.Start.new([:chttpd])
                     |> Setup.Adapter.new(adapter)
                     |> Setup.Admin.new(user: "adm", password: "pass")
                     |> Setup.Login.new(user: "adm", password: "pass")
      test "Create", %{setup: setup} do
        db_name = Utils.random_name("db")
        setup_ctx = setup |> Setup.run()
        assert {:ok, resp} = Adapter.create_db(Setup.get(setup_ctx, :adapter), db_name)
        assert resp.body["ok"]
        # TODO query all dbs to make sure db_name is added
      end
    end
  end
end

defmodule Couch.Test.Setup.Create.DB do
  @moduledoc """
  This `setup` would create database with given name
  """
  alias Couch.Test.Setup
  alias Couch.Test.Setup.Login
  import ExUnit.Assertions, only: [assert: 1, assert: 2]
  def new(setup, db_name) do
    setup |> Setup.step(__MODULE__, db_name)
  end

  def setup(ctx, db_name) do
    assert Setup.completed?(Login), "Require `Login` setup"
    assert {ok, resp} = Adapter.create_db(ctx[:adapter], db_name)
    assert resp.body["ok"]
    {Map.put(ctx, :dbs, [db_name | ctx[:dbs] || []]), nil}
  end

  def teardown(ctx, db_name, _state) do
    Adapter.delete_db(ctx[:adapter], db_name)
    :ok
  end
end

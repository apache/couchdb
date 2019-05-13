defmodule Couch.Test.Setup.Adapter do
  @moduledoc """
  Setup step to configure testing adapter to select
  appropriate testing interface.

  Supported adapters are:
  - Couch.Test.Adapter.Clustered
  - Couch.Test.Adapter.Backdoor
  - Couch.Test.Adapter.Fabric

  start couch and some extra applications

  It is usually called as:
  ```
      test "Create" do
        ctx =
          %Setup{}
          |> Setup.Start.new([:chttpd])
          |> Setup.Adapter.new(Clustered)
          ...
          |> Setup.run()
          ...
      end
  ```
  """
  alias Couch.Test.Adapter
  alias Couch.Test.Adapter.{Clustered, Backdoor, Fabric}
  alias Couch.Test.Setup

  alias Couch.Test.Setup.Start
  import ExUnit.Assertions, only: [assert: 2]
  def new(setup, args) do
    setup |> Setup.step(__MODULE__, args)
  end
  def setup(setup, Clustered) do
    assert Setup.has_key?(setup, :clustered_url), "Require `chttpd` application"
    state = Clustered.new(Setup.get(setup, :clustered_url))
    {Setup.put(setup, :adapter, state), state}
  end
  def setup(setup, Backdoor) do
    assert Setup.has_key?(setup, :backdoor_url), "Require `Start` setup"
    state = Backdoor.new(Setup.get(setup, :backdoor_url))
    {Setup.put(setup, :adapter, state), state}
  end
  def setup(setup, Fabric) do
    assert Setup.completed?(setup, Start), "Require `Start` setup"
    state = Fabric.new()
    {Setup.put(setup, :adapter, state), state}
  end
  def teardown(_setup, _args, _state) do
    :ok
  end
  def create_db(setup, db_name) do
    Adapter.create_db(Setup.get(setup, :adapter), db_name)
  end
  def delete_db(setup, db_name) do
    Adapter.delete_db(Setup.get(setup, :adapter), db_name)
  end
end
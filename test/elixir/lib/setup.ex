defmodule Couch.Test.Setup do
  @moduledoc """
  Allows to chain setup functions.
  Example of using:

  ```
      @tag setup: %Setup{}
          |> Setup.Start.new([:chttpd])
          |> Setup.Adapter.new(adapter)
          |> Setup.Admin.new(user: "adm", password: "pass")
          |> Setup.Login.new(user: "adm", password: "pass")
      test "Create", %{setup: setup} do
        db_name = Utils.random_name("db")
        ctx = setup |> Setup.run()
        ...
      end
  ```
  """
  import ExUnit.Callbacks, only: [on_exit: 1]
  import ExUnit.Assertions, only: [assert: 2]
  require Logger

  alias Couch.Test.Setup
  defstruct user_acc: %{}, stages: [], completed: []

  def step(%Setup{stages: stages} = setup, step, args) do
    %{setup | stages: [{step, args, nil} | stages]}
  end
  defp setup_step({step, args, _}, setup) do
    # credo:disable-for-next-line Credo.Check.Warning.LazyLogging
    Logger.debug("Calling 'setup/2' for '#{step}'")
    assert {%Setup{completed: completed} = nsetup, state} = step.setup(setup, args), "Failure in setup for '#{step}''"
    completed = [step | completed]
    on_exit(fn ->
      # credo:disable-for-next-line Credo.Check.Warning.LazyLogging
      Logger.debug("Calling 'teardown/3' for '#{step}'")
      step.teardown(nsetup, args, state)
    end)
    setup = %Setup{nsetup | completed: completed}
    {{step, args, state}, setup}
  end
  def run(%Setup{stages: stages} = setup) do
    {stages, setup} = stages
      |> Enum.reverse
      |> Enum.map_reduce(setup, &setup_step/2)
    %{setup | stages: stages}
  end
  def completed?(%Setup{completed: completed}, step) do
    step in completed
  end
  def ctx(%Setup{user_acc: ctx}) do
    ctx
  end
  def put(%Setup{user_acc: ctx} = setup, key, value) do
    %Setup{setup | user_acc: Map.put(ctx, key, value)}
  end
  def get(%Setup{user_acc: ctx}, key, default \\ nil) do
    Map.get(ctx, key, default)
  end
  def has_key?(%Setup{user_acc: ctx}, key) do
    Map.has_key?(ctx, key)
  end
end
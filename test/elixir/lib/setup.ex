defmodule Couch.Test.Setup do
  @moduledoc """
  Allows to chain setup functions.
  Example of using:

  ```
    alias Couch,Test.Utils
    def with_db_name(context, setup) do
      setup =
        setup
          |> Step.Start.new(:start, extra_apps: [:chttpd])
          |> Step.User.new(:admin, roles: [:server_admin])
          |> Setup.run()

      context =
        Map.merge(context, %{
          db_name: Utils.random_name("db")
          base_url: setup |> Setup.get(:start) |> Step.Start.clustered_url(),
          user: setup |> Setup.get(:admin) |> Step.User.name()
        })
      {context, setup}
    end

    @tag setup: &__MODULE__.with_db_name/2
      test "Create", %{db_name: db_name, user: user} do
        ...
      end
  ```
  """
  import ExUnit.Callbacks, only: [on_exit: 1]
  import ExUnit.Assertions, only: [assert: 2]
  require Logger

  alias Couch.Test.Setup
  alias Couch.Test.Setup.Step
  defstruct stages: [], by_type: %{}, state: %{}

  def step(%Setup{stages: stages} = setup, id, step) do
    %{setup | stages: [{id, step} | stages]}
  end

  defp setup_step({id, step}, %Setup{state: state, by_type: by_type} = setup) do
    %module{} = step
    # credo:disable-for-next-line Credo.Check.Warning.LazyLogging
    Logger.debug("Calling 'setup/2' for '#{module}'")
    step = module.setup(setup, step)
    state = Map.put(state, id, step)
    by_type = Map.update(by_type, module, [id], fn ids -> [id | ids] end)
    on_exit(fn ->
      # credo:disable-for-next-line Credo.Check.Warning.LazyLogging
      Logger.debug("Calling 'teardown/3' for '#{module}'")
      try do
        module.teardown(setup, step)
        :ok
      catch
        _ -> :ok
        _, _ -> :ok
      end
    end)
    {{id, step}, %{setup | state: state, by_type: by_type}}
  end

  def run(%Setup{stages: stages} = setup) do
    {stages, setup} = stages
      |> Enum.reverse
      |> Enum.map_reduce(setup, &setup_step/2)
    %{setup | stages: stages}
  end

  def setup(ctx) do
    Map.get(ctx, :__setup)
  end

  def setup(ctx, setup_fun) do
    setup = %Setup{} |> Step.Config.new(:test_config, config_file: nil)
    {ctx, setup} = setup_fun.(ctx, setup)
    assert not Map.has_key?(ctx, :__setup), "Key `__setup` is reserved for internal purposes"
    Map.put(ctx, :__setup, setup)
  end

  def completed?(%Setup{by_type: by_type}, step) do
    Map.has_key?(by_type, step)
  end

  def all_for(%Setup{by_type: by_type, state: state}, step_module) do
    Map.take(state, by_type[step_module] || [])
  end

  def reduce_for(setup, step_module, acc, fun) do
    Enum.reduce(all_for(setup, step_module), acc, fun)
  end

  def get(%Setup{state: state}, id) do
    state[id]
  end

end
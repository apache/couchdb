defmodule Couch.Test.Setup.Step.Create.DB do
  @moduledoc """
    This setup step creates a database with given name.
    If name is not provided random name would be used.

    Example
      setup
        ...
        |> Setup.Step.Create.DB.new(:db)
        ...
        |> Setup.run
      ...

      db_name = setup |> Setup.get(:db) |> Setup.Step.Create.DB.name
  """
  alias Couch.Test.Setup
  alias Couch.Test.Setup.Step
  alias Couch.Test.Utils

  defstruct [:name]

  import ExUnit.Assertions, only: [assert: 1, assert: 2]

  import Utils

  @admin {:user_ctx, user_ctx(roles: ["_admin"])}

  def new(setup, id) do
    new(setup, id,  name: Utils.random_name("db"))
  end

  def new(setup, id, name: name) do
    setup |> Setup.step(id, %__MODULE__{name: name})
  end

  def setup(setup, %__MODULE__{name: name} = step) do
    assert Setup.completed?(setup, Step.Start), "Require `Start` step"
    assert :fabric in Step.Start.apps(), "Fabric is not started"
    res = :fabric.create_db(name, [@admin])
    assert res in [:ok, :accepted], "Cannot create `#{name}` database"
    step
  end

  def teardown(setup, %__MODULE__{name: name} = step) do
    :fabric.delete_db(name, [@admin])
    :ok
  end

  def name(%__MODULE__{name: name}) do
    name
  end

end
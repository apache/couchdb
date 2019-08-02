defmodule Couch.Test.Setup.Step do
  @moduledoc """
  A behaviour module for implementing custom setup steps for future reuse.

  Every module implementing this behaviour must implement following three functions:
  - new
  - setup
  - teardown

  Here is an example of a custom step
  ```
  defmodule Couch.Test.Setup.Step.Foo do

    alias Couch.Test.Setup

    defstruct [:foo_data, :foo_arg]

    def new(setup, id, arg: arg) do
      setup |> Setup.step(id, %__MODULE__{foo_arg: arg})
    end

    def setup(_setup, %__MODULE__{foo_arg: arg} = step) do
      ...
      foo_data = ...
      %{step | foo_data: foo_data}
    end

    def teardown(_setup, _step) do
    end

    def get_data(%__MODULE__{foo_data: data}) do
      data
    end
  end
  ```
  """
  @type t :: struct()
  @callback new(setup :: %Couch.Test.Setup{}, id :: atom(), args: Keyword.t()) ::
    %Couch.Test.Setup{}
  @callback setup(setup :: %Couch.Test.Setup{}, step :: t()) ::
    t()
  @callback teardown(setup :: %Couch.Test.Setup{}, step :: t()) ::
    any()
end
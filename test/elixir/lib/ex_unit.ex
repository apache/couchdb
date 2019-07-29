defmodule Couch.Test.ExUnit.Case do
  @moduledoc """
  Template for ExUnit test case. It can be used as follows:
  ```
  defmodule Couch.Test.CRUD do
    use Couch.Test.ExUnit.Case
    ...
    def with_db(context, setup) do
      setup = setup
        |> Step.Start.new(:start, extra_apps: [:chttpd])
        |> Setup.run
      context = Map.merge(context, %{
        base_url: setup |> Setup.get(:start) |> Step.Start.clustered_url
      })
      {context, setup}
    end
    describe "Group of tests" do
    @describetag setup: &__MODULE__.with_db/2
    test "Single test in a group", ctx do
      ctx.base_url
    end
    ...
  end
  ```
  """

  use ExUnit.CaseTemplate
  alias Couch.Test.Setup

  using do
    quote do
      require Logger
      use ExUnit.Case
    end
  end

  setup context do
    case context do
      %{:setup => setup_fun} ->
        {:ok, Setup.setup(context, setup_fun)}
      _ -> {:ok, context}
    end
  end
end
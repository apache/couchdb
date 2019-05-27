defmodule Couch.Test.Setup.Step.Config do
  @moduledoc """
    This setup reads configuration for a test run.
    It is not supposed to be called manually.
  """

  alias Couch.Test.Setup

  defstruct [:config, :config_file]

  def new(setup, id, config_file: config_file) do
    setup |> Setup.step(id, %__MODULE__{config_file: config_file})
  end

  def setup(_setup, %__MODULE__{config_file: config_file} = step) do
    # TODO we would need to access config file here
    %{step | config: %{
       backdoor: %{
         protocol: "http"
       },
       clustered: %{
         protocol: "http"
       }
    }}
  end

  def teardown(_setup, _step) do
  end

  def get(%__MODULE__{config: config}) do
    config
  end
end
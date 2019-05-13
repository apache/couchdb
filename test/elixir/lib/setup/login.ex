defmodule Couch.Test.Setup.Login do
  @moduledoc """
  This `setup` would establish a session by calling
  _session endpoint for given adapter. It is a noop
  for Fabric adapter
  """
  alias Couch.Test.Setup
  alias Couch.Test.Adapter
  import ExUnit.Assertions, only: [assert: 1, assert: 2]
  def new(setup, args) do
    assert Keyword.has_key?(args, :user), "`user` argument is missing"
    assert Keyword.has_key?(args, :password), "`password` argument is missing"
    setup |> Setup.step(__MODULE__, args |> Enum.into(%{}))
  end
  def setup(setup, %{user: user, password: password}) do
    assert Setup.completed?(setup, Setup.Adapter), "Require `Adapter` setup"
    adapter = Adapter.login(Setup.get(setup, :adapter), user, password)
    {Setup.put(setup, :adapter, adapter), adapter}
  end
  def teardown(_setup, _args, _state) do
    :ok
  end
end
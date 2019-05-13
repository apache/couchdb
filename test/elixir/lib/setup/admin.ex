defmodule Couch.Test.Setup.Admin do
  @moduledoc """
  This `setup` would make sure an admin user is created.
  It is taking a shortcut and wouldn't work in standalone
  integration test.
  """
  alias Couch.Test.Setup
  alias Couch.Test.Setup.Start
  import ExUnit.Assertions, only: [assert: 2]
  def new(setup, args) do
    assert Keyword.has_key?(args, :user), "`user` argument is missing"
    assert Keyword.has_key?(args, :password), "`password` argument is missing"
    setup |> Setup.step(__MODULE__, args |> Enum.into(%{}))
  end
  def setup(setup, args) do
    assert Setup.completed?(setup, Start), "Require `Start` setup"
    # Latter we might want to mock config:set/4 so it would
    # return configured value specifically for this user.
    # This would allow us to execute tests concurently
    # given we use random name for user
    :config.set('admins', String.to_charlist(args[:user]), String.to_charlist(args[:password]), false)
    {Setup.put(setup, :admin, args), nil}
  end
  def teardown(_setup, args, _state) do
    :config.delete("admins", args[:user], false)   
    :ok
  end
end
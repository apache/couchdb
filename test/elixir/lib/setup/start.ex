defmodule Couch.Test.Setup.Start do
  @moduledoc """
  Setup step to start couch and some extra applications

  It is usually called as:
  ```
      test "Create" do
        ctx =
          %Setup{}
          |> Setup.Start.new([:chttpd])
          ...
          |> Setup.run()
          ...
      end
  ```
  """
  alias Couch.Test.Setup
  import ExUnit.Assertions, only: [assert: 1, assert: 2]
  def new() do
    new(%Setup{})
  end
  def new(setup) do
    new(setup, [])
  end
  def new(setup, extra_apps) do
    setup |> Setup.step(__MODULE__, extra_apps)
  end
  def setup(setup, extra_apps) do
    state = :test_util.start_couch(extra_apps)
    addr = :config.get('couch_httpd', 'bind_address', '127.0.0.1')
    port = :mochiweb_socket_server.get(:couch_httpd, :port)
    url = "https://#{addr}:#{port}"
    setup = Setup.put(setup, :backdoor_url, url)
    if :chttpd in extra_apps do
      addr = :config.get('chttpd', 'bind_address', '127.0.0.1')
      port = :mochiweb_socket_server.get(:chttpd, :port)
      url = "https://#{addr}:#{port}"
      setup = Setup.put(setup, :clustered_url, url)
    end
    {setup, state}
  end
  def teardown(setup, args, state) do
    :test_util.stop_couch(state)
  end
end

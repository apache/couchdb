defmodule Couch.Test.Setup.Step.Start do
  @moduledoc """
  Step to start a set of couchdb applications. By default it starts
  list of applications from DEFAULT_APPS macro defined in `test_util.erl`.
  At the time of writing this list included:
    - inets
    - ibrowse
    - ssl
    - config
    - couch_epi
    - couch_event
    - couch

  It is possible to specify additional list of applications to start.

  This setup is also maintains `clustered_url` and `backdoor_url` for future use.
  The value for `clustered_url` could be nil if :chttpd app is not included in extra_apps.

  Example
    setup
      |> Setup.Step.Start.new(:start, extra_apps: [:fabric, :chttpd])
    ...
      |> Setup.run
    ...

    started_apps = Setup.Step.Start.apps
    clustered_url = setup |> Setup.get(:start) |> Setup.Step.Start.clustered_url
    backdoor_url = setup |> Setup.get(:start) |> Setup.Step.Start.backdoor_url
  """
  alias Couch.Test.Setup
  alias Couch.Test.Setup.Step

  defstruct [:test_ctx, :extra_apps, :clustered_url, :backdoor_url]

  def new(setup, id, extra_apps: extra_apps) do
    setup |> Setup.step(id, %__MODULE__{extra_apps: extra_apps || []})
  end

  def setup(setup, %__MODULE__{extra_apps: extra_apps} = step) do
    test_config = setup |> Setup.get(:test_config) |> Step.Config.get()
    protocol = test_config[:backdoor][:protocol] || "http"
    test_ctx = :test_util.start_couch(extra_apps)
    addr = :config.get("couch_httpd", "bind_address", "127.0.0.1")
    port = :mochiweb_socket_server.get(:couch_httpd, :port)
    backdoor_url = "#{protocol}://#{addr}:#{port}"
    clustered_url =
      if :chttpd in extra_apps do
        protocol = test_config[:clustered][:protocol] || "http"
        addr = :config.get("chttpd", "bind_address", "127.0.0.1")
        port = :mochiweb_socket_server.get(:chttpd, :port)
        "#{protocol}://#{addr}:#{port}"
      else
        nil
      end
    %{step |
      test_ctx: test_ctx,
      clustered_url: clustered_url,
      backdoor_url: backdoor_url
    }
  end

  def teardown(_setup, %__MODULE__{test_ctx: test_ctx}) do
    :test_util.stop_couch(test_ctx)
  end

  def backdoor_url(%__MODULE__{backdoor_url: url}) do
    url
  end

  def clustered_url(%__MODULE__{clustered_url: url}) do
    url
  end

  def extra_apps(%__MODULE__{extra_apps: apps}) do
    apps
  end

  @doc """
  Returns list of currently running applications
  """
  def apps() do
    for {x, _, _} <- Application.started_applications, do: x
  end

end

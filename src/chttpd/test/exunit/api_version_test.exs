defmodule :api_version_test do
  # couch_epi_plugin behaviour
  def app(), do: __MODULE__

  def providers() do
    [{:chttpd_handlers, __MODULE__}]
  end

  def services(), do: []
  def data_providers(), do: []
  def data_subscriptions(), do: []
  def processes(), do: []
  def notify(_, _, _), do: :ok

  # chttpd_handlers callbacks
  def url_handler("_test_api", 1), do: &handle_test_v1/1
  def url_handler("_test_api", 2), do: &handle_test_v2/1

  def url_handler(path, _), do: :no_match

  def db_handler(_, _), do: :no_match

  def design_handler(_, _), do: :no_match

  def handler_info(_, _, _), do: :no_match

  # endpoints implementation
  def handle_test_v1(req) do
    :chttpd.send_json(req, %{version: 1})
  end

  def handle_test_v2(req) do
    :chttpd.send_json(req, %{version: 2})
  end
end

defmodule Couch.Test.APIVersioning do
  use ExUnit.Case
  import Couch.DBTest, only: [retry_until: 1]
  alias Couch.DBTest, as: Utils

  defp create_admin(user_name, password) do
    hashed = String.to_charlist(:couch_passwords.hash_admin_password(password))
    :config.set('admins', String.to_charlist(user_name), hashed, false)
  end

  defp base_url() do
    addr = :config.get('chttpd', 'bind_address', '127.0.0.1')
    port = :mochiweb_socket_server.get(:chttpd, :port)
    "http://#{addr}:#{port}"
  end

  defp setup_epi(module) do
    # in case it's already running from other tests...
    plugins = Application.get_env(:couch_epi, :plugins, [])
    Application.stop(:couch_epi)
    Application.unload(:couch_epi)
    Application.load(:couch_epi)

    Application.put_env(:couch_epi, :plugins, [
      module,
      :couch_db_epi,
      :fabric2_epi,
      :chttpd_epi
      | plugins
    ])
  end

  setup_all do
    setup_epi(:api_version_test)

    test_ctx =
      :test_util.start_couch([
        :couch_epi,
        :chttpd
      ])

    :ok = create_admin("adm", "pass")

    on_exit(fn ->
      :test_util.stop_couch(test_ctx)
    end)

    %{
      base_url: base_url(),
      user: "adm",
      pass: "pass"
    }
  end

  defp with_session(context) do
    session = Couch.login(context.user, context.pass, base_url: context.base_url)
    %{session: session}
  end

  for api <- [1, 2] do
    describe "Version #{api} of the API" do
      @describetag api: api
      setup [:with_session]

      test "specified via path", ctx do
        resp = Couch.Session.get(ctx.session, "/_v#{ctx.api}/_test_api")

        assert resp.status_code == 200, "got error #{inspect(resp.body)}"
        assert ctx.api == resp.body["version"]
      end

      test "specified via header", ctx do
        resp =
          Couch.Session.get(
            ctx.session,
            "/_test_api",
            headers: [
              "X-Couch-API": ctx.api
            ]
          )

        assert resp.status_code == 200, "got error #{inspect(resp.body)}"
        assert ctx.api == resp.body["version"]
      end

      test "specified via accept", ctx do
        resp =
          Couch.Session.get(
            ctx.session,
            "/_test_api",
            headers: [
              Accept: "application/couchdb; _v=#{ctx.api},application/json"
            ]
          )

        assert resp.status_code == 200, "got error #{inspect(resp.body)}"
        assert ctx.api == resp.body["version"]
      end
    end
  end

  describe "defaults" do
    setup [:with_session]

    test "not specified", ctx do
      resp = Couch.Session.get(ctx.session, "/_test_api")

      assert resp.status_code == 200, "got error #{inspect(resp.body)}"
      assert 1 == resp.body["version"]
    end
  end

  describe "failure scenario" do
    setup [:with_session]

    test "inconsistent versions", ctx do
      resp =
        Couch.Session.get(
          ctx.session,
          "/_v1/_test_api",
          headers: [
            Accept: "application/couchdb; _v=2,application/json"
          ]
        )

      assert resp.status_code == 400, "got error #{inspect(resp.body)}"
      assert "bad_request" == resp.body["error"]
      assert "conflicted API versions" == resp.body["reason"]
    end
  end
end

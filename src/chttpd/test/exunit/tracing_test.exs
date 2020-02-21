defmodule Couch.Test.OpenTracing do
  use Couch.Test.ExUnit.Case
  alias Couch.Test.Setup
  alias Couch.Test.Setup.Step
  alias Couch.Test.Utils
  import Couch.DBTest, only: [retry_until: 1]

  defp create_admin(user_name, password) do
    hashed = String.to_charlist(:couch_passwords.hash_admin_password(password))
    :config.set('admins', String.to_charlist(user_name), hashed, false)
  end

  defp base_url() do
    addr = :config.get('chttpd', 'bind_address', '127.0.0.1')
    port = :mochiweb_socket_server.get(:chttpd, :port)
    "http://#{addr}:#{port}"
  end

  setup_all context do
    test_ctx = :test_util.start_couch([:chttpd])
    :ok = create_admin("adm", "pass")

    Map.merge(context, %{
      base_url: base_url(),
      user: "adm",
      pass: "pass"
    })
  end

  setup context do
    db_name = Utils.random_name("db")
    session = Couch.login(context.user, context.pass, base_url: context.base_url)

    on_exit(fn ->
      delete_db(session, db_name)
    end)

    create_db(session, db_name)

    Map.merge(context, %{
      db_name: db_name,
      session: session
    })
  end

  def create_db(session, db_name, opts \\ []) do
    retry_until(fn ->
      resp = Couch.Session.put(session, "/#{db_name}", opts)
      assert resp.status_code in [201, 202]
      assert resp.body == %{"ok" => true}
      {:ok, resp}
    end)
  end

  def delete_db(session, db_name) do
    retry_until(fn ->
      resp = Couch.Session.delete(session, "/#{db_name}")
      assert resp.status_code in [200, 202, 404]
      {:ok, resp}
    end)
  end

  def create_doc(session, db_name, body) do
    retry_until(fn ->
      resp = Couch.Session.post(session, "/#{db_name}", body: body)
      assert resp.status_code in [201, 202]
      assert resp.body["ok"]
      {:ok, resp}
    end)
  end

  defp trace_id() do
    :couch_util.to_hex(:crypto.strong_rand_bytes(16))
  end

  defp span_id() do
    :couch_util.to_hex(:crypto.strong_rand_bytes(8))
  end

  describe "Open Tracing" do
    test "should return success with combined b3 header", ctx do
      %{session: session, db_name: db_name} = ctx
      doc = '{"mr": "rockoartischocko"}'
      {:ok, _} = create_doc(session, db_name, doc)

      resp =
        retry_until(fn ->
          b3 = "#{trace_id()}-#{span_id()}-#{span_id()}"

          response =
            Couch.Session.get(session, "/#{db_name}/_all_docs", headers: [b3: b3])

          assert %HTTPotion.Response{} = response
          response
        end)

      assert resp.status_code == 200, "Expected 200, got: #{resp.status_code}"
      assert length(resp.body["rows"]) == 1
    end
  end
end

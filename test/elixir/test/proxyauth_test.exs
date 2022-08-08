defmodule ProxyAuthTest do
  use CouchTestCase

  @moduletag :authentication

  @tag :with_db
  test "proxy auth with secret" do

    users_db_name = random_db_name()
    create_db(users_db_name)

    secret = generate_secret(64)

    server_config = [
      %{
        :section => "chttpd_auth",
        :key => "authentication_db",
        :value => users_db_name
      },
      %{
        :section => "chttpd_auth",
        :key => "proxy_use_secret",
        :value => "true"
      },
      %{
        :section => "chttpd_auth",
        :key => "secret",
        :value => secret
      }
    ]

    run_on_modified_server(server_config, fn ->
      test_fun(users_db_name, secret)
    end)
    delete_db(users_db_name)
  end

  defp generate_secret(len) do
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    |> String.splitter("", trim: true)
    |> Enum.take_random(len)
    |> Enum.join("")
  end

  defp hex_hmac_sha1(secret, message) do
    signature = :crypto.mac(:hmac, :sha, secret, message)
    Base.encode16(signature, case: :lower)
  end

  def test_fun(users_db_name, secret) do
    user = prepare_user_doc(name: "couch@apache.org", password: "test")
    create_doc(users_db_name, user)

    resp =
      Couch.get("/_session",
        headers: [authorization: "Basic Y291Y2hAYXBhY2hlLm9yZzp0ZXN0"]
      )

    assert resp.body["userCtx"]["name"] == "couch@apache.org"
    assert resp.body["info"]["authenticated"] == "default"

    headers = [
      "X-Auth-CouchDB-UserName": "couch@apache.org",
      "X-Auth-CouchDB-Roles": "test_role",
      "X-Auth-CouchDB-Token": hex_hmac_sha1(secret, "couch@apache.org")
    ]

    resp2 =
      Couch.get("/_session",
        headers: headers
      )

    assert resp2.body["userCtx"]["name"] == "couch@apache.org"
    assert resp2.body["userCtx"]["roles"] == ["test_role"]
    assert resp2.body["info"]["authenticated"] == "proxy"
    assert resp2.body["ok"] == true

  end

  @tag :with_db
  test "proxy auth without secret" do

    users_db_name = random_db_name()
    create_db(users_db_name)

    server_config = [
      %{
        :section => "chttpd_auth",
        :key => "authentication_db",
        :value => users_db_name
      },
      %{
        :section => "chttpd_auth",
        :key => "proxy_use_secret",
        :value => "false"
      }
    ]

    run_on_modified_server(server_config, fn ->
      test_fun_no_secret(users_db_name)
    end)

    delete_db(users_db_name)
  end

  def test_fun_no_secret(users_db_name) do
    user = prepare_user_doc(name: "couch@apache.org", password: "test")
    create_doc(users_db_name, user)

    resp =
      Couch.get("/_session",
        headers: [authorization: "Basic Y291Y2hAYXBhY2hlLm9yZzp0ZXN0"]
      )

    assert resp.body["userCtx"]["name"] == "couch@apache.org"
    assert resp.body["info"]["authenticated"] == "default"

    headers = [
      "X-Auth-CouchDB-UserName": "couch@apache.org",
      "X-Auth-CouchDB-Roles": "test_role_1,test_role_2"
    ]

    resp2 =
      Couch.get("/_session",
        headers: headers
      )

    assert resp2.body["userCtx"]["name"] == "couch@apache.org"
    assert resp2.body["userCtx"]["roles"] == ["test_role_1", "test_role_2"]
    assert resp2.body["info"]["authenticated"] == "proxy"
    assert resp2.body["ok"] == true

  end
end

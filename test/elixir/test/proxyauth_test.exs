defmodule ProxyAuthTest do
  use CouchTestCase

  @moduletag :authentication

  @tag :with_db
  test "proxy auth with secret", context do
    db_name = context[:db_name]

    design_doc = %{
      _id: "_design/test",
      language: "javascript",
      shows: %{
        welcome: """
           function(doc,req) {
          return "Welcome " + req.userCtx["name"];
        }
        """,
        role: """
          function(doc, req) {
          return req.userCtx['roles'][0];
        }
        """
      }
    }

    {:ok, _} = create_doc(db_name, design_doc)

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
      test_fun(db_name, users_db_name, secret)
    end)
    delete_db(users_db_name)
  end

  defp generate_secret(len) do
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    |> String.splitter("", trim: true)
    |> Enum.take_random(len)
    |> Enum.join("")
  end

  defp hex_hmac_sha256(secret, message) do
    signature = case :erlang.system_info(:otp_release) do
      _ -> :crypto.mac(:hmac, :sha256, secret, message)
    end
    Base.encode16(signature, case: :lower)
  end

  def test_fun(db_name, users_db_name, secret) do
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
      "X-Auth-CouchDB-Roles": "test",
      "X-Auth-CouchDB-Token": hex_hmac_sha256(secret, "couch@apache.org")
    ]
    resp = Couch.get("/#{db_name}/_design/test/_show/welcome", headers: headers)
    assert resp.body == "Welcome couch@apache.org"

    resp = Couch.get("/#{db_name}/_design/test/_show/role", headers: headers)
    assert resp.body == "test"
  end

  @tag :with_db
  test "proxy auth without secret", context do
    db_name = context[:db_name]

    design_doc = %{
      _id: "_design/test",
      language: "javascript",
      shows: %{
        welcome: """
           function(doc,req) {
          return "Welcome " + req.userCtx["name"];
        }
        """,
        role: """
          function(doc, req) {
          return req.userCtx['roles'][0];
        }
        """
      }
    }

    {:ok, _} = create_doc(db_name, design_doc)

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
      test_fun_no_secret(db_name, users_db_name)
    end)

    delete_db(users_db_name)
  end

  def test_fun_no_secret(db_name, users_db_name) do
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
      "X-Auth-CouchDB-Roles": "test"
    ]

    resp = Couch.get("/#{db_name}/_design/test/_show/welcome", headers: headers)
    assert resp.body == "Welcome couch@apache.org"

    resp = Couch.get("/#{db_name}/_design/test/_show/role", headers: headers)
    assert resp.body == "test"
  end
end

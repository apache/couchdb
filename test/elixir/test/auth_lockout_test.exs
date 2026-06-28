defmodule AuthLockoutTest do
  use CouchTestCase

  @moduletag :authentication

  @moduletag config: [
               {
                 "admins",
                 "chttpd_auth_lockout",
                 "bar"
               }
             ]

  test "lockout after multiple failed authentications", _context do
    server_config = [
      %{
        :section => "chttpd_auth_lockout",
        :key => "mode",
        :value => "enforce"
      }
    ]

    run_on_modified_server(server_config, &test_chttpd_auth_lockout_enforcement/0)
  end

  test "do not lockout after multiple failed authentications", _context do
    server_config = [
      %{
        :section => "chttpd_auth_lockout",
        :key => "mode",
        :value => "warn"
      }
    ]

    run_on_modified_server(server_config, &test_chttpd_auth_lockout_warning/0)
  end

  test "lockout is tracked per client_ip_header, not the shared socket peer", _context do
    server_config = [
      %{
        :section => "chttpd_auth_lockout",
        :key => "mode",
        :value => "enforce"
      },
      %{
        :section => "chttpd_auth_lockout",
        :key => "client_ip_header",
        :value => "X-Forwarded-For"
      }
    ]

    run_on_modified_server(server_config, &test_chttpd_auth_lockout_per_client_ip/0)
  end

  defp test_chttpd_auth_lockout_per_client_ip do
    locked_out_client = "192.0.2.10"
    other_client = "192.0.2.20"

    # exceed the lockout threshold as one forwarded-for client
    for _n <- 1..5 do
      assert auth_failure(locked_out_client).status_code == 401
    end

    # that client is now locked out, even though every request shares the
    # same socket peer (127.0.0.1)
    assert auth_failure(locked_out_client).status_code == 403

    # a different forwarded-for client is unaffected: it still gets a plain
    # 401, proving the lockout keys on the header and not the socket peer
    assert auth_failure(other_client).status_code == 401
  end

  defp auth_failure(forwarded_for) do
    Couch.get("/_all_dbs",
      no_auth: true,
      headers: [
        "X-Forwarded-For": forwarded_for,
        authorization: "Basic #{:base64.encode("chttpd_auth_lockout:baz")}"
      ]
    )
  end

  defp test_chttpd_auth_lockout_enforcement do
    # exceed the lockout threshold
    for _n <- 1..5 do
      resp = Couch.get("/_all_dbs",
        no_auth: true,
        headers: [authorization: "Basic #{:base64.encode("chttpd_auth_lockout:baz")}"]
      )

      assert resp.status_code == 401
    end

    # locked out?
    resp = Couch.get("/_all_dbs",
      no_auth: true,
      headers: [authorization: "Basic #{:base64.encode("chttpd_auth_lockout:baz")}"]
    )

    assert resp.status_code == 403
    assert resp.body["reason"] == "Account is temporarily locked due to multiple authentication failures"
  end

  defp test_chttpd_auth_lockout_warning do
    # exceed the lockout threshold
    for _n <- 1..6 do
      resp = Couch.get("/_all_dbs",
        no_auth: true,
        headers: [authorization: "Basic #{:base64.encode("chttpd_auth_lockout:baz")}"]
      )

      assert resp.status_code == 401
    end
  end
end

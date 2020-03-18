defmodule JwtAuthTest do
  use CouchTestCase

  @moduletag :authentication

  test "jwt auth with secret", _context do

    secret = "zxczxc12zxczxc12"

    server_config = [
      %{
        :section => "jwt_auth",
        :key => "secret",
        :value => secret
      }
    ]

    run_on_modified_server(server_config, fn ->
      test_fun()
    end)
  end

  def test_fun() do
    resp = Couch.get("/_session",
      headers: [authorization: "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJjb3VjaEBhcGFjaGUub3JnIn0.KYHmGXWj0HNHzZCjfOfsIfZWdguEBSn31jUdDUA9118"]
    )

    assert resp.body["userCtx"]["name"] == "couch@apache.org"
    assert resp.body["info"]["authenticated"] == "jwt"
  end

  test "jwt auth without secret", _context do

    server_config = [
    ]

    run_on_modified_server(server_config, fn ->
      test_fun_no_secret()
    end)
  end

  def test_fun_no_secret() do
    resp = Couch.get("/_session")

    assert resp.body["userCtx"]["name"] == "adm"
    assert resp.body["info"]["authenticated"] == "default"
  end
end

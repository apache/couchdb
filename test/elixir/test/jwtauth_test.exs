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
      headers: [authorization: "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJjb3VjaEBhcGFjaGUub3JnIiwicm9sZXMiOiJfYWRtaW4ifQ.hP_nxaHADCkx5cNzHGQUFm2j0OEbtYvL7c1fEdmBQSU"]
    )

    assert resp.body["userCtx"]["name"] == "couch@apache.org"
    assert resp.body["userCtx"]["roles"] == [<<"_admin">>]
    assert resp.body["info"]["authenticated"] == "jwt"
  end

  test "jwt auth without secret", _context do

    resp = Couch.get("/_session")

    assert resp.body["userCtx"]["name"] == "adm"
    assert resp.body["info"]["authenticated"] == "default"
  end
end

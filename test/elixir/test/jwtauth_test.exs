defmodule JwtAuthTest do
  use CouchTestCase

  @moduletag :authentication

  test "jwt auth with HMAC secret", _context do

    secret = "zxczxc12zxczxc12"

    server_config = [
      %{
        :section => "jwt_auth",
        :key => "secret",
        :value => secret
      },
      %{
        :section => "jwt_auth",
        :key => "allowed_algorithms",
        :value => "HS256, HS384, HS512"
      }
    ]

    run_on_modified_server(server_config, fn -> test_fun("HS256", secret) end)
    run_on_modified_server(server_config, fn -> test_fun("HS384", secret) end)
    run_on_modified_server(server_config, fn -> test_fun("HS512", secret) end)
  end

  def test_fun(alg, key) do
    {:ok, token} = :jwtf.encode({[{"alg", alg}, {"typ", "JWT"}]}, {[{"sub", "couch@apache.org"}]}, key)
    resp = Couch.get("/_session",
      headers: [authorization: "Bearer #{token}"]
    )

    assert resp.body["userCtx"]["name"] == "couch@apache.org"
    assert resp.body["info"]["authenticated"] == "jwt"
  end

  test "jwt auth without secret", _context do

    resp = Couch.get("/_session")

    assert resp.body["userCtx"]["name"] == "adm"
    assert resp.body["info"]["authenticated"] == "default"
  end
end

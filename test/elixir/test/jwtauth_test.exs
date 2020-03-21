defmodule JwtAuthTest do
  use CouchTestCase

  @moduletag :authentication

  test "jwt auth with secret and roles", _context do

    secret = "zxczxc12zxczxc12"

    server_config = [
      %{
        :section => "jwt_auth",
        :key => "secret",
        :value => secret
      }
    ]

    run_on_modified_server(server_config, fn ->
      resp = Couch.get("/_session",
        headers: [authorization: "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJjb3VjaEBhcGFjaGUub3JnIiwicm9sZXMiOlsiX2FkbWluIl19.JF_vunmn95R-XxQK1UtYhlKdLJdg4ms3sBeZRxpXgkw"]
      )

      assert resp.body["userCtx"]["name"] == "couch@apache.org"
      assert resp.body["userCtx"]["roles"] == [<<"_admin">>]
      assert resp.body["info"]["authenticated"] == "jwt"
    end)
  end

  test "jwt auth with secret and without roles", _context do

    secret = "zxczxc12zxczxc12"

    server_config = [
      %{
        :section => "jwt_auth",
        :key => "secret",
        :value => secret
      }
    ]

    run_on_modified_server(server_config, fn ->
      run_on_modified_server(server_config, fn ->
        resp = Couch.get("/_session",
          headers: [authorization: "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJjb3VjaEBhcGFjaGUub3JnIn0.KYHmGXWj0HNHzZCjfOfsIfZWdguEBSn31jUdDUA9118"]
        )
  
        assert resp.body["userCtx"]["name"] == "couch@apache.org"
        assert resp.body["userCtx"]["roles"] == []
        assert resp.body["info"]["authenticated"] == "jwt"
      end)
    end)
  end

  def test_fun() do

  end

  test "jwt auth without secret", _context do

    resp = Couch.get("/_session")

    assert resp.body["userCtx"]["name"] == "adm"
    assert resp.body["info"]["authenticated"] == "default"
  end
end

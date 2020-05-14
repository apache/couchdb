defmodule JwtAuthTest do
  use CouchTestCase

  @moduletag :authentication

  test "jwt auth with HMAC secret", _context do

    secret = "zxczxc12zxczxc12"

    server_config = [
      %{
        :section => "jwt_keys",
        :key => "hmac:_default",
        :value => :base64.encode(secret)
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

  defmodule RSA do
    require Record
    Record.defrecord :public, :RSAPublicKey,
      Record.extract(:RSAPublicKey, from_lib: "public_key/include/public_key.hrl")
    Record.defrecord :private, :RSAPrivateKey,
      Record.extract(:RSAPrivateKey, from_lib: "public_key/include/public_key.hrl")
  end

  test "jwt auth with RSA secret", _context do
    require JwtAuthTest.RSA

    private_key = :public_key.generate_key({:rsa, 2048, 17})
    public_key = RSA.public(
      modulus: RSA.private(private_key, :modulus),
      publicExponent: RSA.private(private_key, :publicExponent))

    public_pem = :public_key.pem_encode(
      [:public_key.pem_entry_encode(
          :SubjectPublicKeyInfo, public_key)])
    public_pem = String.replace(public_pem, "\n", "\\n")

    server_config = [
      %{
        :section => "jwt_keys",
        :key => "rsa:_default",
        :value => public_pem
      },
      %{
        :section => "jwt_auth",
        :key => "allowed_algorithms",
        :value => "RS256, RS384, RS512"
      }
    ]

    run_on_modified_server(server_config, fn -> test_fun("RS256", private_key) end)
    run_on_modified_server(server_config, fn -> test_fun("RS384", private_key) end)
    run_on_modified_server(server_config, fn -> test_fun("RS512", private_key) end)
  end

  defmodule EC do
    require Record
    Record.defrecord :point, :ECPoint,
      Record.extract(:ECPoint, from_lib: "public_key/include/public_key.hrl")
    Record.defrecord :private, :ECPrivateKey,
      Record.extract(:ECPrivateKey, from_lib: "public_key/include/public_key.hrl")
  end

  test "jwt auth with EC secret", _context do
    require JwtAuthTest.EC

    private_key = :public_key.generate_key({:namedCurve, :secp384r1})
    point = EC.point(point: EC.private(private_key, :publicKey))
    public_key = {point, EC.private(private_key, :parameters)}

    public_pem = :public_key.pem_encode(
      [:public_key.pem_entry_encode(
          :SubjectPublicKeyInfo, public_key)])
    public_pem = String.replace(public_pem, "\n", "\\n")

    server_config = [
      %{
        :section => "jwt_keys",
        :key => "ec:_default",
        :value => public_pem
      },
      %{
        :section => "jwt_auth",
        :key => "allowed_algorithms",
        :value => "ES256, ES384, ES512"
      }
    ]

    run_on_modified_server(server_config, fn -> test_fun("ES256", private_key) end)
    run_on_modified_server(server_config, fn -> test_fun("ES384", private_key) end)
    run_on_modified_server(server_config, fn -> test_fun("ES512", private_key) end)
  end

  def test_fun(alg, key) do
    now = DateTime.to_unix(DateTime.utc_now())
    {:ok, token} = :jwtf.encode(
      {
        [
          {"alg", alg},
          {"typ", "JWT"}
        ]
      },
      {
        [
          {"nbf", now - 60},
          {"exp", now + 60},
          {"sub", "couch@apache.org"},
          {"_couchdb.roles", ["testing"]
          }
        ]
      }, key)

    resp = Couch.get("/_session",
      headers: [authorization: "Bearer #{token}"]
    )

    assert resp.body["userCtx"]["name"] == "couch@apache.org"
    assert resp.body["userCtx"]["roles"] == ["testing"]
    assert resp.body["info"]["authenticated"] == "jwt"
  end

  test "jwt auth without secret", _context do

    resp = Couch.get("/_session")

    assert resp.body["userCtx"]["name"] == "adm"
    assert resp.body["info"]["authenticated"] == "default"
  end

  test "jwt auth with required iss claim", _context do

    secret = "zxczxc12zxczxc12"

    server_config = [
      %{
        :section => "jwt_auth",
        :key => "required_claims",
        :value => "{iss, \"hello\"}"
      },
      %{
        :section => "jwt_keys",
        :key => "hmac:_default",
        :value => :base64.encode(secret)
      },
      %{
        :section => "jwt_auth",
        :key => "allowed_algorithms",
        :value => "HS256, HS384, HS512"
      }
    ]

    run_on_modified_server(server_config, fn -> good_iss("HS256", secret) end)
    run_on_modified_server(server_config, fn -> bad_iss("HS256", secret) end)
  end

  def good_iss(alg, key) do
    {:ok, token} = :jwtf.encode(
      {
        [
          {"alg", alg},
          {"typ", "JWT"}
        ]
      },
      {
        [
          {"iss", "hello"},
          {"sub", "couch@apache.org"},
          {"_couchdb.roles", ["testing"]
          }
        ]
      }, key)

    resp = Couch.get("/_session",
      headers: [authorization: "Bearer #{token}"]
    )

    assert resp.body["userCtx"]["name"] == "couch@apache.org"
    assert resp.body["userCtx"]["roles"] == ["testing"]
    assert resp.body["info"]["authenticated"] == "jwt"
  end

  def bad_iss(alg, key) do
    {:ok, token} = :jwtf.encode(
      {
        [
          {"alg", alg},
          {"typ", "JWT"}
        ]
      },
      {
        [
          {"iss", "goodbye"},
          {"sub", "couch@apache.org"},
          {"_couchdb.roles", ["testing"]
          }
        ]
      }, key)

    resp = Couch.get("/_session",
      headers: [authorization: "Bearer #{token}"]
    )

    assert resp.status_code == 400
  end

end

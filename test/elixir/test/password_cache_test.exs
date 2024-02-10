defmodule PasswordCacheTest do
  use CouchTestCase

  @moduletag :authentication

  @users_db "_users"

  @moduletag config: [
    {
      "chttpd_auth",
      "authentication_db",
      @users_db
    }
  ]

  setup do
    reset_db(@users_db)
    wait_for_design_auth(@users_db)
    on_exit(&tear_down/0)
  end

  defp tear_down do
    reset_db(@users_db)
  end

  defp fast() do
    hits = request_stats(["couchdb", "password_hashing_fast"], true)
    hits["value"] || 0
  end

  defp slow() do
    misses = request_stats(["couchdb", "password_hashing_slow"], true)
    misses["value"] || 0
  end

  defp logout(session) do
    assert Couch.Session.logout(session).body["ok"]
  end

  defp login_fail(user, password) do
    resp = Couch.login(user, password, :fail)
    assert resp.error, "Login error is expected."
  end

  defp login(user, password) do
    sess = Couch.login(user, password)
    assert sess.cookie, "Login correct is expected"
    sess
  end

  defp assert_cache(event, user, password, expect \\ :expect_login_success) do
    slow_before = slow()
    fast_before = fast()

    session =
      case expect do
        :expect_login_success -> login(user, password)
        :expect_login_fail -> login_fail(user, password)
        _ -> assert false
      end

    slow_after = slow()
    fast_after = fast() - 3 # ignore the 3 fast bumps for previous _stats calls

    if expect == :expect_success do
      logout(session)
    end

    case event do
      :expect_fast ->
        assert fast_after == fast_before + 1,
          "Fast hash is expected for #{user} during login"

        assert slow_after == slow_before,
          "No slow hash is expected for #{user} during login"

      :expect_slow ->
        assert slow_after == slow_before + 1,
        "Slow hash is expected for #{user} during login"

        case expect do
          :expect_login_success ->
            assert fast_after == fast_before + 1,
              "Fast hash is expected after successful auth after slow hash for #{user} during login"
          :expect_login_fail ->
            assert fast_after == fast_before
            assert fast_after == fast_before,
              "No fast hash is expected after unsuccessful auth after slow hash for #{user} during login"
          _ ->
            assert false
          end
      _ ->
        assert false
    end
  end

  def save_doc(db_name, body) do
    resp = Couch.put("/#{db_name}/#{body["_id"]}", body: body)
    assert resp.status_code in [201, 202]
    assert resp.body["ok"]
    Map.put(body, "_rev", resp.body["rev"])
  end

  def make_user(db_name, user_name) do
    result = prepare_user_doc([
      {:name, user_name},
      {:password, user_name}
    ])
    {:ok, resp} = create_doc(db_name, result)
    Map.put(result, "_rev", resp.body["rev"])
  end

  test "password hash cache" do
    user1 = make_user(@users_db, "user1")

    # cache misses mean slow auth
    assert_cache(:expect_slow, "user1", "wrong_password", :expect_login_fail)
    assert_cache(:expect_slow, "user1", "wrong_password", :expect_login_fail)

    # last slow auth that populates the cache
    assert_cache(:expect_slow, "user1", "user1")

    # Fast while cached
    assert_cache(:expect_fast, "user1", "user1")
    assert_cache(:expect_fast, "user1", "user1")

    # change password
    user1 = Map.replace!(user1, "password", "new_password")
    save_doc(@users_db, user1)

    # Slow rejection for wrong password
    assert_cache(:expect_slow, "user1", "wrong_password", :expect_login_fail)

    # Slow acceptance for new, uncached password
    assert_cache(:expect_slow, "user1", "new_password")
  end
end

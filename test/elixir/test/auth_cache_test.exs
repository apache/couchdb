defmodule AuthCacheTest do
  use CouchTestCase

  @moduletag :authentication
  @moduletag kind: :single_node

  @tag :pending
  @tag :with_db
  test "auth cache management", context do
    db_name = context[:db_name]

    server_config = [
      %{
        :section => "chttpd_auth",
        :key => "authentication_db",
        :value => db_name
      },
      %{
        :section => "chttpd_auth",
        :key => "auth_cache_size",
        :value => "3"
      },
      %{
        :section => "httpd",
        :key => "authentication_handlers",
        :value => "{couch_httpd_auth, default_authentication_handler}"
      },
      %{
        :section => "chttpd_auth",
        :key => "secret",
        :value => generate_secret(64)
      }
    ]

    run_on_modified_server(server_config, fn -> test_fun(db_name) end)
  end

  defp generate_secret(len) do
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    |> String.splitter("", trim: true)
    |> Enum.take_random(len)
    |> Enum.join("")
  end

  defp hits() do
    hits = request_stats(["couchdb", "auth_cache_hits"], true)
    hits["value"] || 0
  end

  defp misses() do
    misses = request_stats(["couchdb", "auth_cache_misses"], true)
    misses["value"] || 0
  end

  defp logout(session) do
    assert Couch.Session.logout(session).body["ok"]
  end

  defp login_fail(user, password) do
    resp = Couch.login(user, password, expect: :fail)
    assert resp.error, "Login error is expected."
  end

  defp login(user, password) do
    sess = Couch.login(user, password)
    assert sess.cookie, "Login correct is expected"
    sess
  end

  defp assert_cache(event, user, password, expect \\ :expect_login_success) do
    hits_before = hits()
    misses_before = misses()

    session =
      case expect do
        :expect_login_success -> login(user, password)
        :expect_login_fail -> login_fail(user, password)
        _ -> assert false
      end

    hits_after = hits()
    misses_after = misses()

    if expect == :expect_success do
      logout(session)
    end

    case event do
      :expect_miss ->
        assert misses_after == misses_before + 1,
               "Cache miss is expected for #{user} after login"

        assert hits_after == hits_before,
               "No cache hit is expected for #{user} after login"

      :expect_hit ->
        assert misses_after == misses_before,
               "No cache miss is expected for #{user} after login"

        assert hits_after == hits_before + 1,
               "Cache hit is expected for #{user} after login"

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

  def delete_doc(db_name, body) do
    resp = Couch.delete("/#{db_name}/#{body["_id"]}", query: [rev: body["_rev"]])
    assert resp.status_code in [200, 202]
    assert resp.body["ok"]
    {:ok, resp}
  end

  defp test_fun(db_name) do
    fdmanana =
      prepare_user_doc([
        {:name, "fdmanana"},
        {:password, "qwerty"},
        {:roles, ["dev"]}
      ])

    {:ok, resp} = create_doc(db_name, fdmanana)
    fdmanana = Map.put(fdmanana, "_rev", resp.body["rev"])

    chris =
      prepare_user_doc([
        {:name, "chris"},
        {:password, "the_god_father"},
        {:roles, ["dev", "mafia", "white_costume"]}
      ])

    create_doc(db_name, chris)

    joe =
      prepare_user_doc([
        {:name, "joe"},
        {:password, "functional"},
        {:roles, ["erlnager"]}
      ])

    create_doc(db_name, joe)

    johndoe =
      prepare_user_doc([
        {:name, "johndoe"},
        {:password, "123456"},
        {:roles, ["user"]}
      ])

    create_doc(db_name, johndoe)

    assert_cache(:expect_miss, "fdmanana", "qwerty")
    assert_cache(:expect_hit, "fdmanana", "qwerty")
    assert_cache(:expect_miss, "chris", "the_god_father")
    assert_cache(:expect_miss, "joe", "functional")
    assert_cache(:expect_miss, "johndoe", "123456")

    # It's a MRU cache, joe was removed from cache to add johndoe
    # BUGGED assert_cache(:expect_miss, "joe", "functional")

    assert_cache(:expect_hit, "fdmanana", "qwerty")

    fdmanana = Map.replace!(fdmanana, "password", "foobar")
    fdmanana = save_doc(db_name, fdmanana)

    # Cache was refreshed
    # BUGGED
    # assert_cache(:expect_hit, "fdmanana", "qwerty", :expect_login_fail)
    # assert_cache(:expect_hit, "fdmanana", "foobar")

    # and yet another update
    fdmanana = Map.replace!(fdmanana, "password", "javascript")
    fdmanana = save_doc(db_name, fdmanana)

    # Cache was refreshed
    # BUGGED
    # assert_cache(:expect_hit, "fdmanana", "foobar", :expect_login_fail)
    # assert_cache(:expect_hit, "fdmanana", "javascript")

    delete_doc(db_name, fdmanana)

    assert_cache(:expect_hit, "fdmanana", "javascript", :expect_login_fail)

    # login, compact authentication DB, login again and verify that
    # there was a cache hit
    assert_cache(:expect_hit, "johndoe", "123456")
    compact(db_name)
    assert_cache(:expect_hit, "johndoe", "123456")
  end
end

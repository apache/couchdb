defmodule CookieAuthTest do
  use CouchTestCase

  @moduletag :authentication
  @moduletag kind: :single_node

  @users_db "_users"

  @moduletag config: [
               {
                 "chttpd_auth",
                 "authentication_db",
                 @users_db
               },
               {
                 "couch_httpd_auth",
                 "authentication_db",
                 @users_db
               },
               {
                 "couch_httpd_auth",
                 "iterations",
                 "1"
               },
               {
                 "admins",
                 "jan",
                 "apple"
               }
             ]

  @password "3.141592653589"

  setup do
    # Create db if not exists
    Couch.put("/#{@users_db}")

    retry_until(fn ->
      resp =
        Couch.get(
          "/#{@users_db}/_changes",
          query: [feed: "longpoll", timeout: 5000, filter: "_design"]
        )
        length(resp.body["results"]) > 0
    end)

    on_exit(&tear_down/0)

    :ok
  end

  defp tear_down do
    # delete users
    user = URI.encode("org.couchdb.user:jchris")
    user_doc = Couch.get("/#{@users_db}/#{URI.encode(user)}").body
    Couch.delete("/#{@users_db}/#{user}", query: [rev: user_doc["_rev"]])

    user = URI.encode("org.couchdb.user:Jason Davies")
    user_doc = Couch.get("/#{@users_db}/#{user}").body
    Couch.delete("/#{@users_db}/#{user}", query: [rev: user_doc["_rev"]])
  end

  defp login(user, password) do
    sess = Couch.login(user, password)
    assert sess.cookie, "Login correct is expected"
    sess
  end

  defp logout(session) do
    assert Couch.Session.logout(session).body["ok"]
  end

  defp login_as(user) do
    pws = %{
      "jan" => "apple",
      "Jason Davies" => @password,
      "jchris" => "funnybone"
    }

    user1 = Regex.replace(~r/[0-9]$/, user, "")
    login(user1, pws[user])
  end

  defp create_doc_expect_error(db_name, doc, status_code, msg) do
    resp = Couch.post("/#{db_name}", body: doc)
    assert resp.status_code == status_code
    assert resp.body["error"] == msg
    resp
  end

  defp open_as(db_name, doc_id, options) do
    use_session = Keyword.get(options, :use_session)
    user = Keyword.get(options, :user)
    expect_response = Keyword.get(options, :expect_response, 200)
    expect_message = Keyword.get(options, :error_message)

    session = use_session || login_as(user)

    resp =
      Couch.Session.get(
        session,
        "/#{db_name}/#{URI.encode(doc_id)}"
      )

    if use_session == nil do
      logout(session)
    end

    assert resp.status_code == expect_response

    if expect_message != nil do
      assert resp.body["error"] == expect_message
    end

    resp.body
  end

  defp save_as(db_name, doc, options) do
    use_session = Keyword.get(options, :use_session)
    user = Keyword.get(options, :user)
    expect_response = Keyword.get(options, :expect_response, [201, 202])
    expect_message = Keyword.get(options, :error_message)

    session = use_session || login_as(user)

    resp =
      Couch.Session.put(
        session,
        "/#{db_name}/#{URI.encode(doc["_id"])}",
        body: doc
      )

    if use_session == nil do
      logout(session)
    end

    if is_list(expect_response) do
      assert resp.status_code in expect_response
    else
      assert resp.status_code == expect_response
    end

    if expect_message != nil do
      assert resp.body["error"] == expect_message
    end

    resp
  end

  defp delete_as(db_name, doc, options) do
    use_session = Keyword.get(options, :use_session)
    user = Keyword.get(options, :user)
    expect_response = Keyword.get(options, :expect_response, [200, 202])
    expect_message = Keyword.get(options, :error_message)

    session = use_session || login_as(user)

    resp =
      Couch.Session.delete(
        session,
        "/#{db_name}/#{URI.encode(doc["_id"])}"
      )

    if use_session == nil do
      logout(session)
    end

    if is_list(expect_response) do
      assert resp.status_code in expect_response
    else
      assert resp.status_code == expect_response
    end

    if expect_message != nil do
      assert resp.body["error"] == expect_message
    end

    resp
  end

  defp test_change_admin_fun do
    sess = login("jchris", "funnybone")
    info = Couch.Session.info(sess)
    assert info["userCtx"]["name"] == "jchris"
    assert Enum.member?(info["userCtx"]["roles"], "_admin")
    assert Enum.member?(info["userCtx"]["roles"], "foo")

    jchris_user_doc =
      open_as(
        @users_db,
        "org.couchdb.user:jchris",
        use_session: sess
      )

    jchris_user_doc = Map.drop(jchris_user_doc, [:salt, :password_sha])
    save_as(@users_db, jchris_user_doc, use_session: sess)
    logout(sess)
    sess = login("jchris", "funnybone")
    info = Couch.Session.info(sess)
    assert info["userCtx"]["name"] == "jchris"
    assert Enum.member?(info["userCtx"]["roles"], "_admin")
    assert info["info"]["authenticated"] == "cookie"
    assert info["info"]["authentication_db"] == @users_db
    assert Enum.member?(info["userCtx"]["roles"], "foo")
    logout(sess)
  end

  test "cookie auth" do
    # test that the users db is born with the auth ddoc
    ddoc = open_as(@users_db, "_design/_auth", user: "jan")
    assert ddoc["validate_doc_update"] != nil

    jason_user_doc =
      prepare_user_doc([
        {:name, "Jason Davies"},
        {:password, @password}
      ])

    create_doc(@users_db, jason_user_doc)
    jason_check_doc = open_as(@users_db, jason_user_doc["_id"], user: "jan")
    assert jason_check_doc["name"] == "Jason Davies"

    jchris_user_doc =
      prepare_user_doc([
        {:name, "jchris"},
        {:password, "funnybone"}
      ])

    {:ok, resp} = create_doc(@users_db, jchris_user_doc)
    jchris_rev = resp.body["rev"]

    duplicate_jchris_user_doc =
      prepare_user_doc([
        {:name, "jchris"},
        {:password, "eh, Boo-Boo?"}
      ])

    # make sure we cant create duplicate users
    create_doc_expect_error(@users_db, duplicate_jchris_user_doc, 409, "conflict")

    # we can't create _names
    underscore_user_doc =
      prepare_user_doc([
        {:name, "_why"},
        {:password, "copperfield"}
      ])

    create_doc_expect_error(@users_db, underscore_user_doc, 403, "forbidden")

    # we can't create malformed ids
    bad_id_user_doc =
      prepare_user_doc([
        {:id, "org.apache.couchdb:w00x"},
        {:name, "w00x"},
        {:password, "bar"}
      ])

    create_doc_expect_error(@users_db, bad_id_user_doc, 403, "forbidden")

    # login works
    session = login_as("Jason Davies")
    info = Couch.Session.info(session)
    assert info["userCtx"]["name"] == "Jason Davies"
    assert not Enum.member?(info["userCtx"]["roles"], "_admin")

    # update one's own credentials document
    jason_user_doc =
      jason_user_doc
      |> Map.put("_rev", jason_check_doc["_rev"])
      |> Map.put("foo", 2)

    resp = save_as(@users_db, jason_user_doc, use_session: session)
    jason_user_doc_rev = resp.body["rev"]

    # can't delete another users doc unless you are admin

    jchris_user_doc = Map.put(jchris_user_doc, "_rev", jchris_rev)

    delete_as(
      @users_db,
      jchris_user_doc,
      use_session: session,
      expect_response: 404,
      error_message: "not_found"
    )

    logout(session)

    # test redirect on success
    resp =
      Couch.post(
        "/_session",
        query: [next: "/_up"],
        body: %{
          :username => "Jason Davies",
          :password => @password
        }
      )

    assert resp.status_code == 302
    assert resp.body["ok"]
    assert String.ends_with?(resp.headers["location"], "/_up")

    # test redirect on fail
    resp =
      Couch.post(
        "/_session",
        query: [fail: "/_up"],
        body: %{
          :username => "Jason Davies",
          :password => "foobar"
        }
      )

    assert resp.status_code == 302
    assert resp.body["error"] == "unauthorized"
    assert String.ends_with?(resp.headers["location"], "/_up")

    session = login("jchris", "funnybone")
    info = Couch.Session.info(session)
    assert info["userCtx"]["name"] == "jchris"
    assert Enum.empty?(info["userCtx"]["roles"])

    jason_user_doc =
      jason_user_doc
      |> Map.put("_rev", jason_user_doc_rev)
      |> Map.put("foo", 3)

    save_as(
      @users_db,
      jason_user_doc,
      use_session: session,
      expect_response: 404,
      error_message: "not_found"
    )

    jchris_user_doc = Map.put(jchris_user_doc, "roles", ["foo"])

    save_as(
      @users_db,
      jchris_user_doc,
      use_session: session,
      expect_response: 403,
      error_message: "forbidden"
    )

    logout(session)

    jchris_user_doc = Map.put(jchris_user_doc, "foo", ["foo"])

    resp =
      save_as(
        @users_db,
        jchris_user_doc,
        user: "jan"
      )

    # test that you can't save system (underscore) roles even if you are admin
    jchris_user_doc =
      jchris_user_doc
      |> Map.put("roles", ["_bar"])
      |> Map.put("_rev", resp.body["rev"])

    save_as(
      @users_db,
      jchris_user_doc,
      user: "jan",
      expect_response: 403,
      error_message: "forbidden"
    )

    session = login("jchris", "funnybone")
    info = Couch.Session.info(session)

    assert not Enum.member?(info["userCtx"]["roles"], "_admin")
    assert(Enum.member?(info["userCtx"]["roles"], "foo"))

    logout(session)

    login("jan", "apple")

    run_on_modified_server(
      [
        %{
          :section => "admins",
          :key => "jchris",
          :value => "funnybone"
        }
      ],
      &test_change_admin_fun/0
    )

    # log in one last time so run_on_modified_server can clean up the admin account
    login("jan", "apple")
  end
end

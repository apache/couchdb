defmodule UsersDbTest do
  use CouchTestCase

  @moduletag :authentication

  @users_db_name "_users"

  @server_config [
    %{
      :section => "chttpd_auth",
      :key => "authentication_db",
      :value => @users_db_name
    },
    %{
      :section => "couch_httpd_auth",
      :key => "authentication_db",
      :value => @users_db_name
    },
    %{
      :section => "couch_httpd_auth",
      :key => "iterations",
      :value => "1"
    },
    %{
      :section => "admins",
      :key => "jan",
      :value => "apple"
    }
  ]

  @tag :with_db
  test "test users db", context do
    db_name = context[:db_name]

    # Create db if not exists
    Couch.put("/#{@users_db_name}")

    resp =
      Couch.get(
        "/#{@users_db_name}/_changes",
        query: [feed: "longpoll", timeout: 5000, filter: "_design"]
      )

    assert resp.body
    run_on_modified_server(@server_config, fn -> test_fun(db_name) end)
  end

  defp replicate(source, target, rep_options \\ []) do
    headers = Keyword.get(rep_options, :headers, [])
    body = Keyword.get(rep_options, :body, %{})

    body =
      body
      |> Map.put("source", source)
      |> Map.put("target", target)

    Couch.post("/_replicate", headers: headers, body: body)
  end

  defp save_as(db_name, doc, options) do
    session = Keyword.get(options, :use_session)
    expect_response = Keyword.get(options, :expect_response, 201)
    expect_message = Keyword.get(options, :error_message)
    expect_reason = Keyword.get(options, :error_reason)

    headers =
      if session != nil do
        [
          Cookie: session.cookie,
          "X-CouchDB-www-Authenticate": "Cookie"
        ]
      else
        []
      end

    resp =
      Couch.put(
        "/#{db_name}/#{URI.encode(doc["_id"])}",
        headers: headers,
        body: doc
      )

    assert resp.status_code == expect_response

    if expect_message != nil do
      assert resp.body["error"] == expect_message
    end

    if expect_reason != nil do
      assert resp.body["reason"] == expect_reason
    end

    resp
  end

  defp login(user, password) do
    sess = Couch.login(user, password)
    assert sess.cookie, "Login correct is expected"
    sess
  end

  defp logout(session) do
    assert Couch.Session.logout(session).body["ok"]
  end

  defp test_fun(db_name) do
    # test that the users db is born with the auth ddoc
    ddoc = Couch.get("/#{@users_db_name}/_design/_auth")
    assert ddoc.body["validate_doc_update"] != nil

    jchris_user_doc =
      prepare_user_doc([
        {:name, "jchris@apache.org"},
        {:password, "funnybone"}
      ])

    {:ok, resp} = create_doc(@users_db_name, jchris_user_doc)
    jchris_rev = resp.body["rev"]

    # assert :base64.decode("amNocmlzQGFwYWNoZS5vcmc6ZnVubnlib25l") == nil

    resp =
      Couch.get(
        "/_session",
        headers: [authorization: "Basic #{:base64.encode("jchris@apache.org:funnybone")}"]
      )

    assert resp.body["userCtx"]["name"] == "jchris@apache.org"
    assert resp.body["info"]["authenticated"] == "default"
    assert resp.body["info"]["authentication_db"] == @users_db_name
    assert resp.body["info"]["authentication_handlers"] == ["cookie", "default"]

    resp =
      Couch.get(
        "/_session",
        headers: [authorization: "Basic Xzpf"]
      )

    assert resp.body["userCtx"]["name"] == :null
    assert not Enum.member?(resp.body["info"], "authenticated")

    # ok, now create a conflicting edit on the jchris doc, and make sure there's no login.
    # (use replication to create the conflict) - need 2 be admin
    session = login("jan", "apple")
    replicate(@users_db_name, db_name)

    jchris_user_doc = Map.put(jchris_user_doc, "_rev", jchris_rev)

    jchris_user_doc2 = Map.put(jchris_user_doc, "foo", "bar")

    save_as(@users_db_name, jchris_user_doc2, use_session: session)
    save_as(@users_db_name, jchris_user_doc, use_session: session, expect_response: 409)

    # then in the other
    jchris_user_doc3 = Map.put(jchris_user_doc, "foo", "barrrr")
    save_as(db_name, jchris_user_doc3, use_session: session)
    replicate(db_name, @users_db_name)
    # now we should have a conflict

    resp =
      Couch.get(
        "/#{@users_db_name}/#{jchris_user_doc3["_id"]}",
        query: [conflicts: true]
      )

    assert length(resp.body["_conflicts"]) == 1
    jchris_with_conflict = resp.body

    logout(session)

    # wait for auth_cache invalidation
    :timer.sleep(5000)

    resp =
      Couch.get(
        "/_session",
        headers: [authorization: "Basic #{:base64.encode("jchris@apache.org:funnybone")}"]
      )

    assert resp.body["error"] == "unauthorized"
    assert String.contains?(resp.body["reason"], "conflict")

    # You can delete a user doc
    session = login("jan", "apple")
    info = Couch.Session.info(session)
    assert Enum.member?(info["userCtx"]["roles"], "_admin")

    resp =
      Couch.delete(
        "/#{@users_db_name}/#{jchris_with_conflict["_id"]}",
        query: [rev: jchris_with_conflict["_rev"]],
        headers: [
          Cookie: session.cookie,
          "X-CouchDB-www-Authenticate": "Cookie"
        ]
      )

    assert resp.body["ok"]

    # you can't change doc from type "user"
    resp =
      Couch.get(
        "/#{@users_db_name}/#{jchris_user_doc["_id"]}",
        headers: [
          Cookie: session.cookie,
          "X-CouchDB-www-Authenticate": "Cookie"
        ]
      )

    jchris_user_doc = Map.replace!(resp.body, "type", "not user")

    save_as(
      @users_db_name,
      jchris_user_doc,
      use_session: session,
      expect_response: 403,
      error_message: "forbidden",
      error_reason: "doc.type must be user"
    )

    # "roles" must be an array
    jchris_user_doc =
      jchris_user_doc
      |> Map.replace!("type", "user")
      |> Map.replace!("roles", "not an array")

    save_as(
      @users_db_name,
      jchris_user_doc,
      use_session: session,
      expect_response: 403,
      error_message: "forbidden",
      error_reason: "doc.roles must be an array"
    )

    # "roles" must be and array of strings
    jchris_user_doc = Map.replace!(jchris_user_doc, "roles", [12])

    save_as(
      @users_db_name,
      jchris_user_doc,
      use_session: session,
      expect_response: 403,
      error_message: "forbidden",
      error_reason: "doc.roles can only contain strings"
    )

    # "roles" must exist
    jchris_user_doc = Map.drop(jchris_user_doc, ["roles"])

    save_as(
      @users_db_name,
      jchris_user_doc,
      use_session: session,
      expect_response: 403,
      error_message: "forbidden",
      error_reason: "doc.roles must exist"
    )

    # character : is not allowed in usernames
    joe_user_doc =
      prepare_user_doc([
        {:name, "joe:erlang"},
        {:password, "querty"}
      ])

    save_as(
      @users_db_name,
      joe_user_doc,
      use_session: session,
      expect_response: 403,
      error_message: "forbidden",
      error_reason: "Character `:` is not allowed in usernames."
    )

    # test that you can login as a user with a password starting with :
    joe_user_doc =
      prepare_user_doc([
        {:name, "foo@example.org"},
        {:password, ":bar"}
      ])

    {:ok, _} = create_doc(@users_db_name, joe_user_doc)
    logout(session)

    resp =
      Couch.get(
        "/_session",
        headers: [authorization: "Basic #{:base64.encode("foo@example.org::bar")}"]
      )

    assert resp.body["userCtx"]["name"] == "foo@example.org"
  after
    delete_db(@users_db_name)
    create_db(@users_db_name)
  end
end

defmodule UsersDbSecurityTest do
  use CouchTestCase

  @moduletag :authentication
  @moduletag kind: :single_node

  @users_db "_users"

  @login_user %{
    jerry: "apple",
    tom: "mp3",
    spike: "foobar",
    speedy: "test",
    silvestre: "anchovy"
  }

  setup_all do
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
    users = Map.keys(@login_user)
    Enum.each(users, fn name ->
      resp = Couch.get("/#{@users_db}/org.couchdb.user:#{name}")
      if resp.status_code == 200 do
        rev = resp.body["_rev"]
        Couch.delete("/#{@users_db}/org.couchdb.user:#{name}?rev=#{rev}")
      end
    end)
  end

  defp login_as(user, password \\ nil) do
    pwd =
      case password do
        nil -> @login_user[String.to_atom(user)]
        _ -> password
      end

    sess = Couch.login(user, pwd)
    assert sess.cookie, "Login correct is expected"
    sess
  end

  defp logout(session) do
    assert Couch.Session.logout(session).body["ok"]
  end

  defp open_as(db_name, doc_id, options) do
    use_session = Keyword.get(options, :use_session)
    user = Keyword.get(options, :user)
    pwd = Keyword.get(options, :pwd)
    expect_response = Keyword.get(options, :expect_response, 200)
    expect_message = Keyword.get(options, :error_message)

    session = use_session || login_as(user, pwd)

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

  defp view_as(db_name, view_name, options) do
    use_session = Keyword.get(options, :use_session)
    user = Keyword.get(options, :user)
    pwd = Keyword.get(options, :pwd)
    expect_response = Keyword.get(options, :expect_response, 200)
    expect_message = Keyword.get(options, :error_message)

    session = use_session || login_as(user, pwd)

    [view_root, view_name] = String.split(view_name, "/")

    resp =
      Couch.Session.get(session, "/#{db_name}/_design/#{view_root}/_view/#{view_name}")

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

  defp changes_as(db_name, options) do
    use_session = Keyword.get(options, :use_session)
    user = Keyword.get(options, :user)
    expect_response = Keyword.get(options, :expect_response, [200, 202])
    expect_message = Keyword.get(options, :error_message)

    session = use_session || login_as(user)

    resp =
      Couch.Session.get(
        session,
        "/#{db_name}/_changes"
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

  defp request_raw_as(db_name, path, options) do
    use_session = Keyword.get(options, :use_session)
    user = Keyword.get(options, :user)
    pwd = Keyword.get(options, :pwd)
    expect_response = Keyword.get(options, :expect_response, 200)
    expect_message = Keyword.get(options, :error_message)

    session = use_session || login_as(user, pwd)

    resp =
      Couch.Session.get(
        session,
        "/#{db_name}/#{path}",
        parse_response: false
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

  defp request_as(db_name, path, options) do
    use_session = Keyword.get(options, :use_session)
    user = Keyword.get(options, :user)
    pwd = Keyword.get(options, :pwd)
    expect_response = Keyword.get(options, :expect_response, 200)
    expect_message = Keyword.get(options, :error_message)

    session = use_session || login_as(user, pwd)

    resp =
      Couch.Session.get(
        session,
        "/#{db_name}/#{path}"
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

  defp set_security(db_name, security, expect_response \\ 200) do
    resp = Couch.put("/#{db_name}/_security", body: security)
    assert resp.status_code == expect_response
  end

  @tag config: [
         {
           "couchdb",
           "users_db_security_editable",
           "true"
         },
         {
           "chttpd_auth",
           "iterations",
           "1"
         },
         {
           "admins",
           "jerry",
           "apple"
         }
       ]
  test "user db security" do
    # _users db
    # a doc with a field 'password' should be hashed to 'derived_key'
    # with salt and salt stored in 'salt', 'password' is set to null.
    # Exising 'derived_key' and 'salt' fields are overwritten with new values
    # when a non-null 'password' field exists.
    # anonymous should be able to create a user document
    user_doc = %{
      _id: "org.couchdb.user:tom",
      type: "user",
      name: "tom",
      password: "mp3",
      roles: []
    }

    resp =
      Couch.post("/#{@users_db}", body: user_doc, headers: [authorization: "annonymous"])

    assert resp.status_code in [201, 202]
    assert resp.body["ok"]

    user_doc =
      retry_until(fn ->
        user_doc = open_as(@users_db, "org.couchdb.user:tom", user: "tom")
        assert !user_doc["password"]
        assert String.length(user_doc["derived_key"]) == 40
        assert String.length(user_doc["salt"]) == 32
        user_doc
      end)

    # anonymous should not be able to read an existing user's user document
    resp =
      Couch.get("/#{@users_db}/org.couchdb.user:tom",
        headers: [authorization: "annonymous"]
      )

    assert resp.status_code == 404

    # anonymous should not be able to read /_users/_changes
    resp = Couch.get("/#{@users_db}/_changes", headers: [authorization: "annonymous"])
    assert resp.status_code == 401
    assert resp.body["error"] == "unauthorized"

    # user should be able to read their own document
    tom_doc = open_as(@users_db, "org.couchdb.user:tom", user: "tom")
    assert tom_doc["_id"] == "org.couchdb.user:tom"

    # user should not be able to read /_users/_changes
    changes_as(@users_db,
      user: "tom",
      expect_response: 401,
      expect_message: "unauthorized"
    )

    tom_doc = Map.put(tom_doc, "password", "couch")
    save_as(@users_db, tom_doc, user: "tom")

    tom_doc = open_as(@users_db, "org.couchdb.user:tom", user: "jerry")
    assert !tom_doc["password"]
    assert String.length(tom_doc["derived_key"]) == 40
    assert String.length(tom_doc["salt"]) == 32
    assert tom_doc["derived_key"] != user_doc["derived_key"]
    assert tom_doc["salt"] != user_doc["salt"]

    # user should not be able to read another user's user document
    spike_doc = %{
      _id: "org.couchdb.user:spike",
      type: "user",
      name: "spike",
      password: "foobar",
      roles: []
    }

    {:ok, _} = create_doc(@users_db, spike_doc)

    open_as(@users_db, "org.couchdb.user:spike",
      user: "tom",
      pwd: "couch",
      expect_response: 404
    )

    speedy_doc = %{
      _id: "org.couchdb.user:speedy",
      type: "user",
      name: "speedy",
      password: "test",
      roles: ["user_admin"]
    }

    {:ok, _} = create_doc(@users_db, speedy_doc)

    security = %{
      admins: %{
        roles: [],
        names: ["speedy"]
      }
    }

    set_security(@users_db, security)

    # user should not be able to read from any view
    ddoc = %{
      _id: "_design/user_db_auth",
      views: %{
        test: %{
          map: "function(doc) { emit(doc._id, null); }"
        }
      },
      lists: %{
        names: """
        function(head, req) {
        var row; while (row = getRow()) { send(row.key + \"\\n\"); }
        }
        """
      },
      shows: %{
        name: "function(doc, req) { return doc.name; }"
      }
    }

    create_doc(@users_db, ddoc)

    resp =
      Couch.get("/#{@users_db}/_design/user_db_auth/_view/test",
        headers: [authorization: "annonymous"]
      )

    assert resp.body["error"] == "forbidden"

    # admin should be able to read from any view
    resp = view_as(@users_db, "user_db_auth/test", user: "jerry")
    assert resp.body["total_rows"] == 3

    # db admin should be able to read from any view
    resp = view_as(@users_db, "user_db_auth/test", user: "speedy")
    assert resp.body["total_rows"] == 3

    # non-admins can't read design docs
    open_as(@users_db, "_design/user_db_auth",
      user: "tom",
      pwd: "couch",
      expect_response: 403,
      expect_message: "forbidden"
    )

    # admin shold be able to read _list
    result =
      request_raw_as(@users_db, "_design/user_db_auth/_list/names/test", user: "jerry")

    assert result.status_code == 200
    assert length(String.split(result.body, "\n")) == 4

    # non-admins can't read _list
    request_raw_as(@users_db, "_design/user_db_auth/_list/names/test",
      user: "tom",
      pwd: "couch",
      expect_response: 403
    )

    # admin should be able to read _show
    result =
      request_raw_as(@users_db, "_design/user_db_auth/_show/name/org.couchdb.user:tom",
        user: "jerry"
      )

    assert result.status_code == 200
    assert result.body == "tom"

    # non-admin should be able to access own _show
    result =
      request_raw_as(@users_db, "_design/user_db_auth/_show/name/org.couchdb.user:tom",
        user: "tom",
        pwd: "couch"
      )

    assert result.status_code == 200
    assert result.body == "tom"

    # non-admin can't read other's _show
    request_raw_as(@users_db, "_design/user_db_auth/_show/name/org.couchdb.user:jerry",
      user: "tom",
      pwd: "couch",
      expect_response: 404
    )

    # admin should be able to read and edit any user doc
    spike_doc = open_as(@users_db, "org.couchdb.user:spike", user: "jerry")
    spike_doc = Map.put(spike_doc, "password", "mobile")
    save_as(@users_db, spike_doc, user: "jerry")

    # admin should be able to read and edit any user doc
    spike_doc = open_as(@users_db, "org.couchdb.user:spike", user: "jerry")
    spike_doc = Map.put(spike_doc, "password", "mobile1")
    save_as(@users_db, spike_doc, user: "speedy")

    security = %{
      admins: %{
        roles: ["user_admin"],
        names: []
      }
    }

    set_security(@users_db, security)

    # db admin should be able to read and edit any user doc
    spike_doc = open_as(@users_db, "org.couchdb.user:spike", user: "jerry")
    spike_doc = Map.put(spike_doc, "password", "mobile2")
    save_as(@users_db, spike_doc, user: "speedy")

    # ensure creation of old-style docs still works
    silvestre_doc = prepare_user_doc(name: "silvestre", password: "anchovy")

    resp =
      Couch.post("/#{@users_db}",
        body: silvestre_doc,
        headers: [authorization: "annonymous"]
      )

    assert resp.body["ok"]

    run_on_modified_server(
      [
        %{
          :section => "couch_httpd_auth",
          :key => "public_fields",
          :value => "name"
        },
        %{
          :section => "couch_httpd_auth",
          :key => "users_db_public",
          :value => "false"
        }
      ],
      fn ->
        request_as(@users_db, "_all_docs?include_docs=true",
          user: "tom",
          pwd: "couch",
          expect_response: 401,
          expect_message: "unauthorized"
        )

        # COUCHDB-1888 make sure admins always get all fields
        resp = request_as(@users_db, "_all_docs?include_docs=true", user: "jerry")
        rows = resp.body["rows"]
        assert Enum.at(rows, 2)["doc"]["type"] == "user"
      end
    )
  end
end

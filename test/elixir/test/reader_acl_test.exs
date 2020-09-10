defmodule ReaderACLTest do
  use CouchTestCase

  @moduletag :authentication
  @moduletag kind: :single_node

  @users_db_name "custom-users"
  @password "funnybone"

  @moduletag config: [
               {
                 "chttpd_auth",
                 "authentication_db",
                 @users_db_name
               },
               {
                 "couch_httpd_auth",
                 "authentication_db",
                 @users_db_name
               }
             ]
  setup do
    # Create db if not exists
    Couch.put("/#{@users_db_name}")

    # create a user with top-secret-clearance
    user_doc =
      prepare_user_doc([
        {:name, "bond@apache.org"},
        {:password, @password},
        {:roles, ["top-secret"]}
      ])

    {:ok, _} = create_doc(@users_db_name, user_doc)

    # create a user with top-secret-clearance
    user_doc =
      prepare_user_doc([
        {:name, "juanjo@apache.org"},
        {:password, @password}
      ])

    {:ok, _} = create_doc(@users_db_name, user_doc)

    on_exit(&tear_down/0)

    :ok
  end

  defp tear_down do
    delete_db(@users_db_name)
  end

  defp login(user, password) do
    sess = Couch.login(user, password)
    assert sess.cookie, "Login correct is expected"
    sess
  end

  defp logout(session) do
    assert Couch.Session.logout(session).body["ok"]
  end

  defp open_as(db_name, doc_id, options) do
    use_session = Keyword.get(options, :use_session)
    user = Keyword.get(options, :user)
    expect_response = Keyword.get(options, :expect_response, 200)
    expect_message = Keyword.get(options, :error_message)

    session = use_session || login(user, @password)

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

  defp set_security(db_name, security, expect_response \\ 200) do
    resp = Couch.put("/#{db_name}/_security", body: security)
    assert resp.status_code == expect_response
  end

  @tag :with_db
  test "unrestricted db can be read", context do
    db_name = context[:db_name]

    doc = %{_id: "baz", foo: "bar"}
    {:ok, _} = create_doc(db_name, doc)

    # any user can read unrestricted db
    open_as(db_name, "baz", user: "juanjo@apache.org")
    open_as(db_name, "baz", user: "bond@apache.org")
  end

  @tag :with_db
  test "restricted db can be read by authorized users", context do
    db_name = context[:db_name]

    doc = %{_id: "baz", foo: "bar"}
    {:ok, _} = create_doc(db_name, doc)

    security = %{
      members: %{
        roles: ["super-secret-club"],
        names: ["joe", "barb"]
      }
    }

    set_security(db_name, security)

    # can't read it as bond is missing the needed role
    open_as(db_name, "baz", user: "bond@apache.org", expect_response: 403)

    # make anyone with the top-secret role an admin
    # db admins are automatically members
    security = %{
      admins: %{
        roles: ["top-secret"],
        names: []
      },
      members: %{
        roles: ["super-secret-club"],
        names: ["joe", "barb"]
      }
    }

    set_security(db_name, security)

    # db admin can read
    open_as(db_name, "baz", user: "bond@apache.org")

    # admin now adds the top-secret role to the db's members
    # and removes db-admins
    security = %{
      admins: %{
        roles: [],
        names: []
      },
      members: %{
        roles: ["super-secret-club", "top-secret"],
        names: ["joe", "barb"]
      }
    }

    set_security(db_name, security)

    # server _admin can always read
    resp = Couch.get("/#{db_name}/baz")
    assert resp.status_code == 200

    open_as(db_name, "baz", user: "bond@apache.org")
  end

  @tag :with_db
  test "works with readers (backwards compat with 1.0)", context do
    db_name = context[:db_name]

    doc = %{_id: "baz", foo: "bar"}
    {:ok, _} = create_doc(db_name, doc)

    security = %{
      admins: %{
        roles: [],
        names: []
      },
      readers: %{
        roles: ["super-secret-club", "top-secret"],
        names: ["joe", "barb"]
      }
    }

    set_security(db_name, security)
    open_as(db_name, "baz", user: "bond@apache.org")
  end

  @tag :with_db
  test "can't set non string reader names or roles", context do
    db_name = context[:db_name]

    security = %{
      members: %{
        roles: ["super-secret-club", %{"top-secret": "awesome"}],
        names: ["joe", "barb"]
      }
    }

    set_security(db_name, security, 500)

    security = %{
      members: %{
        roles: ["super-secret-club", "top-secret"],
        names: ["joe", 22]
      }
    }

    set_security(db_name, security, 500)

    security = %{
      members: %{
        roles: ["super-secret-club", "top-secret"],
        names: "joe"
      }
    }

    set_security(db_name, security, 500)
  end

  @tag :with_db
  test "members can query views", context do
    db_name = context[:db_name]

    doc = %{_id: "baz", foo: "bar"}
    {:ok, _} = create_doc(db_name, doc)

    security = %{
      admins: %{
        roles: [],
        names: []
      },
      members: %{
        roles: ["super-secret-club", "top-secret"],
        names: ["joe", "barb"]
      }
    }

    set_security(db_name, security)

    view = %{
      _id: "_design/foo",
      views: %{
        bar: %{
          map: "function(doc){emit(null, null)}"
        }
      }
    }

    {:ok, _} = create_doc(db_name, view)

    # members can query views
    open_as(db_name, "_design/foo/_view/bar", user: "bond@apache.org")
  end
end

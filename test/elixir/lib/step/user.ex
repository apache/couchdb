defmodule Couch.Test.Setup.Step.User do
  @moduledoc """
  Step to create user with given list of roles.
  The :server_admin is a special role which is used to put user
  into `admins` section of a config instead of a database.

  Example
    setup
      |> Setup.Step.User.new(:admin, roles: [:server_admin])
    ...
      |> Setup.run
    ...

    user = setup |> Setup.get(:admin) |> Step.User.name()
  """

  alias Couch.Test.Setup
  alias Couch.Test.Utils

  import ExUnit.Callbacks, only: [on_exit: 1]

  defstruct [:roles, :name, :password, :users_db]

  import ExUnit.Assertions, only: [assert: 1, assert: 2]

  import Utils

  @admin {:user_ctx, user_ctx(roles: ["_admin"])}

  def new(setup, id, roles: roles) do
    setup |> Setup.step(id, %__MODULE__{roles: roles || []})
  end

  def setup(_setup, %__MODULE__{roles: roles} = step) do
    users_db = IO.chardata_to_string(
      :config.get("chttpd_auth", "authentication_db", "_users"))
    if not Utils.db_exists?(users_db) do
      on_exit fn ->
        :fabric.delete_db(users_db, [@admin])
      end
      res = :fabric.create_db(users_db, [@admin])
      assert res in [:ok, :accepted], "Cannot create `users` database #{users_db}"
    end

    if :server_admin in roles do
      name = Utils.random_name("admin")
      pass = Utils.random_password()
      :config.set(
        "admins", String.to_charlist(name), String.to_charlist(pass), false)
      %{step |
        name: name,
        password: pass,
        users_db: users_db
      }
    else
      name = Utils.random_name("admin")
      pass = Utils.random_password()
      doc_id = "org.couchdb.user:#{name}"
      user_doc = :couch_doc.from_json_obj(%{
        _id: doc_id,
        name: name,
        type: "user",
        roles: roles,
        password: pass
      })
      res = :fabric.update_doc(users_db, user_doc, [@admin])
      assert res in [:ok, :accepted], "Cannot create user document"
      %{step |
        name: name,
        password: pass,
        users_db: users_db,
        roles: roles
      }
    end
  end

  def teardown(_setup, %__MODULE__{name: name, users_db: users_db, roles: roles} = _step) do
    if :server_admin in roles do
      :config.delete("admins", String.to_charlist(name), false)
    else
      doc_id = "org.couchdb.user:#{name}"
      assert {:ok, doc_info(revs: [rev | _])} = :fabric.get_doc_info(users_db, doc_id, [])
      doc = :couch_doc.from_json_obj(%{
        _id: doc_id,
        _rev: rev,
        _deleted: true
      })
      assert {:ok, _resp} = :fabric.update_doc(users_db, doc, [@admin])
    end
    :ok
  end

  def name(%__MODULE__{name: name}) do
    name
  end
  def password(%__MODULE__{password: pass}) do
    pass
  end
  def credentials(%__MODULE__{name: name, password: pass}) do
    {name, pass}
  end

end

defmodule Couch.Test.Adapter.Fabric do
  @moduledoc "Fabric API testing adapter type"
  defstruct [:connection_str, :session]

  def new() do
    %Couch.Test.Adapter.Fabric{}
  end
end

defimpl Couch.Test.Adapter, for: Couch.Test.Adapter.Fabric do
  import ExUnit.Assertions

  @moduledoc "Implements Fabric API testing adapter"
  def login(adapter, _user, _pass) do
    adapter
  end

  def create_user(adapter, user) do
    user = if user in [nil, ""] do
      Couch.Test.random_name("user")
    end

    user_doc = Couch.Test.Adapter.Shared.format_user_doc(user)
    create_user_from_doc(adapter, user_doc)
  end

  def create_user_from_doc(_adapter, user_doc) do
    doc = :couch_doc.from_json_obj(user_doc)
    assert {:ok, resp} = :fabric.update_doc("_users", doc, [])
    {:ok, resp}
  end

  def create_db(_adapter, db_name, opts \\ []) do
    # TODO opts will be different for every adapter type
    assert :ok = :fabric.create_db(db_name, opts)
    {:ok, %{body: %{"ok" => true}}}
  end

  def delete_db(_adapter, db_name) do
    assert :ok = :fabric.delete_db(db_name)
    {:ok, :ok}
  end

  def create_doc(adapter, db_name, body) do
    update_doc(adapter, db_name, body)
  end

  def update_doc(_adapter, db_name, body) do
    doc = :couch_doc.from_json_obj(body)
    assert {:ok, resp} = :fabric.update_doc(db_name, doc, [])
    {:ok, resp}
  end

  def delete_doc(_adapter, db_name, doc_id, rev) do
    doc = :couch_doc.from_json_obj(%{
      "_id": doc_id,
      "_rev": rev,
      "_deleted": true
    })
    assert {:ok, resp} = :fabric.update_doc(db_name, doc, [])
    {:ok, resp}
  end

  def open_doc(_adapter, db_name, doc_name) do
    assert {:ok, resp} = :fabric.open_doc(db_name, doc_name, [])
    {:ok, resp}
  end

  def bulk_save(_adapter, db_name, docs) do
    docs = docs
      |> Enum.map(&:couch_doc.from_json_obj/1)
    assert {:ok, resp} = :fabric.update_docs(db_name, docs, [])
    {:ok, resp}
  end

  def query(
        adapter,
        db_name,
        map_fun,
        reduce_fun \\ nil,
        options \\ nil,
        keys \\ nil,
        language \\ "javascript"
      ) do
    {view_options, request_options} =
      if options != nil and Map.has_key?(options, :options) do
        {options.options, Map.delete(options, :options)}
      else
        {nil, options}
      end

    ddoc =
      Couch.Test.Adapter.Shared.format_query_ddoc(
        map_fun,
        reduce_fun,
        language,
        view_options
      )

    request_options =
      if keys != nil and is_list(keys) do
        Map.merge(request_options || %{}, %{:keys => :jiffy.encode(keys)})
      else
        request_options
      end

    resp = update_doc(adapter, db_name, ddoc)

    assert resp.status_code == 201

    # TODO transform resp
    resp = :fabric.query_view(db_name, ddoc._id, "view", request_options)

    adapter.delete_doc(db_name, ddoc._id)

    {:ok, resp}
  end

  def set_config(_adapter, section, key, val) do
    :config.set(section, key, String.to_charlist(val), false)
  end
end

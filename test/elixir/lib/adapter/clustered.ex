defmodule Couch.Test.Adapter.Clustered do
  @moduledoc "Clustered API testing adapter type"
  defstruct [:connection_str, :session]
  def new(connection_str) do
    %Couch.Test.Adapter.Clustered{
      connection_str: connection_str
    }
  end
end

defimpl Couch.Test.Adapter, for: Couch.Test.Adapter.Clustered do
  import ExUnit.Assertions
  import Couch.DBTest, only: [
    retry_until: 1,
  ]

  def login(adapter, user, pass) do
    addr = :config.get('chttpd', 'bind_address', '127.0.0.1')
    port = :mochiweb_socket_server.get(:chttpd, :port)
    base_url = "http://#{addr}:#{port}"
    session = Couch.login(base_url, user, pass)
    %{adapter | session: session}
  end

  def create_user(adapter, user) do
    assert adapter.session, "Requires login"
    user = if user in [nil, ""] do
      Couch.Test.random_name("user")
    end
    user_doc = Couch.Test.Adapter.Shared.format_user_doc(user)
    create_user_from_doc(adapter, user_doc)
  end

  def create_user_from_doc(adapter, user_doc) do
      resp = Couch.Session.get(adapter.session, "/_users/#{user_doc["_id"]}")

      user_doc =
        case resp.status_code do
          404 ->
            user_doc

          sc when sc >= 200 and sc < 300 ->
            Map.put(user_doc, "_rev", resp.body["_rev"])
        end

      resp = Couch.Session.post(adapter.session, "/_users", body: user_doc)
      assert HTTPotion.Response.success?(resp)
      assert resp.body["ok"]
      Map.put(user_doc, "_rev", resp.body["rev"])
  end

  def create_db(adapter, db_name, opts \\ []) do
    assert adapter.session, "Requires login"
    retry_until(fn ->
      resp = Couch.Session.put(adapter.session, "/#{db_name}", opts)
      assert resp.status_code in [201, 202]
      assert resp.body == %{"ok" => true}
      {:ok, resp}
    end)
  end

  def delete_db(adapter, db_name) do
    assert adapter.session, "Requires login"
    resp = Couch.Session.delete(adapter.session, "/#{db_name}")
    assert resp.status_code in [200, 202, 404]
    {:ok, resp}
  end

  def create_doc(adapter, db_name, body) do
    assert adapter.session, "Requires login"
    resp = Couch.Session.post(adapter.session, "/#{db_name}", body: body)
    assert resp.status_code in [201, 202]
    assert resp.body["ok"]
    {:ok, resp}
  end

  def open_doc(adapter, db_name, doc_id) do
    assert adapter.session, "Requires login"
    resp = Couch.Session.get(adapter.session, "/#{db_name}/#{doc_id}")
    assert resp.status_code in [200]
    {:ok, resp.body}
  end

  def update_doc(adapter, db_name, body) do
    assert adapter.session, "Requires login"
    resp = Couch.Session.put(adapter.session, "/#{db_name}/#{body._id}", body: body)
    assert resp.status_code in [200]
    {:ok, resp.body}
  end

  def delete_doc(adapter, db_name, doc_id, rev) do
    assert adapter.session, "Requires login"
    resp = Couch.Session.delete(adapter.session, "/#{db_name}/#{doc_id}", %{"rev": rev})
    assert resp.status_code in [200]
    {:ok, resp.body}
  end

  def bulk_save(adapter, db_name, docs) do
    assert adapter.session, "Requires login"
    resp =
      Couch.Session.post(adapter.session,
        "/#{db_name}/_bulk_docs",
        body: %{
          docs: docs
        }
      )

    assert resp.status_code == 201
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
    assert adapter.session, "Requires login"
    {view_options, request_options} =
      if options != nil and Map.has_key?(options, :options) do
        {options.options, Map.delete(options, :options)}
      else
        {nil, options}
      end
    ddoc = Couch.Test.Adapter.Shared.format_query_ddoc(
      map_fun, reduce_fun, language, view_options)

    request_options =
      if keys != nil and is_list(keys) do
        Map.merge(request_options || %{}, %{:keys => :jiffy.encode(keys)})
      else
        request_options
      end

    resp =
      Couch.Session.put(
        adapter.session,
        "/#{db_name}/#{ddoc._id}",
        headers: ["Content-Type": "application/json"],
        body: ddoc
      )

    assert resp.status_code == 201

    resp = Couch.Session.get(adapter.session, "/#{db_name}/#{ddoc._id}/_view/view", query: request_options)
    assert resp.status_code == 200

    Couch.Session.delete(adapter.session, "/#{db_name}/#{ddoc._id}")

    resp.body
  end

  def set_config(adapter, section, key, val) do
    assert adapter.session, "Requires login"
    url = "#{adapter.connection_str}/#{section}/#{key}"
    headers = ["X-Couch-Persist": "false"]
    resp = Couch.Session.put(adapter.session, url, headers: headers, body: :jiffy.encode(val))
    resp.body
  end

end

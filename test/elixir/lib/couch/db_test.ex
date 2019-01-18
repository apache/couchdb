defmodule Couch.DBTest do
  @moduledoc false

  import ExUnit.Callbacks, only: [on_exit: 1]
  import ExUnit.Assertions, only: [assert: 1, assert: 2]

  def set_db_context(context) do
    context =
      case context do
        %{:with_db_name => true} ->
          Map.put(context, :db_name, random_db_name())

        %{:with_db_name => db_name} when is_binary(db_name) ->
          Map.put(context, :db_name, db_name)

        %{:with_random_db => db_name} when is_binary(db_name) ->
          context
          |> Map.put(:db_name, random_db_name(db_name))
          |> Map.put(:with_db, true)

        %{:with_partitioned_db => true} ->
          context
          |> Map.put(:db_name, random_db_name())
          |> Map.put(:query, %{partitioned: true})
          |> Map.put(:with_db, true)

        %{:with_db => true} ->
          Map.put(context, :db_name, random_db_name())

        %{:with_db => db_name} when is_binary(db_name) ->
          Map.put(context, :db_name, db_name)

        _ ->
          context
      end

    if Map.has_key?(context, :with_db) do
      {:ok, _} = create_db(context[:db_name], query: context[:query])
      on_exit(fn -> delete_db(context[:db_name]) end)
    end

    context
  end

  def set_config_context(context) do
    if is_list(context[:config]) do
      Enum.each(context[:config], fn cfg ->
        set_config(cfg)
      end)
    end

    context
  end

  def set_user_context(context) do
    case Map.get(context, :user) do
      nil ->
        context

      user when is_list(user) ->
        user = create_user(user)

        on_exit(fn ->
          query = %{:rev => user["_rev"]}
          resp = Couch.delete("/_users/#{user["_id"]}", query: query)
          assert HTTPotion.Response.success?(resp)
        end)

        context = Map.put(context, :user, user)
        userinfo = user["name"] <> ":" <> user["password"]
        Map.put(context, :userinfo, userinfo)
    end
  end

  def random_db_name do
    random_db_name("random-test-db")
  end

  def random_db_name(prefix) do
    time = :erlang.monotonic_time()
    umi = :erlang.unique_integer([:monotonic])
    "#{prefix}-#{time}-#{umi}"
  end

  def set_config({section, key, value}) do
    existing = set_config_raw(section, key, value)

    on_exit(fn ->
      Enum.each(existing, fn {node, prev_value} ->
        if prev_value != "" do
          url = "/_node/#{node}/_config/#{section}/#{key}"
          headers = ["X-Couch-Persist": "false"]
          body = :jiffy.encode(prev_value)
          resp = Couch.put(url, headers: headers, body: body)
          assert resp.status_code == 200
        else
          url = "/_node/#{node}/_config/#{section}/#{key}"
          headers = ["X-Couch-Persist": "false"]
          resp = Couch.delete(url, headers: headers)
          assert resp.status_code == 200
        end
      end)
    end)
  end

  def set_config_raw(section, key, value) do
    resp = Couch.get("/_membership")

    Enum.map(resp.body["all_nodes"], fn node ->
      url = "/_node/#{node}/_config/#{section}/#{key}"
      headers = ["X-Couch-Persist": "false"]
      body = :jiffy.encode(value)
      resp = Couch.put(url, headers: headers, body: body)
      assert resp.status_code == 200
      {node, resp.body}
    end)
  end

  def create_user(user) do
    required = [:name, :password, :roles]

    Enum.each(required, fn key ->
      assert Keyword.has_key?(user, key), "User missing key: #{key}"
    end)

    name = Keyword.get(user, :name)
    password = Keyword.get(user, :password)
    roles = Keyword.get(user, :roles)

    assert is_binary(name), "User name must be a string"
    assert is_binary(password), "User password must be a string"
    assert is_list(roles), "Roles must be a list of strings"

    Enum.each(roles, fn role ->
      assert is_binary(role), "Roles must be a list of strings"
    end)

    user_doc = %{
      "_id" => "org.couchdb.user:" <> name,
      "type" => "user",
      "name" => name,
      "roles" => roles,
      "password" => password
    }

    resp = Couch.get("/_users/#{user_doc["_id"]}")

    user_doc =
      case resp.status_code do
        404 ->
          user_doc

        sc when sc >= 200 and sc < 300 ->
          Map.put(user_doc, "_rev", resp.body["_rev"])
      end

    resp = Couch.post("/_users", body: user_doc)
    assert HTTPotion.Response.success?(resp)
    assert resp.body["ok"]
    Map.put(user_doc, "_rev", resp.body["rev"])
  end

  def create_db(db_name, opts \\ []) do
    resp = Couch.put("/#{db_name}", opts)
    assert resp.status_code in [201, 202]
    assert resp.body == %{"ok" => true}
    {:ok, resp}
  end

  def delete_db(db_name) do
    resp = Couch.delete("/#{db_name}")
    assert resp.status_code in [200, 202]
    assert resp.body == %{"ok" => true}
    {:ok, resp}
  end

  def create_doc(db_name, body) do
    resp = Couch.post("/#{db_name}", body: body)
    assert resp.status_code in [201, 202]
    assert resp.body["ok"]
    {:ok, resp}
  end

  def sample_doc_foo do
    %{
      _id: "foo",
      bar: "baz"
    }
  end

  # Generate range of docs with strings as keys
  def make_docs(id_range) do
    for id <- id_range, str_id = Integer.to_string(id) do
      %{"_id" => str_id, "integer" => id, "string" => str_id}
    end
  end

  # Generate range of docs with atoms as keys, which are more
  # idiomatic, and are encoded by jiffy to binaries
  def create_docs(id_range) do
    for id <- id_range, str_id = Integer.to_string(id) do
      %{_id: str_id, integer: id, string: str_id}
    end
  end

  def retry_until(condition, sleep \\ 100, timeout \\ 5000) do
    retry_until(condition, now(:ms), sleep, timeout)
  end

  defp retry_until(condition, start, sleep, timeout) do
    now = now(:ms)

    if now > start + timeout do
      raise "timed out after #{now - start} ms"
    else
      try do
        if condition.() do
          :ok
        else
          raise ExUnit.AssertionError
        end
      rescue
        ExUnit.AssertionError ->
          :timer.sleep(sleep)
          retry_until(condition, start, sleep, timeout)
      end
    end
  end

  defp now(:ms) do
    div(:erlang.system_time(), 1_000_000)
  end

  @spec rev(map(), map()) :: map()
  def rev(doc = %{_id: id}, %{"id" => id, "rev" => rev}) do
    Map.put(doc, :_rev, rev)
  end

  @spec rev([map()], [map()]) :: [map()]
  def rev(docs, rows) when length(docs) == length(rows) do
    for {doc, row} <- Enum.zip(docs, rows), do: rev(doc, row)
  end

  def pretty_inspect(resp) do
    opts = [pretty: true, width: 20, limit: :infinity, printable_limit: :infinity]
    inspect(resp, opts)
  end

  def restart_cluster() do
    resp = Couch.get("/_membership")
    assert resp.status_code == 200
    nodes = resp.body["all_nodes"]

    nodes_ports =
      Enum.reduce(nodes, [], fn node, acc ->
        port = node_to_port(node)
        [{node, port} | acc]
      end)

    tasks =
      Enum.map(nodes_ports, fn {node, port} ->
        Task.async(fn -> restart_node(node, port) end)
      end)

    Task.yield_many(tasks, length(nodes) * 5000)
  end

  def restart_node(node \\ "node1@127.0.0.1") do
    port = node_to_port(node)
    restart_node(node, port)
  end

  defp restart_node(node, port) do
    url = "http://127.0.0.1:#{port}/_node/#{node}/_restart"
    resp = Couch.post(url)
    assert HTTPotion.Response.success?(resp)
    assert resp.body["ok"]
    # make sure node went down. we assuming the node can't bounce quick
    # enough to inroduce a race here
    retry_until(fn -> !node_is_running(port) end)
    # wait utill node is back
    retry_until(fn -> node_is_running(port) end, 500, 30_000)
  end

  defp node_is_running(port) do
    url = "http://127.0.0.1:#{port}/_up"
    resp = Couch.get(url)

    case HTTPotion.Response.success?(resp) do
      true -> resp.status_code in 200..399
      false -> false
    end
  end

  defp node_to_port(node) do
    url = "/_node/#{node}/_config/chttpd/port"
    resp = Couch.get(url)
    assert HTTPotion.Response.success?(resp)
    resp.body
  end
end

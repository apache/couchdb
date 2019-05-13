defmodule Couch.Test.Adapter.Shared do
  @moduledoc "Common functionality for test adapters"
  import ExUnit.Assertions
  def format_user_doc(user) do
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

      %{
        "_id" => "org.couchdb.user:" <> name,
        "type" => "user",
        "name" => name,
        "roles" => roles,
        "password" => password
      }
  end
  def format_query_ddoc(map_fun, reduce_fun, language, view_options) do
    l_map_function =
      if language == "javascript" do
        "#{map_fun} /* avoid race cond #{now(:ms)} */"
      else
        map_fun
      end

    view = %{
      :map => l_map_function
    }

    view =
      if reduce_fun != nil do
        Map.put(view, :reduce, reduce_fun)
      else
        view
      end

    view =
      if view_options != nil do
        Map.put(view, :options, view_options)
      end

    ddoc_name = "_design/temp_#{now(:ms)}"

    %{
      _id: ddoc_name,
      language: language,
      views: %{
        view: view
      }
    }
  end
  defp now(:ms) do
      div(:erlang.system_time(), 1_000_000)
  end
end
defmodule Couch.Test.Utils do
  require Record
  @moduledoc "Helper functions for testing"
  @project_root "#{__DIR__}/../../../"
  Record.defrecord :user_ctx, Record.extract(
    :user_ctx, from: "#{@project_root}/src/couch/include/couch_db.hrl")

  Record.defrecord :doc_info, Record.extract(
    :doc_info, from: "#{@project_root}/src/couch/include/couch_db.hrl")

  def random_name(prefix) do
    time = :erlang.monotonic_time()
    umi = :erlang.unique_integer([:monotonic])
    "#{prefix}-#{time}-#{umi}"
  end

  def random_password() do
    rand_bytes = :crypto.strong_rand_bytes(16)
    rand_bytes
      |> :base64.encode()
      |> String.slice(0..16)
  end

  def db_exists?(db_name) do
    try do
      :fabric.get_db_info(db_name)
    catch
      :error, :database_does_not_exist -> false
    end
  end

  @doc """
  In some cases we need to access record definition at compile time.
  We cannot use Record.defrecord in such cases. This helper function
  can be used instead. Use it as follows:
  ```
  defmodule Foo do
    admin_ctx = {:user_ctx, Utils.erlang_record(
      :user_ctx, "couch/include/couch_db.hrl", roles: ["_admin"])}
  end
  ```

  Longer term we should wrap erlang records as it is done for user_ctx
  see beginning of the Utils.ex. In this case we would be able to use
  them at compile time in other modules.
  ```
  Record.defrecord :user_ctx, Record.extract(
    :user_ctx, from_lib: "couch/include/couch_db.hrl")
  ```
  """
  def erlang_record(name, from_lib, opts \\ []) do
    record_info = Record.extract(name, from_lib: from_lib)
    index = [name | Keyword.keys(record_info)] |> Enum.with_index
    draft = [name | Keyword.values(record_info)] |> List.to_tuple
    opts
      |> Enum.reduce(draft, fn
        {k, v}, acc -> put_elem(acc, index[k], v)
      end)
  end

end
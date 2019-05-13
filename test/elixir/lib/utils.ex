defmodule Couch.Test.Utils do
  require Record
  @moduledoc "Helper functions for testing"
  def random_name(prefix) do
    time = :erlang.monotonic_time()
    umi = :erlang.unique_integer([:monotonic])
    "#{prefix}-#{time}-#{umi}"
  end
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
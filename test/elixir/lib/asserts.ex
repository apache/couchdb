defmodule Couch.Test.Asserts do
  @moduledoc """
  Custom asserts.
  """
  defmacro assert_on_status(resp, expected, failure_message) do
    expected_list = List.wrap(expected)

    expected_msg = case expected_list do
      [single] -> "Expected #{single}"
      multiple -> "Expected one of #{inspect(multiple)}"
    end

    quote do
      status_code = unquote(resp).status_code
      body = unquote(resp).body
      message = "#{unquote(failure_message)} #{unquote(expected_msg)}, got: #{status_code}, body: #{inspect(body)}"
      ExUnit.Assertions.assert(status_code in unquote(expected_list), "#{message}")
    end
  end
end

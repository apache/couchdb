defmodule HelperTest do
  use CouchTestCase

  @moduledoc """
  Test helper code
  """

  test "retry_until handles boolean conditions", _context do
    retry_until(fn ->
      true
    end)
  end

  test "retry_until handles assertions", _context do
    retry_until(fn ->
      assert true
    end)
  end

  test "retry_until times out", _context do
    assert_raise ExUnit.AssertionError, "\n\nExpected truthy, got false\ncode: assert false\n", fn ->
      retry_until(
        fn ->
          assert false
        end,
        1,
        5
      )
    end
  end
end

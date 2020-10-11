defmodule HelperTest do
  use CouchTestCase

  @moduledoc """
  Test helper code
  """

  @moduletag :helper
  @moduletag kind: :single_node

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
    assert_raise RuntimeError, ~r/^timed out after \d+ ms$/, fn ->
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

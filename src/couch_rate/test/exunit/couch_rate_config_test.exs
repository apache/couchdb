# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

defmodule Couch.Rate.Config.Test do
  use ExUnit.Case, async: true
  use ExUnitProperties
  import StreamData

  @erlang_reserved_words MapSet.new([
                           "after",
                           "and",
                           "andalso",
                           "band",
                           "begin",
                           "bnot",
                           "bor",
                           "bsl",
                           "bsr",
                           "bxor",
                           "case",
                           "catch",
                           "cond",
                           "div",
                           "end",
                           "fun",
                           "if",
                           "let",
                           "not",
                           "of",
                           "or",
                           "orelse",
                           "receive",
                           "rem",
                           "try",
                           "when",
                           "xor"
                         ])

  alias :couch_rate_config, as: RLC

  test "parse valid configuration" do
    parsed = RLC.from_str(~S(#{foo => 1, bar => 2.0}))
    assert %{foo: 1, bar: 2} == parsed
  end

  property "roundtrip" do
    check all(options <- valid_config()) do
      parsed = RLC.from_str(RLC.to_str(options))
      assert options == parsed
    end
  end

  defp valid_config() do
    map_of(
      erlang_atom(),
      one_of([
        positive_integer(),
        # we only support positive float
        float(min: 0.0)
      ])
    )
  end

  defp erlang_atom() do
    bind(string(:alphanumeric), fn str ->
      bind(integer(?a..?z), fn char ->
        erlang_atom(str, char)
      end)
    end)
  end

  defp erlang_atom(str, char) do
    if MapSet.member?(@erlang_reserved_words, <<char, str::binary>>) do
      String.to_atom(<<char, char, str::binary>>)
    else
      String.to_atom(<<char, str::binary>>)
    end
  end
end

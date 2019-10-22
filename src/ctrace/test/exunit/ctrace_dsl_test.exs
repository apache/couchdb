defmodule Couch.CTrace.DSL.Test do
  require Logger
  use ExUnit.Case, async: true
  @filter_module List.to_atom(Atom.to_charlist(__MODULE__) ++ '_filter')
  @moduletag capture_log: true

  describe "DSL roundtrip :" do
    test "Simple Parse and Compile" do
      rule_str = ~C"""
        (#{'http.method' := Method}) when Method == get -> [sample(1.0)]
      """

      rule = :ctrace_dsl.parse_rule('get', rule_str)
      rule = set_actions(rule)
      ast = generate([rule])
      :merl.compile_and_load(ast, [:verbose])
    end
  end

  describe "DSL compiler :" do
    test "match clauses are in alphabetical order" do
      rule_str_a = ~C"(#{foo := A}) when A == 1 -> [sample(1)]"
      rule_str_b = ~C"(#{foo := B}) when B == 2 -> [sample(2)]"
      rule_a = :ctrace_dsl.parse_rule('a', rule_str_a)
      rule_b = :ctrace_dsl.parse_rule('b', rule_str_b)
      set_action = fn {:sample, rate} -> {__MODULE__, :as_is, [rate]} end
      rule_a = set_actions(rule_a, set_action)
      rule_b = set_actions(rule_b, set_action)

      ast = generate([rule_b, rule_a])
      :merl.compile_and_load(ast, [:verbose])
      assert match?({[[1]], [[2]]}, {match(%{foo: 1}), match(%{foo: 2})})
      :code.delete(@filter_module)
      :code.purge(@filter_module)

      ast = generate([rule_a, rule_b])
      :merl.compile_and_load(ast, [:verbose])
      assert match?({[[1]], [[2]]}, {match(%{foo: 1}), match(%{foo: 2})})
    end
  end

  describe "DSL parsing :" do
    test "empty map" do
      rule_str = ~C"(#{}) -> [report]"
      assert match?(%{}, parse(rule_str))
    end

    test "empty actions" do
      rule_str = ~C"(#{}) -> []"
      assert match?(%{}, parse(rule_str))
    end
  end

  describe "DSL parsing error handling :" do
    test "body is not a list" do
      rule_str = ~C"(#{}) -> hello"
      error_str = 'Function body should be a list of actions'

      assert catch_throw(parse(rule_str)) == {:error, error_str}
    end

    test "body contains calls" do
      rule_str = ~C"(#{}) -> [module:function()]"
      error_str = ~C"unsuported action 'module:function()'"

      assert catch_throw(parse(rule_str)) == {:error, error_str}
    end

    test "less than one argument" do
      rule_str = ~C"() -> [report]"
      error_str = ~C"The arrity of the filter function should be 1"

      assert catch_throw(parse(rule_str)) == {:error, error_str}
    end

    test "more than one argument" do
      rule_str = ~C"(#{}, foo) -> [report]"
      error_str = ~C"The arrity of the filter function should be 1"

      assert catch_throw(parse(rule_str)) == {:error, error_str}
    end

    test "argument is not a map (atom)" do
      rule_str = ~C"(atom) -> [report]"
      error_str = ~C"The only argument of the filter should be map"

      assert catch_throw(parse(rule_str)) == {:error, error_str}
    end

    test "argument is not a map (list)" do
      rule_str = ~C"([atom]) -> [report]"
      error_str = ~C"The only argument of the filter should be map"

      assert catch_throw(parse(rule_str)) == {:error, error_str}
    end

    test "argument is not a map (integer)" do
      rule_str = ~C"(1) -> [report]"
      error_str = ~C"The only argument of the filter should be map"

      assert catch_throw(parse(rule_str)) == {:error, error_str}
    end

    test "argument is not a map (float)" do
      rule_str = ~C"(1.0) -> [report]"
      error_str = ~C"The only argument of the filter should be map"

      assert catch_throw(parse(rule_str)) == {:error, error_str}
    end
  end

  defp parse(rule) do
    :ctrace_dsl.parse_rule('test', rule)
  end

  defp set_actions(%{} = rule) do
    set_actions(rule, &set_action/1)
  end

  defp set_actions(%{:actions => actions} = rule, map_fun) do
    actions =
      actions
      |> Enum.map(map_fun)

    %{rule | actions: actions}
  end

  defp set_action({:sample, rate}) do
    {__MODULE__, :sample, [rate]}
  end

  defp set_action(:report) do
    {__MODULE__, :report, []}
  end

  def as_is(arg) do
    arg
  end

  def sample(_rate) do
    fn _ -> true end
  end

  def report() do
    fn _ -> true end
  end

  def generate(rules) do
    ast = :ctrace_dsl.generate(@filter_module, rules)
    Logger.debug(fn -> "Generated module:\n#{:ctrace_dsl.source(ast)}\n" end)
    ast
  end

  def match(tags) do
    @filter_module.match(tags)
  end
end

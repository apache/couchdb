defmodule Couch.CTrace.Test do
  require Logger
  use ExUnit.Case
  @moduletag capture_log: true

  setup do
    apps = :test_util.start_applications([:ctrace])
    :meck.new(:couch_log, [{:stub_all, :meck.val(:ok)}])
    on_exit(fn ->
      :test_util.stop_applications(apps)
      :meck.unload()
    end)

    :config.set('tracing.samplers', 'all-docs', 'all', false)
    :config.set('tracing.all-docs', 'all', ~C"(#{method := M}) when M == get -> []", false)
    :config.set_boolean('tracing', 'enabled', true, false)

    {:ok, reporter} = wait_non_error(fn ->
      :passage_tracer_registry.get_reporter(:"all-docs")
    end)

    filter = :passage_reporter.get_state(reporter)
    %{filter: :ctrace_filter.module(filter)}
  end

  describe "Supervision tree :" do
    test "main jaeger reporter is started" do
      assert match?(
               {:ok, _},
               :passage_tracer_registry.get_reporter(:jaeger_passage_reporter)
             )
    end

    test "pre-configured reporter is started" do
      assert match?(
               {:ok, _},
               :passage_tracer_registry.get_reporter(:'all-docs')
             )
    end

    test "reporter is started on config change" do
      :config.set('tracing.samplers', 'bulk', 'all', false)
      :config.set('tracing.bulk', 'all', ~C"(#{}) -> [report]", false)

      assert wait_non_error(fn ->
               :passage_tracer_registry.get_reporter(:bulk)
             end)
    end

    test "reporter is stoped when deleted" do
      assert wait_non_error(fn ->
               :passage_tracer_registry.get_reporter('all-docs')
             end)

      :config.delete('tracing.samplers', 'all-docs', false)

      assert wait_error(fn ->
               :passage_tracer_registry.get_reporter('all-docs')
             end)
    end
  end

  describe "Configuration :" do
    test "recompile rules on config update", %{filter: module} do
      assert match?([], module.match(%{method: :get}))
      assert match?(false, module.match(%{method: :post}))

      :config.set('tracing.all-docs', 'all', ~C"(#{method := M}) when M == post -> []", false)

      assert match?(false, :test_util.wait_other_value(fn ->
        module.match(%{method: :get})
      end, []))
      assert match?([], module.match(%{method: :post}))
    end

    test "log errors", %{filter: module} do
       :config.set('tracing.all-docs', 'all', ~C"( -> syntax_error", false)
       :ctrace.update_tracers()
       [error | _ ] = :test_util.wait_other_value(fn ->
         capture_logs(:error, ~r"cannot compile '")
       end, [])
       assert [:"all-docs", '1: syntax error before: \'->\''] == error
    end
  end

  describe "Matching :" do
    test "should match", %{filter: module} do
       assert match?([], module.match(%{method: :get}))
    end

    test "should not match", %{filter: module} do
       assert match?(false, module.match(%{method: :post}))
    end
  end

  defp wait_error(fun) do
    :test_util.wait_value(fun, :error)
  end

  defp wait_non_error(fun) do
    :test_util.wait_other_value(fun, :error)
  end

  defp capture_logs(level, regexp) do
    history(:couch_log, level, 2)
      |> Enum.flat_map(fn event ->
          {_, _, [msg, args]} = elem(event, 1)
          if Regex.match?(regexp, List.to_string(msg)) do
              [args]
          else
              []
          end
        end)
  end

  defp history(module, function, arity) do
    :meck.history(module)
      |> Enum.filter(fn event ->
          {_, fun, args} = elem(event, 1)
          function == fun and arity == length(args)
        end)
  end

end

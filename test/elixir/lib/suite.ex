defmodule Couch.Test.Suite do
  @moduledoc """
    Common code to configure ExUnit runner.
    It replaces the usual invocation of `ExUnit.start()` in
    `test_helper.exs` related to integration tests with:
    ```
    Couch.Test.Suite.start()
    ```
  """
  @doc """
  This helper function can be used to create `suite.elixir`
  as
  ```
  tests =
    Couch.Test.Suite.list()
    |> Enum.sort()
    |> Couch.Test.Suite.group_by()

  IO.puts(Couch.Test.Suite.pretty_print(tests))

  ```
  """
  def list() do
    test_paths = Keyword.get(Mix.Project.config(), :test_paths, [])
    Enum.reduce(test_paths, [], fn directory, acc ->
      list(directory) ++ acc
    end)
  end

  @doc """
  This helper function can be used to create `suite.elixir`
  as
  ```
  tests =
    Couch.Test.Suite.list(["test/elixir/test"])
    |> Enum.sort()
    |> Couch.Test.Suite.group_by()

  IO.puts(Couch.Test.Suite.pretty_print(tests))
  ```
  """
  def list(directory) do
    ensure_exunit_started()
    Enum.reduce(test_files(directory), [], fn file_path, acc ->
      tests_in_file(file_path) ++ acc
    end)
  end

  @doc """
  This helper function is used in a snippet to create `suite.elixir`
  see list/1
  """
  def group_by(tests) do
    tests |> Enum.group_by(&module_name/1, &test_name/1)
  end

  @doc """
  This helper function is used in a snippet to create `suite.elixir`
  see list/1
  """
  def pretty_print(tests) do
    tests = Enum.join(Enum.sort(Enum.map(tests, fn {module_name, test_names} ->
      test_names = test_names
        |> Enum.map(fn x -> ~s("#{x}") end) |> Enum.join(",\n    ")
      ~s(  "#{module_name}": [\n    #{test_names}\n  ])
    end)), ",\n")
    "%{\n#{tests}\n}"
  end

  def start(exclude \\ []) do
    # If build number detected assume we running on Jenkins
    # and skip certain tests that fail on jenkins.
    default_exclude =
      case System.get_env("BUILD_NUMBER") !== nil do
        true -> [:pending, :skip_on_jenkins]
        false -> [:pending]
      end

    current_exclude = Keyword.get(ExUnit.configuration(), :exclude, [])
    {ignores, current_exclude} = from_file(current_exclude)

    current_include = Keyword.get(ExUnit.configuration(), :include, [])
    {suite, current_include} = from_file(current_include)

    only_test_ids =
      case suite -- ignores do
        [] ->
          nil

        test_ids ->
          to_tests(test_ids)
      end

    ExUnit.configure(
      exclude: Enum.uniq(default_exclude ++ current_exclude ++ exclude),
      include: current_include,
      formatters: [JUnitFormatter, ExUnit.CLIFormatter],
      only_test_ids: only_test_ids
    )

    ExUnit.start()
  end

  # Helpers for start/0

  defp split_files(opts) do
    {files, opts} =
      Enum.split_with(opts, fn x ->
        String.ends_with?(Atom.to_string(x), ".elixir")
      end)

    {Enum.map(files, &Atom.to_string/1), opts}
  end

  defp read_from_file(file_name) do
    {map, _} = Code.eval_file(file_name)

    map
    |> Enum.reduce([], fn {module, tests}, acc ->
      Enum.map(tests, &{module, &1}) ++ acc
    end)
  end

  defp from_file(opts) do
    case split_files(opts) do
      {[], opts} ->
        {[], opts}

      {[file_name], opts} ->
        {read_from_file(file_name), opts}

      {_, _} ->
        throw("Only one file is supported in --exclude or --include")
    end
  end

  defp to_tests(ids) do
    MapSet.new(
      Enum.map(ids, fn {module_name, test_name} ->
        {String.to_atom("Elixir.#{module_name}"), String.to_atom("test #{test_name}")}
      end)
    )
  end

  # Helpers for list/0

  defp ensure_exunit_started() do
    if not Process.get(EXUNIT_STARTED, false) do
      started? =
        Application.started_applications()
        |> Enum.map(&Kernel.elem(&1, 0))
        |> Enum.member?(:ex_unit)

      if not started? do
        ExUnit.start(autorun: false)
        Process.put(EXUNIT_STARTED, true)
      end
    end
  end

  defp test_files(directory) do
    files = Path.wildcard(Path.join(directory, "*_test.exs"))
    Enum.filter(files, &File.regular?/1)
  end

  defp test_helpers(directory) do
    files = Path.wildcard(Path.join(directory, "*_helpers.exs"))
    Enum.filter(files, &File.regular?/1)
  end

  def tests_in_file(file_path) do
    ensure_exunit_started()
    Code.compiler_options(ignore_module_conflict: true)

    Enum.each(
      test_helpers(Path.dirname(file_path)), &require_file/1
    )

    tests =
      Enum.reduce(require_file(file_path), [], fn {module_name, _}, acc ->
        if :erlang.function_exported(module_name, :__ex_unit__, 0) do
          module_name.__ex_unit__().tests ++ acc
        else
          acc
        end
      end)

    Code.unrequire_files([file_path])
    tests
  end

  def require_file(file_path) do
    drop_stderr(fn ->
      Code.require_file(file_path)
    end)
  end

  defp drop_stderr(fun) do
    {:ok, pid} = StringIO.open("")
    original_pid = Process.whereis(:standard_error)

    try do
      Process.unregister(:standard_error)
      Process.register(pid, :standard_error)
      fun.()
    after
      Process.unregister(:standard_error)
      Process.register(original_pid, :standard_error)
      StringIO.close(pid)
    end
  end

  defp test_name(test) do
    String.replace_leading(Atom.to_string(test.name), "test ", "")
  end

  defp module_name(test) do
    test.module
    |> Atom.to_string()
    |> String.replace_leading("Elixir.", "")
  end
end

defmodule CouchDBTest.Mixfile do
  use Mix.Project

  def project do
    [
      app: :couchdbtest,
      version: "0.1.0",
      elixir: "~> 1.5",
      lockfile: Path.expand("mix.lock", __DIR__),
      deps_path: Path.expand("src", __DIR__),
      build_path: Path.expand("_build", __DIR__),
      compilers: [:elixir, :app],
      start_permanent: Mix.env() == :prod,
      build_embedded: Mix.env() == :prod,
      deps: deps(),
      consolidate_protocols: Mix.env() not in [:test, :dev, :integration],
      test_paths: get_test_paths(Mix.env()),
      elixirc_paths: elixirc_paths(Mix.env())
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      applications: [:httpotion]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["test/elixir/lib", "test/elixir/test/support"]
  defp elixirc_paths(:integration), do: ["test/elixir/lib", "test/elixir/test/support"]
  defp elixirc_paths(_), do: ["test/elixir/lib"]

  # Run "mix help deps" to learn about dependencies.
  defp deps() do
    [
      {:junit_formatter, "~> 3.0", only: [:dev, :test, :integration]},
      {:httpotion, "~> 3.0", only: [:dev, :test, :integration], runtime: false},
      {:jiffy, path: Path.expand("src/jiffy", __DIR__)},
      {:ibrowse,
       path: Path.expand("src/ibrowse", __DIR__), override: true, compile: false},
      {:credo, "~> 1.0.0", only: [:dev, :test, :integration], runtime: false}
    ]
  end

  def get_test_paths(:test) do
    Path.wildcard("src/*/test/exunit") |> Enum.filter(&File.dir?/1)
  end

  def get_test_paths(:integration) do
    integration_tests =
      Path.wildcard("src/*/test/integration") |> Enum.filter(&File.dir?/1)

    ["test/elixir/test" | integration_tests]
  end

  def get_test_paths(_) do
    []
  end
end

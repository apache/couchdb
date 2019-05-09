defmodule CouchDBTest.Mixfile do
  use Mix.Project

  def project do
    [
      app: :couchdbtest,
      version: "0.1.0",
      elixir: "~> 1.5",
      deps_dir: "src",
      compilers: [:elixir, :app],
      start_permanent: Mix.env() == :prod,
      build_embedded: Mix.env() == :prod,
      deps: deps(),
      test_paths: get_test_paths(),
      elixirc_paths: elixirc_paths(Mix.env())
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["test/elixir/lib", "test/elixir/test/support"]
  defp elixirc_paths(_), do: ["test/elixir/lib"]

  # Run "mix help deps" to learn about dependencies.
  defp deps() do
    [
      # {:dep_from_hexpm, "~> 0.3.0"},
      {:httpotion, "~> 3.0", only: [:dev, :test], runtime: false},
      {:jiffy, "~> 0.15.2", only: [:dev, :test], runtime: false},
      {:credo, "~> 1.0.0", only: [:dev, :test], runtime: false},
      {:junit_formatter, "~> 3.0", only: [:test], runtime: false}

      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"},
    ]
  end

  def get_test_paths do
    Path.wildcard("src/*/test/exunit") |> Enum.filter(&File.dir?/1)
  end
end

defmodule Fast_Pbkdf2.MixProject do
  use Mix.Project

  def project do
    [
      app: :fast_pbkdf2,
      version: "0.1.0",
      elixir: "~> 1.10",
      deps: deps()
    ]
  end

  defp deps do
    [
      {:benchee, "~> 1.0", only: :dev}
    ]
  end
end

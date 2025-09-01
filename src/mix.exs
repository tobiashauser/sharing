defmodule Sharing.MixProject do
  use Mix.Project

  def project do
    [
      app: :sharing,
      version: "0.1.0",
      elixir: "~> 1.15",
      elixirc_paths: elixirc_paths(Mix.env()),
      deps: deps()
    ]
  end

  # Specifies which paths to compile per environment. The environment
  # is taken from the shell variable MIX_ENV (set in the flake) or
  # falls back to `:dev`.
  defp elixirc_paths(_), do: []

  # Specify the project dependencies (see `mix help deps`).
  defp deps do
    [
      {:phoenix, "~> 1.8.1"}
    ]
  end
end

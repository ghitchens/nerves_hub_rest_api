defmodule JrtpBridge.Mixfile do

  use Mix.Project

  def project, do: [
    app:      :jrtp_bridge,
    version:  version,
    elixir:   "~> 1.0",
    deps:     deps(Mix.env)
  ]

  def application, do: [
      applications: [ :elixir, :jsx, :cowboy, :hub ]
  ]

  defp deps(:test), do: deps(:dev) ++ [
      { :httpotion, github: "myfreeweb/httpotion"}
  ]

  defp deps(_), do: [
      { :earmark, "~> 0.1", only: :dev },
      { :ex_doc, "~> 0.7", only: :dev },
      { :hub, github: "cellulose/hub" },
      { :firmware, github: "cellulose/firmware" },
      { :cowboy, "~> 1.0" },
      { :jsx, github: "talentdeficit/jsx", ref: "v1.4.3", override: true },
  ]

  defp version do
    case File.read("VERSION") do
      {:ok, ver} -> String.strip ver
      _ -> "0.0.0-dev"
    end
  end
end

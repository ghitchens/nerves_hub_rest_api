defmodule JrtpBridge.Mixfile do

  use Mix.Project

  def project, do: [
    app:      :jrtp_bridge,
    version:  version,
    elixir:   "~> 1.0",
    deps:     deps(Mix.env)
  ]

  def application, do: [
      applications: [ :elixir, :jsx, :cowboy, :nerves_hub ]
  ]

  defp deps(:test), do: deps(:dev) ++ [
      { :httpotion, github: "myfreeweb/httpotion"}
  ]

  defp deps(_), do: [
      { :earmark, "~> 0.1", only: :dev },
      { :ex_doc, "~> 0.7", only: :dev },
      { :nerves_hub, github: "nerves-project/nerves_hub" },
      { :cowboy, "~> 1.0" },
      { :exjsx, "~> 3.2.0" },
  ]

  defp version do
    case File.read("VERSION") do
      {:ok, ver} -> String.strip ver
      _ -> "0.0.0-dev"
    end
  end
end

defmodule Nerves.HubRestApi.Mixfile do

  @version "0.1.1-dev"

  use Mix.Project

  def project, do: [
    app:      :nerves_hub_rest_api,
    version:  @version,
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
    { :nerves_hub, github: "nerves-project/nerves_hub" },
    { :earmark, "~> 0.1", only: :dev },
    { :ex_doc, "~> 0.7", only: :dev },
    { :cowboy, "~> 1.0" },
    { :exjsx, "~> 3.2.0" },
  ]

end

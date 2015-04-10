defmodule JrtpBridge.Mixfile do

  use Mix.Project

  def project, do: [
    app:      :jrtp_bridge,
    version:  "0.0.1",
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
      { :hub, github: "cellulose/hub"},
      { :cowboy, "~> 1.0" },
      { :jsx, github: "talentdeficit/jsx", ref: "v1.4.3", override: true },
  ]

end

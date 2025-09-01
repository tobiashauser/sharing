defmodule Sharing.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {DNSCluster, query: Application.get_env(:sharing, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: Sharing.PubSub},
      SharingWeb.Endpoint
    ]

    opts = [strategy: :one_for_one, name: Sharing.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @impl true
  def config_change(changed, _new, removed) do
    SharingWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end

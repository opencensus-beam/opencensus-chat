defmodule Chat.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    :ets.new(:app_chat_state, [:set, :public, :named_table])
    :ets.insert(:app_chat_state, {:users, []})
    :ets.insert(:app_chat_state, {:conversations, []})
    :ets.insert(:app_chat_state, {:messages, []})

    ChatWeb.Observability.Plug.Metrics.setup_metrics()
    ChatWeb.Observability.Plug.MetricsExporter.setup()
    :prometheus_registry.register_collector(:oc_stat_exporter_prometheus)
    
    # List all child processes to be supervised
    children = [
      # Start the Ecto repository
      # Chat.Repo,
      # Start the endpoint when the application starts
      ChatWeb.Endpoint
      # Starts a worker by calling: Chat.Worker.start_link(arg)
      # {Chat.Worker, arg},
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Chat.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    ChatWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end

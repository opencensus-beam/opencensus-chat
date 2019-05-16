# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

config :chat,
  ecto_repos: [Chat.Repo]

# Configures the endpoint
config :chat, ChatWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "auum+pzxRHAxqxoEStNEcACg0TxsTX1HksTzFpw4hdzlUeRcdx5+JIonn/nejuw7",
  render_errors: [view: ChatWeb.ErrorView, accepts: ~w(json)],
  pubsub: [name: Chat.PubSub, adapter: Phoenix.PubSub.PG2],
  instrumenters: [OpencensusPhoenix.Instrumenter],
  http: [protocol_options: [middlewares: [:opencensus_cowboy2_context, :cowboy_router, :cowboy_handler],
                            metrics_callback: &:opencensus_cowboy2_instrumenter.observe/1]]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

config :guardian, Chat.Guardian,
  issuer: "Chat.Guardian",
  verify_module: Guardian.JWT,
  secret_key: "xPzfhA/qbNaWDkST8JsSbQQVWRC8FIkqQZHSH9+wGylupbZwtvEHkQU0sBz6LaD2",
  serializer: Chat.GuardianSerializer

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"

config :opencensus, :reporters,
  oc_reporter_jaeger: [hostname: 'localhost',
                       port: 6831, ##  default for compact protocol
                       service_name: "chat",
                       service_tags: %{"key" => "value"}],
  oc_reporter_zipkin: [address: 'http://localhost:9411/api/v2/spans',
                       local_endpoint: %{"serviceName" => "chat"}]

config :opencensus, :sampler, {:oc_sampler_always, []}

config :chat, Chat.History,
  url: 'http://localhost:4001/history',
  count: 50

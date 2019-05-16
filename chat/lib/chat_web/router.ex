defmodule ChatWeb.Router do
  use ChatWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  pipeline :secure do
    plug Guardian.Plug.Pipeline, module: Chat.Guardian, error_handler: ChatWeb.SessionController

    plug Guardian.Plug.VerifySession
    plug Guardian.Plug.VerifyHeader
    plug Guardian.Plug.EnsureAuthenticated
    plug Guardian.Plug.LoadResource, allow_blank: true
  end

  scope "/api", ChatWeb do
    pipe_through [:api]

    post "/session", SessionController, :create
    post "/users", UserController, :create
  end

  scope "/api", ChatWeb do
    pipe_through [:api, :secure]

    resources "/users", UserController, except: [:new, :create, :edit]
    resources "/conversations", ConversationController, except: [:new, :edit]

    get "/me", UserController, :current_user
    delete "/session", SessionController, :delete

    post "/message", ConversationController, :add_message
  end
end

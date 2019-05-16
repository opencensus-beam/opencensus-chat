defmodule ChatWeb.UserController do
  use ChatWeb, :controller

  alias Chat.User

  action_fallback ChatWeb.FallbackController

  def index(conn, _params) do
    users = User.all()
    render(conn, "index.json", users: users)
  end

  def create(conn, %{"user" => user_params}) do
    with {:ok, user} <- User.create_user(user_params) do
      conn
      |> put_status(:created)
      # |> put_resp_header("location", Routes.user_path(conn, :show, user))
      |> render("show.json", user: user)
    end
  end

  def show(conn, %{"id" => id}) do
    case User.get(id) do
      nil ->
        conn
        |> put_status(:not_found)
        |> put_view(ChatWeb.ErrorView)
        |> render(:"404")
      user ->
        render(conn, "show.json", user: user)
    end
  end

  def delete(conn, %{"id" => _id}) do
    #  TODO
    conn
    |> put_status(:no_content)
    |> json("")
  end

  def current_user(conn, params) do
    user = Chat.Guardian.Plug.current_resource(conn)
    render(conn, "show.json", user: user)
  end
end

defmodule ChatWeb.ConversationController do
  use ChatWeb, :controller

  alias Chat.Message
  alias Chat.Conversation
  alias Chat.User

  action_fallback ChatWeb.FallbackController

  def index(conn, _params) do
    conversations = Conversation.all()
    render(conn, "index.json", conversations: conversations)
  end

  def create(conn, %{"conversation" => params}) do
    with {:ok, conversation} <- Conversation.create_conversation(params) do
      conn
      |> put_status(:created)
      # |> put_resp_header("location", Routes.user_path(conn, :show, user))
      |> render("show.json", conversation: conversation)
    end
  end

  def show(conn, params) do
    case Conversation.get(params) do
      nil ->
        conn
        |> put_status(:not_found)
        |> put_view(ChatWeb.ErrorView)
        |> render(:"404")
      conversation ->
        render(conn, "show.json", conversation: conversation)
    end
  end

  def delete(conn, %{"id" => _id}) do
    #  TODO
    conn
    |> put_status(:no_content)
    |> json("")
  end

  def add_message(conn, params) do
    with {:ok, message} <- Message.create_message(params) do
      conn
      |> put_status(:created)
      # |> put_resp_header("location", Routes.user_path(conn, :show, user))
      |> json("")
    end
  end
end

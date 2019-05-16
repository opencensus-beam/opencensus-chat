defmodule ChatWeb.ChatChannel do
  use Phoenix.Channel

  def join("chat:all", _message, socket) do
    {:ok, socket}
  end
  def join("chat:" <> chat_id, _params, socket) do
    {:ok, socket}
  end
end
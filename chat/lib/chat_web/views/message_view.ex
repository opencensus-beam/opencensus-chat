defmodule ChatWeb.MessageView do
  use ChatWeb, :view
  alias ChatWeb.MessageView

  def render("index.json", %{messages: messages}) do
    render_many(messages, MessageView, "message.json")
  end

  def render("show.json", %{message: message}) do
    render_one(message, MessageView, "message.json")
  end

  def render("message.json", %{message: message}) do
    %{id: message.id,
      conversation_id: message.conversation_id,
      user_id: message.user_id,
      content: message.content,
      date: message.date}
  end
end

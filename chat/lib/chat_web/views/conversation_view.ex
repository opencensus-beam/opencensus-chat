defmodule ChatWeb.ConversationView do
  use ChatWeb, :view
  alias ChatWeb.ConversationView
  alias ChatWeb.MessageView

  def render("index.json", %{conversations: conversations}) do
    render_many(conversations, ConversationView, "conversation.json")
  end

  def render("show.json", %{conversation: conversation}) do
    render_one(conversation, ConversationView, "conversation.json")
  end

  def render("conversation.json", %{conversation: conversation}) do
    %{id: conversation.id,
      users: conversation.users,
      title: conversation.title,
      content: render_many(conversation.content, MessageView, "message.json")
    }
  end
end

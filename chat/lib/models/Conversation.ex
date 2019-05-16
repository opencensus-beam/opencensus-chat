defmodule Chat.Conversation do
  alias Chat.Message

  defstruct [:id, :users, :content, :title]

  def all() do
    [{:conversations, conversations} | _] = :ets.lookup(:app_chat_state, :conversations)
    load_messages(conversations)
  end

  def get(%{"id" => id} = params) do
    all()
    |> Enum.find(fn %{id: ^id} -> true
                     _ -> false end)
    |> load_messages()
    |> slice_messages(params)
  end
  # def get(%{"user" => user}) do
  #   all()
  #   |> Enum.filter(fn %{users: users} -> Enum.member?(users, user)
  #                     _ -> false end)
  # end
  def get(_) do
    nil
  end

  def slice_messages(conversations, params) when is_list(conversations) do
    Enum.map(conversations, fn c -> slice_messages(c, params) end)
  end
  def slice_messages(conversation, params) do
    default_count = Application.get_env(:chat, Chat.History)[:count]
    messages = Enum.reverse(conversation.content)

    last_message_index = case Map.get(params, "last_message_id", nil) do
      nil -> 0
      last_message_id ->
        Enum.find_index(messages, fn %{id: ^last_message_id} -> true
                                     _ -> false end)
    end

    sliced_messages = case last_message_index do
      nil -> messages
      index ->
        Enum.slice(messages, index + 1, Map.get(params, "count", default_count))
    end

    IO.inspect "--"
    IO.inspect default_count
    IO.inspect messages
    IO.inspect sliced_messages
    IO.inspect "------"


    %Chat.Conversation{conversation | content: sliced_messages}
  end

  #

  defp load_messages(conversations) when is_list(conversations) do
    Enum.map(conversations, &load_messages/1)
  end
  defp load_messages(%Chat.Conversation{} = conversation) do
    %Chat.Conversation{conversation | content: Message.get(%{"conversation_id" => conversation.id})}
  end

  #

  def create_conversation(params) do
    {:ok, conversation}  = _create_conversation(params)

    insert_conversation_to_ets(conversation)

    ChatWeb.Endpoint.broadcast(
      "chat:all",
      "new_chat",
      ChatWeb.ConversationView.render("show.json", conversation: conversation)
    )

    {:ok, conversation}
  end

  defp _create_conversation(%{"users" => users, "title" => title}) do
    {:ok, %Chat.Conversation{id: UUID.uuid4(), users: users, title: title, content: []}}
  end
  defp _create_conversation(%{users: users, title: title}) do
    {:ok, %Chat.Conversation{id: UUID.uuid4(), users: users, title: title, content: []}}
  end
  defp _create_conversation(_) do
    {:error, :invalid_fields}
  end

  #

  defp insert_conversation_to_ets(conversation) do
    :ets.insert(:app_chat_state, {:conversations, all() ++ [conversation]})
  end
end
defmodule Chat.Message do
  alias Chat.History

  defstruct [:id, :conversation_id, :user_id, :content, :date]

  def all() do
    Jason.decode!(Chat.History.get())
    |> Enum.map(fn (m) ->
        {:ok, message} = _create_message(m["message"])
        message
      end)
    |> Enum.sort(fn (m1, m2) when is_binary(m1) and is_binary(m2) ->
                      case DateTime.compare DateTime.from_iso8601(m1.date), DateTime.from_iso8601(m2.date) do
                        :lt -> true
                        _ -> false
                      end
                    (_, _) -> false
                  end)
  end

  def get(%{"id" => id}) do
    all()
    |> Enum.find(fn %{id: ^id} -> true
                    _ -> false end)
  end
  def get(%{"conversation_id" => conversation_id}) do
    all()
    |> Enum.filter(fn %{conversation_id: ^conversation_id} -> true
                      _ -> false end)
  end
  def get(_) do
    nil
  end

  def create_message(params) do
    {:ok, message}  = _create_message(params)

    # insert_message_to_ets(message)
    insert_message_to_history(message)

    ChatWeb.Endpoint.broadcast(
      "chat:" <> message.conversation_id,
      "chat_message",
      ChatWeb.MessageView.render("show.json", message: message)
    )

    {:ok, message}
  end

  defp _create_message(%{"id" => id, "conversation_id" => cid, "user_id" => user_id, "content" => content, "date" => date}) do
    {:ok, %Chat.Message{
      id: id,
      conversation_id: cid,
      user_id: user_id,
      content: content,
      date: date
    }}
  end
  defp _create_message(%{"conversation_id" => cid, "user_id" => user_id, "content" => content} = params) do
    _create_message(Map.merge(params, %{
        "id" => UUID.uuid4(),
        "date" => DateTime.utc_now()
      })
    )
  end
  defp _create_message(_) do
    {:error, :invalid_fields}
  end

  #

  defp insert_message_to_history(%Chat.Message{} = message) do
    rendered_message = ChatWeb.MessageView.render("show.json", message: message)
    json = Jason.encode!([rendered_message])
    History.add(json)
  end
end
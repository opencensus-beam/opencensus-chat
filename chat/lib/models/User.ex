defmodule Chat.User do
  defstruct [:id, :email, :password, :name]

  def all() do
    [{:users, users} | _] = :ets.lookup(:app_chat_state, :users)
    users
  end

  def get(%{"id" => id}) do
    all()
    |> Enum.find(fn %{id: ^id} -> true
                    _ -> false end)
  end
  def get(%{"email" => email}) do
    all()
    |> Enum.find(fn %{email: ^email} -> true
                    _ -> false end)
  end
  def get(_) do
    nil
  end

  def create_user(user_data) do
    {:ok, user} = _create_user(user_data)

    insert_user_to_ets(user)

    ChatWeb.Endpoint.broadcast(
      "chat:all",
      "new_user",
      ChatWeb.UserView.render("show.json", user: user)
    )

    {:ok, user}
  end

  def _create_user(%{"email" => email, "password" => password, "name" => name}) do
    {:ok, %Chat.User{id: UUID.uuid4(), email: email, password: password, name: name}}
  end
  def _create_user(%{email: email, password: password, name: name}) do
    {:ok, %Chat.User{id: UUID.uuid4(), email: email, password: password, name: name}}
  end
  def _create_user(_) do
    {:error, :invalid_fields}
  end

  #

  def insert_user_to_ets(user) do
    :ets.insert(:app_chat_state, {:users, all() ++ [user]})
  end
end
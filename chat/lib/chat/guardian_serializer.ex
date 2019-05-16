defmodule Chat.GuardianSerializer do
  @behaviour Guardian.Serializer

  # alias MyApp.Repo
  alias Chat.User

  def for_token(user = %{}), do: {:ok, "User:#{user[:id]}"}
  def for_token(_), do: {:error, "Unknown resource"}

  def from_token("User:" <> id), do: {:ok, User.get(%{"id" => id})}
  def from_token(_), do: {:error, "Unknown resource"}
end

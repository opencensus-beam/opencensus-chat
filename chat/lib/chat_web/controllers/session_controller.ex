defmodule ChatWeb.SessionController do
  use ChatWeb, :controller

  alias Chat.User

  def create(conn, %{"email" => email}) do
    case User.get(%{"email" => email}) do
      nil ->
        conn
        |> send_resp(401, "")

      user ->
        {:ok, jwt, _claims} = Chat.Guardian.encode_and_sign(user, %{}, [])

        conn
        |> render("show.json", session: %{token: jwt})
    end
  end

  # def delete(conn, _params) do
  #   jwt = Chat.Guardian.Plug.current_token(conn)
  #   q = Chat.Guardian.revoke(jwt)
  #   IO.inspect q

  #   put_status(conn, :no_content) |> json("")
  # end

  def auth_error(conn, {type, reason}, _opts) do
    body = Jason.encode!(%{message: to_string(type)})
    send_resp(conn, 401, body)
  end
end
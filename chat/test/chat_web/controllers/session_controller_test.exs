defmodule ChatWeb.SessionControllerTest do
  use ChatWeb.ConnCase

  alias Chat.Auth
  alias Chat.Auth.Session

  def fixture(:session) do
    %{
      token: "some token"
    }
  end

  setup %{conn: conn} do
    {:ok, conn: put_req_header(conn, "accept", "application/json")}
  end

  describe "create session" do
    test "renders token when data is valid", %{conn: conn} do
      conn = post(conn, "/api/session", %{"email" => "user@chat.com", "password" => "qwerty"})
      assert %{"token" => token} = json_response(conn, 200)
    end

    test "renders error when data is invalid", %{conn: conn} do
      conn = post(conn, "/api/session", %{"email" => "wrong_user@chat.com", "password" => "qwerty"})
      assert response(conn, 401)
    end
  end

  describe "try unauthorized access" do
    test "renders error", %{conn: conn} do
      conn = get(conn, "/api/users")
      assert response(conn, 401)
    end
  end

  #
  # describe "delete session" do
  #   test "create valid session and revoke it", %{conn: conn} do
  #     conn2 = post(conn, "/api/session", %{"email" => "user@chat.com", "password" => "qwerty"})
  #     assert %{"token" => token} = json_response(conn2, 200)

  #     conn3 = conn
  #     |> put_req_header("authorization", "Bearer " <> token)
  #     |> delete("/api/session")
  #     assert json_response(conn3, 204)

  #     conn4 = conn
  #     |> put_req_header("authorization", "Bearer " <> token)
  #     |> delete("/api/session")
  #     assert json_response(conn4, 401)
  #   end
  # end
end

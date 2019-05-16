defmodule ChatWeb.UserControllerTest do
  use ChatWeb.ConnCase

  alias Chat
  alias Chat.User

  @create_attrs %{
    id: "2",
    email: "some email",
    password: "some password"
  }
  @update_attrs %{
    id: "3",
    email: "some updated email",
    password: "some updated password"
  }
  @invalid_attrs %{id: nil, email: nil, password: nil}

  def fixture(:user) do
    {:ok, user} = User.create_user(@create_attrs)
    user
  end

  setup %{conn: conn} do
    conn2 = post(conn, "/api/session", %{"email" => "user@chat.com", "password" => "qwerty"})
    %{"token" => token} = json_response(conn2, 200)

    conn = conn
    |> put_req_header("accept", "application/json")
    |> put_req_header("authorization", "Bearer " <> token)
    {:ok, conn: conn}
  end

  describe "index" do
    test "lists all users", %{conn: conn} do
      conn = get(conn, Routes.user_path(conn, :index))
      assert [_ | _] = json_response(conn, 200)
    end
  end

  describe "create user" do
    test "renders user when data is valid", %{conn: conn} do
      conn = post(conn, Routes.user_path(conn, :create), user: @create_attrs)
      assert %{"id" => id} = json_response(conn, 201)
      assert id

      # conn = get(conn, Routes.user_path(conn, :show, id))

      # assert %{
      #          "id" => id,
      #          "email" => "some email",
      #          "password" => "some password"
      #        } = json_response(conn, 200)["data"]
    end

    # test "renders errors when data is invalid", %{conn: conn} do
    #   conn = post(conn, Routes.user_path(conn, :create), user: @invalid_attrs)
    #   assert json_response(conn, 422)["errors"] != %{}
    # end
  end

  # describe "update user" do
  #   setup [:create_user]

  #   test "renders user when data is valid", %{conn: conn, user: %{id: id} = user} do
  #     conn = put(conn, Routes.user_path(conn, :update, user), user: @update_attrs)
  #     assert %{"id" => ^id} = json_response(conn, 200)["data"]

  #     conn = get(conn, Routes.user_path(conn, :show, id))

  #     assert %{
  #              "id" => id,
  #              "email" => "some updated email",
  #              "password" => "some updated password"
  #            } = json_response(conn, 200)["data"]
  #   end

  #   test "renders errors when data is invalid", %{conn: conn, user: user} do
  #     conn = put(conn, Routes.user_path(conn, :update, user), user: @invalid_attrs)
  #     assert json_response(conn, 422)["errors"] != %{}
  #   end
  # end

  describe "delete user" do
    setup [:create_user]

    test "deletes chosen user", %{conn: conn, user: user} do
      conn = delete(conn, Routes.user_path(conn, :delete, user))
      assert response(conn, 204)

      assert response(get(conn, Routes.user_path(conn, :show, user)), 404)
    end
  end

  defp create_user(_) do
    user = fixture(:user)
    {:ok, user: user}
  end
end

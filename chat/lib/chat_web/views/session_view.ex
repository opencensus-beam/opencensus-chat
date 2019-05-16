defmodule ChatWeb.SessionView do
  use ChatWeb, :view
  alias ChatWeb.SessionView

  def render("show.json", %{session: session}) do
    render_one(session, SessionView, "session.json")
  end

  def render("session.json", %{session: session}) do
    %{token: session.token}
  end
end

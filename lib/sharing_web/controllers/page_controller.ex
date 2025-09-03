defmodule SharingWeb.PageController do
  use SharingWeb, :controller

  def home(conn, _params) do
    render(conn, :home)
  end
end

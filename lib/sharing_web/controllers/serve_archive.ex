defmodule SharingWeb.ServeArchiveController do
  use SharingWeb, :controller

  alias SharingWeb.Helpers

  def serve(conn, %{"code" => code}) do
    send_download(conn, {:file, Helpers.archive(code)}, filename: "#{code}.zip")
  end
end

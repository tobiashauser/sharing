defmodule SharingWeb.ServeArchiveController do
  use SharingWeb, :controller

  def serve(conn, %{"code" => code}) do
    path = Path.join(Application.app_dir(:sharing, "store"), "#{code}.zip")
    send_download(conn, {:file, path}, filename: "#{code}.zip")
  end
end

defmodule SharingWeb.UploadTest do
  use SharingWeb, :live_view

  def mount(_params, _session, socket) do
    {
      :ok,
      socket
      |> assign(:dragging, false)
      |> assign(:uploaded_files, [])
      |> allow_upload(:files, accept: :any, max_entries: 100)
    }
  end

  def handle_event("dragenter", _params, socket) do
    {:noreply, socket |> assign(:dragging, true)}
  end

  def handle_event("dragleave", _params, socket) do
    {:noreply, socket |> assign(:dragging, false)}
  end

  def handle_event("dragover", _params, socket) do
    {:noreply, socket}
  end

  def handle_event("drop", _params, socket) do
    {:noreply, socket |> assign(:dragging, false)}
  end

  def handle_event("validate", params, socket) do
    dbg(socket.assigns.uploads.files)
    {:noreply, socket}
  end

  def handle_event("cancel-upload", %{"ref" => ref}, socket) do
    {:noreply, cancel_upload(socket, :files, ref)}
  end

  def handle_event("upload", _params, socket) do
    uploaded_files =
      consume_uploaded_entries(socket, :files, fn %{path: path}, _entry ->
        dest =
          Path.join(Application.app_dir(:sharing, "priv/static/uploads"), Path.basename(path))

        File.cp!(path, dest)
        {:ok, ~p"/uploads/#{Path.basename(dest)}"}
      end)

    {:noreply, update(socket, :uploaded_files, &(&1 ++ uploaded_files))}
  end

  defp error_to_string(:too_large), do: "Too large"
  defp error_to_string(:not_accepted), do: "You have selected an unacceptable file type"
  defp error_to_string(:too_many_files), do: "You have selected too many files"
end

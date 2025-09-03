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

  # Calculate unique ids from all entries and push them to the
  # DragAndDrop javascript handler.
  def handle_event("validate", _params, socket) do
    {:noreply, socket |> push_file_ids()}
  end

  def handle_event("cancel-upload", %{"ref" => ref}, socket) do
    {
      :noreply,
      socket
      |> cancel_upload(:files, ref)
      |> push_file_ids()
    }
  end

  def handle_event("upload", _params, socket) do
    uploaded_files =
      consume_uploaded_entries(socket, :files, fn %{path: path}, _entry ->
        dest =
          Path.join(Application.app_dir(:sharing, "priv/static/uploads"), Path.basename(path))

        File.cp!(path, dest)
        {:ok, ~p"/uploads/#{Path.basename(dest)}"}
      end)

    {:noreply, socket |> update(:uploaded_files, &(&1 ++ uploaded_files))}
  end

  defp push_file_ids(socket) do
    ids =
      socket.assigns.uploads.files.entries
      # The schema by which the fileIds are created must be kept in sync with
      # `drag-and-drop.js`.
      |> Enum.map(&(&1.client_name <> ":" <> &1.client_relative_path))

    socket |> push_event("file-ids", %{ids: ids})
  end

  defp error_to_string(:too_large), do: "Too large"
  defp error_to_string(:not_accepted), do: "You have selected an unacceptable file type"
  defp error_to_string(:too_many_files), do: "You have selected too many files"
end

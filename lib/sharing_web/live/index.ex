defmodule SharingWeb.Index do
  use SharingWeb, :live_view

  def mount(_params, _session, socket) do
    {
      :ok,
      socket
      |> assign(:dragging, false)
      |> assign(:uploaded_files, [])
      |> allow_upload(:avatar,
        accept: :any,
        auto_upload: true,
        progress: &handle_progress/3
      )
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

  def handle_event("validate", _params, socket) do
    {:noreply, socket}
  end

  def handle_event("cancel-upload", %{"ref" => ref}, socket) do
    {:noreply, cancel_upload(socket, :avatar, ref)}
  end

  defp handle_progress(:avatar, entry, socket) do
    dbg(entry)

    if entry.done? do
      uploaded_files =
        consume_uploaded_entries(socket, :avatar, fn %{path: path}, _entry ->
          dest =
            Path.join(Application.app_dir(:sharing, "priv/static/uploads"), Path.basename(path))

          File.cp!(path, dest)
          {:ok, ~p"/uploads/#{Path.basename(dest)}"}
        end)

      {:noreply, update(socket, :uploaded_files, &(&1 ++ uploaded_files))}
    else
      {:noreply, socket}
    end
  end
end

defmodule SharingWeb.Index do
  use SharingWeb, :live_view

  alias SharingWeb.ActionButton
  alias SharingWeb.DropArea
  alias SharingWeb.Info
  alias SharingWeb.ItemCard

  def mount(_params, _session, socket) do
    {
      :ok,
      socket
      |> assign(:uploaded_files, [])
      |> allow_upload(
        :files,
        accept: :any,
        max_entries: 100,
        max_file_size: 1_000_000_000
      )
    }
  end

  ### --------------------------------------------------------------------- ###
  ### Actions                                                               ###
  ### --------------------------------------------------------------------- ###

  defp push_file_ids(socket) do
    ids =
      socket.assigns.uploads.files.entries
      # The schema by which the fileIds are created must be kept in sync with
      # `drag-and-drop.js`.
      # |> Enum.map(&(&1.client_name <> ":" <> &1.client_relative_path))
      |> Enum.map(fn entry ->
        if entry.client_relative_path == "",
          do: entry.client_name,
          else: entry.client_relative_path
      end)

    socket |> push_event("file-ids", %{ids: ids})
  end

  defp dbg_entries(socket) do
    dbg(socket.assigns.uploads.files.entries)
    socket
  end

  defp update_client_relative_paths(socket, directories) do
    update(socket, :uploads, fn uploads ->
      put_in(
        uploads.files.entries,
        Enum.map(uploads.files.entries, fn entry ->
          if dir = Map.get(directories, entry.ref),
            do: %{entry | client_relative_path: dir},
            else: entry
        end)
      )
    end)
    |> dbg_entries()
  end

  ### Action Button ---------------------------------------

  def handle_event("show-input", _params, socket) do
    {:noreply, socket |> push_event("show-input", %{})}
  end

  def handle_event("show-button", _params, socket) do
    {:noreply, socket |> push_event("show-button", %{})}
  end

  def handle_event("show-code", _params, socket) do
    {:noreply, socket |> push_event("show-code", %{})}
  end

  def handle_event("submit-code", %{"code" => code}, socket) do
    {:noreply, socket}
  end

  ### File Uploads ----------------------------------------

  def handle_event("directories", params, socket) do
    {:noreply, socket |> update_client_relative_paths(params)}
  end

  def handle_event("validate", _params, socket) do
    {:noreply, socket}
  end

  def handle_event("cancel-upload", %{"ref" => ref}, socket) do
    handle_event("cancel-upload", %{"refs" => [ref]}, socket)
  end

  def handle_event("cancel-upload", %{"refs" => refs}, socket) when is_list(refs) do
    socket =
      Enum.reduce(refs, socket, fn ref, acc ->
        cancel_upload(acc, :files, ref)
      end)

    {:noreply, socket |> push_file_ids()}
  end

  def handle_event("upload", _params, socket) do
    dbg(socket)
    uploaded_files = []
    {:noreply, socket |> update(:uploaded_files, &(&1 ++ uploaded_files))}
  end

  def handle_event("open-file-picker", _params, socket) do
    {:noreply, socket |> push_event("click", %{id: socket.assigns.uploads.files.ref})}
  end

  ### --------------------------------------------------------------------- ###
  ### Components                                                            ###
  ### --------------------------------------------------------------------- ###

  # Must contain a button with type submit!
  defp drop_area(assigns) do
    ~H"""
    <div class="mt-[min(max(100vw,40rem)-40rem,max(100vh,40rem)-40rem,10vh)]">
      <div class="flex justify-center">
        <DropArea.render class="h-44 max-w-md w-2/3 cursor-pointer" />
      </div>
    </div>
    """
  end

  ### --------------------------------------------------------------------- ###
  ### View                                                                  ###
  ### --------------------------------------------------------------------- ###

  def render(assigns) do
    ~H"""
    <div id="window-drag-events" phx-hook="WindowDragEvents">
      <div data-uploads={!Enum.empty?(assigns.uploads.files.entries)}>
        <div class="flex flex-col gap-6">
          <div class="flex justify-between">
            <ActionButton.render />
            <Info.render />
          </div>
          <form
            phx-submit="upload"
            phx-change="validate">
            <label for={@uploads.files.ref}>
              <.drop_area active={!Enum.empty?(@uploads.files.entries)} />
            </label>
            <.live_file_input
              class="sr-only"
              upload={@uploads.files}
            />
          </form>
          <div class="md:snap-x gap-2 md:grid-flow-col md:grid-rows-5 grid snap-mandatory auto-cols-[minmax(300px,400px)] justify-center-safe overflow-scroll [scrollbar-width:none] [&::-webkit-scrollbar]:hidden">
            <ItemCard.render
              :for={item <- ItemCard.normalize(@uploads.files)}
              item={item}
            />
          </div>
        </div>
      </div>
    </div>
    """
  end
end

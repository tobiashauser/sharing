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
      |> assign(:dragging, false)
      |> assign(:uploaded_files, [])
      |> allow_upload(:files, accept: :any, max_entries: 100)
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
      |> Enum.map(&(&1.client_name <> ":" <> &1.client_relative_path))

    socket |> push_event("file-ids", %{ids: ids})
  end

  ### Action Button ---------------------------------------

  def handle_event("show-input", _params, socket) do
    {:noreply, ActionButton.show_input(socket)}
  end

  def handle_event("show-button", _params, socket) do
    {:noreply, ActionButton.show_button(socket)}
  end

  def handle_event("show-code", _params, socket) do
    {:noreply, ActionButton.show_code(socket)}
  end

  ### File Uploads ----------------------------------------

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
    uploaded_files = []
    {:noreply, socket |> update(:uploaded_files, &(&1 ++ uploaded_files))}
  end

  ### Drag and Drop ---------------------------------------

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

  def handle_event("open-file-picker", _params, socket) do
    {:noreply, socket |> push_event("click", %{ref: socket.assigns.uploads.files.ref})}
  end

  ### --------------------------------------------------------------------- ###
  ### Components                                                            ###
  ### --------------------------------------------------------------------- ###

  ### Help Button -----------------------------------------

  attr(:class, :string, default: "")

  defp info_button(assigns) do
    ~H"""
    """
  end

  ### Drop Area -------------------------------------------

  # Must contain a button with type submit!
  defp drop_area(assigns) do
    ~H"""
    <div class="mt-[min(max(100vw,40rem)-40rem,max(100vh,40rem)-40rem,10vh)]">
      <div class="flex justify-center">
        <DropArea.render />
      </div>
    </div>
    """
  end

  ### --------------------------------------------------------------------- ###
  ### View                                                                  ###
  ### --------------------------------------------------------------------- ###

  def render(assigns) do
    ~H"""
    <div class="flex justify-between">
      <ActionButton.render class="border-2" />
      <Info.render class="border-2"/>
    </div>
    <form
      id="file-upload-form"
      phx-hook="WindowDragEvents" 
      phx-submit="upload"
      phx-change="validate">
      <.drop_area/>
      <.live_file_input
        class="sr-only"
        upload={@uploads.files}
      />
    </form>
    <div class="md:snap-x md:grid-flow-col md:grid-rows-5 grid snap-mandatory auto-cols-[minmax(300px,400px)] justify-center-safe overflow-scroll [scrollbar-width:none] [&::-webkit-scrollbar]:hidden">
      <ItemCard.render
        :for={item <- ItemCard.normalize(@uploads.files)}
        item={item}
      />
    </div>
    """
  end
end

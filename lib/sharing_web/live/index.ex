defmodule SharingWeb.Index do
  use SharingWeb, :live_view

  alias SharingWeb.ActionButton
  alias SharingWeb.Info
  alias SharingWeb.ItemCard
  alias SharingWeb.OmniArea

  def mount(_params, _session, socket) do
    petname = petname()

    {
      :ok,
      socket
      # code => show qr-code
      |> State.set(allow_uploads: true, debug: true, code: false)
      # code used for the sharing: e.g. huge-dingo
      |> assign(:petname, petname)
      |> assign(:uploaded_files, [])
      |> allow_upload(
        :files,
        accept: :any,
        max_entries: 100,
        max_file_size: 1_000_000_000
      )
    }
  end

  def handle_params(_params, uri, socket) do
    {
      :noreply,
      socket
      |> assign(:uri, uri)
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
  end

  defp petname() do
    {petname, 0} = System.cmd("petname", [])

    petname
    |> String.trim_trailing("\n")
  end

  defp qr_code(socket) do
    # www.sharing.org/huge-dingo -> server handles with `ServeArchiveController'
    url = socket.assigns.uri <> socket.assigns.petname
    output = code(socket)

    System.cmd("qrrs", ["-m", "0", "-o", "svg", url, output])

    socket
    |> State.set(code: true)
  end

  ### Action Button ---------------------------------------

  def handle_event("ab-show-input", _params, socket) do
    {:noreply, socket |> push_event("ab-show-input", %{})}
  end

  def handle_event("ab-show-button", _params, socket) do
    {:noreply, socket |> push_event("ab-show-button", %{})}
  end

  def handle_event("ab-show-code", _params, socket) do
    {:noreply, socket |> push_event("ab-show-code", %{})}
  end

  def handle_event("submit-code", %{"code" => code}, socket) do
    path = archive(code)

    if File.exists?(path) do
      {:reply, %{continue: true}, socket}
    else
      {:reply, %{continue: false}, socket}
    end
  end

  ### Omni Area -------------------------------------------

  def handle_event("oa-show-code", _params, socket) do
    {
      :noreply,
      socket
      |> push_event("oa-show-code", %{})
      |> State.set(code: true)
    }
  end

  def handle_event("oa-show-drop-area", _params, socket) do
    {
      :noreply,
      socket
      |> push_event("oa-show-drop-area", %{})
      |> State.set(code: false)
    }
  end

  ### File Uploads ----------------------------------------

  def handle_event("open-file-picker", _params, socket) do
    {
      :noreply,
      socket
      |> push_event("click", %{id: socket.assigns.uploads.files.ref})
    }
  end

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

  ### Save Uploads ----------------------------------------

  # Called whenever the submit button is clicked. This will eagerly
  # update some state. 
  def handle_event("submit-files", _params, socket) do
    {
      :noreply,
      socket
      |> State.set(allow_uploads: false)
      |> push_event("remove-hovering", %{})
    }
  end

  # Handles the uploads and bundles them in an archive at
  # store/<petname>.zip. This is only called after the files have been
  # uploaded which is handled in `handle_progress/3'.
  def handle_event("upload", _params, socket) do
    # The uploaded entries are temporarily stored at SHARING before
    # they are archived.
    uploaded_files =
      consume_uploaded_entries(socket, :files, fn %{path: path}, entry ->
        file =
          Path.join(
            sharing(socket),
            if(entry.client_relative_path == "",
              do: entry.client_name,
              else: entry.client_relative_path
            )
          )

        File.mkdir_p(Path.dirname(file))
        File.cp!(path, file)

        {:ok, file}
      end)

    # Zip up the sharing.
    :zip.create(
      String.to_charlist(archive(socket)),
      Enum.map(
        uploaded_files,
        &String.to_charlist(String.trim_leading(&1, sharing(socket) <> "/"))
      ),
      cwd: String.to_charlist(sharing(socket))
    )

    # Clean up the staging directory.
    File.rm_rf(sharing(socket))

    {
      :noreply,
      socket
      |> qr_code()
      # [TODO] hardcoded path
      |> push_event("inject-src", %{src: "/store/" <> socket.assigns.petname <> ".svg"})
      |> update(:uploaded_files, &(&1 ++ uploaded_files))
      |> push_event("ab-show-code", %{code: socket.assigns.petname})
      |> push_event("oa-show-code", %{})
    }
  end

  ### Cleanup ---------------------------------------------

  def terminate(_, socket) do
    if socket.assigns.code do
      File.rm(code(socket))
    end

    :ok
  end

  ### --------------------------------------------------------------------- ###
  ### View                                                                  ###
  ### --------------------------------------------------------------------- ###

  attr(:has_uploads, :boolean)
  slot(:inner_block, required: true)

  def hooks(assigns) do
    ~H"""
    <div data-has-uploads={@has_uploads}>
      <div id="state-events" phx-hook="StateEvents">
        <div id="gsap-events" phx-hook="GsapEvents">
          <div id="window-drag-events" phx-hook="WindowDragEvents">
            <%= render_slot(@inner_block) %>
          </div>
        </div>
      </div>
    </div>
    """
  end

  def render(assigns) do
    ~H"""
    <.hooks has_uploads={!Enum.empty?(@uploads.files.entries)} >
      <div class="flex flex-col gap-6">
        <div class="flex justify-between">
          <ActionButton.render petname={@petname} />
          <Info.render />
        </div>

        <!-- Omni Area -->
        <div class="flex justify-center mt-[min(max(100vw,40rem)-40rem,max(100vh,40rem)-40rem,10vh)]">
          <OmniArea.render
            input={@uploads.files.ref}
            has_uploads={!Enum.empty?(@uploads.files.entries)}>
            <.live_file_input
              class="sr-only"
              upload={@uploads.files}
            />
          </OmniArea.render>
          </div>

        <!-- Item Cards -->
        <div class={
          "mt-6 grid justify-center-safe gap-2 snap-mandatory auto-cols-[minmax(300px,400px)]"
          <> " overflow-scroll [scrollbar-width:none] [&::-webkit-scrollbar]:hidden"
          <> " md:snap-x md:grid-flow-col md:grid-rows-5"}>
          <ItemCard.render
            :for={item <- ItemCard.normalize(@uploads.files)}
            item={item}
          />
        </div>
      </div>
    </.hooks>
    """
  end
end

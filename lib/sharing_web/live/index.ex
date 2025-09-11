defmodule SharingWeb.Index do
  use SharingWeb, :live_view

  alias SharingWeb.ActionButton
  alias SharingWeb.DropArea
  alias SharingWeb.Info
  alias SharingWeb.ItemCard

  def mount(params, session, socket) do
    petname = petname()

    {
      :ok,
      socket
      |> State.set(allow_uploads: true, debug: true, code: false)
      |> assign(:petname, petname)
      |> assign(:uploaded_files, [])
      |> assign(:sharing, Path.join(Application.app_dir(:sharing, "store"), petname))
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
  end

  defp petname() do
    {petname, 0} = System.cmd("petname", [])

    petname
    |> String.trim_trailing("\n")
  end

  # defp qr_code(socket, uri) do
  #   if Map.has_key?(socket.assigns, :code) do
  #     dbg("JKL")
  #     dbg(socket.assigns)
  #     socket
  #   else
  #     url = uri <> "store/" <> socket.assigns.petname <> ".png"
  #     output = socket.assigns.sharing <> ".png"
  #     System.cmd("qrrs", ["-o", "image", url, output])

  #     socket
  #     |> assign(code: output)
  #   end
  # end

  defp qr_code(socket) do
    url = socket.assigns.uri <> socket.assigns.petname
    output = socket.assigns.sharing <> ".svg"

    System.cmd("qrrs", ["-o", "svg", url, output])

    socket
    |> assign(code_path: output)
    |> State.set(code: true)
  end

  # This handles some of the logic of `handle_event("upload", ...)'.
  # Disabled, because the entires are removed from socket once the
  # upload is finished. Because I uploaded folders at once, this leads
  # to weird jumps whenever a file is finished and the size of the
  # folder decreases.

  # defp handle_progress(:files, entry, socket) do
  #   if entry.done? do
  #     uploaded_file =
  #       consume_uploaded_entry(socket, entry, fn %{path: path} ->
  #         file =
  #           Path.join(
  #             socket.assigns.sharing,
  #             if(entry.client_relative_path == "",
  #               do: entry.client_name,
  #               else: entry.client_relative_path
  #             )
  #           )

  #         File.mkdir_p(Path.dirname(file))
  #         File.cp!(path, file)
  #         {:ok, file}
  #       end)

  #     {:noreply, socket |> update(:uploaded_files, &(&1 ++ [uploaded_file]))}
  #   else
  #     {:noreply, socket}
  #   end
  # end

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
    path =
      Path.join(Application.app_dir(:sharing, "store"), "#{code}.zip")

    if File.exists?(path) do
      {:reply, %{continue: true}, socket}
    else
      {:reply, %{continue: false}, socket}
    end
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
            socket.assigns.sharing,
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
      String.to_charlist(socket.assigns.sharing <> ".zip"),
      Enum.map(
        uploaded_files,
        &String.to_charlist(String.trim_leading(&1, socket.assigns.sharing <> "/"))
      ),
      cwd: String.to_charlist(socket.assigns.sharing)
    )

    # Clean up the staging directory.
    File.rm_rf(socket.assigns.sharing)

    {
      :noreply,
      socket
      |> qr_code()
      |> update(:uploaded_files, &(&1 ++ uploaded_files))
      |> push_event("show-code", %{code: socket.assigns.petname})
    }
  end

  ### Cleanup ---------------------------------------------

  def terminate(_, socket) do
    if Map.has_key?(socket.assigns, :code_path) do
      dbg(socket.assigns.code_path)
      File.rm(socket.assigns.code_path)
    end

    :ok
  end

  ### --------------------------------------------------------------------- ###
  ### Components                                                            ###
  ### --------------------------------------------------------------------- ###

  attr(:input, :string)
  attr(:code, :string)

  # Must contain a button with type submit!
  defp drop_area(assigns) do
    ~H"""
    <div class="mt-[min(max(100vw,40rem)-40rem,max(100vh,40rem)-40rem,10vh)]">
      <div class="flex justify-center">
        <DropArea.render
          input={@input}
          code={@code}
        />
      </div>
    </div>
    """
  end

  ### --------------------------------------------------------------------- ###
  ### View                                                                  ###
  ### --------------------------------------------------------------------- ###

  slot(:inner_block, required: true)

  def hooks(assigns) do
    ~H"""
    <div id="gsap-events" phx-hook="GsapEvents">
      <div id="state-events" phx-hook="StateEvents">
        <div id="window-drag-events" phx-hook="WindowDragEvents">
          <%= render_slot(@inner_block) %>
        </div>
      </div>
    </div>
    """
  end

  def render(assigns) do
    ~H"""
    <.hooks>
      <div data-has-uploads={!Enum.empty?(assigns.uploads.files.entries)}>
        <div class="flex flex-col gap-6">
          <div class="flex justify-between">
            <ActionButton.render code={@petname} />
            <Info.render />
          </div>
          <form
            phx-submit="upload"
            phx-change="validate">
            <.drop_area
              input={@uploads.files.ref}
              code={@petname}
            />
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
    </.hooks>
    """
  end
end

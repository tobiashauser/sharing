defmodule SharingWeb.ItemCard do
  @moduledoc """
  Renders an item.
  """
  use SharingWeb, :html

  @doc """
  Normalize `@uploads.files.entries`.

  The list of entries is flat but we want to differentiate
  between folders and files.
  """
  def normalize(upload_config) do
    upload_config.entries
    |> Enum.reduce([], fn item, acc ->
      case item do
        %{client_relative_path: ""} ->
          [%{file: item} | acc]

        %{client_relative_path: folder} ->
          folder_name =
            String.split(folder, "/", parts: 2)
            |> List.first()

          case acc do
            [%{name: name, size: size, refs: refs} | rest] when name == folder_name ->
              [%{folder: true, name: folder_name, size: size + 1, refs: [item.ref | refs]} | rest]

            _ ->
              [%{folder: true, name: folder_name, size: 1, refs: [item.ref]} | acc]
          end
      end
    end)
    |> Enum.reverse()
  end

  ### --------------------------------------------------------------------- ###
  ### View                                                                  ###
  ### --------------------------------------------------------------------- ###

  ### Symbol ----------------------------------------------

  attr(:class, :string, default: "")
  attr(:item, :map)
  attr(:size, :string, default: "size-5")

  def symbol(%{item: %{file: %{client_type: type}}} = assigns)
      when type == "text/csv" or type == "text/tsv" do
    ~H"""
    <div class={@class}>
      <span class={"hero-document-text-solid " <> @size} />
    </div>
    """
  end

  def symbol(%{item: %{file: %{client_type: type}}} = assigns)
      when type == "text/plain" do
    ~H"""
    <div class={@class}>
      <span class={"hero-document-text-solid " <> @size} />
    </div>
    """
  end

  def symbol(%{item: %{file: _}} = assigns) do
    ~H"""
    <div class={@class}>
      <span class={"hero-document-solid " <> @size} />
    </div>
    """
  end

  def symbol(%{item: %{folder: _}} = assigns) do
    ~H"""
    <div class={@class}>
      <span class={"hero-folder-solid " <> @size} />
    </div>
    """
  end

  def symbol(assigns) do
    ~H"""
    <div class={@class}>
      <span class={"hero-x-circle " <> @size} />
    </div>
    """
  end

  ### Cancel Button ---------------------------------------

  attr(:class, :string, default: "")
  attr(:item, :map)

  def cancel(%{item: %{file: _}} = assigns) do
    ~H"""
    <span
      class={@class}
      phx-click="cancel-upload"
      phx-value-ref={@item.file.ref}
    />
    """
  end

  def cancel(%{item: %{folder: _}} = assigns) do
    ~H"""
    <span
      class={@class}
      phx-click={JS.push("cancel-upload", value: %{refs: @item.refs})}
    />
    """
  end

  ### Card ------------------------------------------------

  attr(:title, :string)
  attr(:item, :map)

  def card(assigns) do
    ~H"""
    <div class= "flex items-center gap-2 p-2 rounded border-1 border-surface">
      <.symbol class="center-content p-2 rounded bg-elevated" item={@item} />
      <div class="flex justify-between items-center w-full">
        <div class="flex flex-col">
          <div class="text-sm font-medium">{@title}</div>
          <div class="text-subtle text-xs">Info</div>
        </div>
        <.cancel
          class="hero-x-circle-solid size-5 cursor-pointer text-overlay hover:text-critical/70"
          item={@item}
        />
      </div>
    </div>
    """
  end

  ### Render ----------------------------------------------

  attr(:item, :map)

  def render(%{item: %{file: _}} = assigns) do
    ~H"""
    <.card
      title={@item.file.client_name}
      item={@item}
    />
    """
  end

  def render(%{item: %{folder: _}} = assigns) do
    ~H"""
    <.card
      title={@item.name}
      item={@item}
    />
    """
  end
end

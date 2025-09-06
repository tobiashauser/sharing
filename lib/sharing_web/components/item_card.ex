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
            [%{name: name, size: size} | rest] when name == folder_name ->
              [%{folder: true, name: folder_name, size: size + 1} | rest]

            _ ->
              [%{folder: true, name: folder_name, size: 1} | acc]
          end
      end
    end)
    |> Enum.reverse()
  end

  ### --------------------------------------------------------------------- ###
  ### View                                                                  ###
  ### --------------------------------------------------------------------- ###

  attr(:item, :map)

  def render(%{item: %{file: _}} = assigns) do
    ~H"""
    <div class="bg-elevated">
      {@item.file.client_name}
    </div>
    """
  end

  def render(%{item: %{folder: _}} = assigns) do
    ~H"""
    <div class="bg-elevated">
      {@item.name} {@item.size}
    </div>
    """
  end
end

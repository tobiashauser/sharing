defmodule SharingWeb.DropArea do
  use SharingWeb, :html

  def render(assigns) do
    ~H"""
    <div
      class="h-40 max-w-md w-2/3 bg-surface rounded-xl"
      phx-click="open-file-picker">
    </div>
    """
  end
end

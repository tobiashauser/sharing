defmodule SharingWeb.DropArea do
  use SharingWeb, :html

  def render(assigns) do
    ~H"""
    <div
      class="mt-6 h-40 grid max-w-md w-2/3 rounded-xl shadow-[0px_0px_15px_3px_rgba(0,0,0,0.1)] dark:bg-elevated"
      phx-click="open-file-picker">
      <div class="m-4 center-content">
        <div class="border p-3 border-overlay rounded-full mb-3 center-content">
          <.icon name="hero-arrow-up-tray" class="size-5 text-subtle" />
        </div>
        <p class="text-sm mb-1.5 font-medium">Upload files</p>
        <p class="text-subtle text-xs">Drag & Drop or click to browser</p>
      </div>  
    </div>
    """
  end
end

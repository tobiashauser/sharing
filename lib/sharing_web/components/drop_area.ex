defmodule SharingWeb.DropArea do
  @moduledoc """
  The drop area component. This component contains some
  internal state that is needed to draw the view:

      dragging: true, if the user is dragging an item
                over the window.
      hovering: true, if the mouse is over the area

  The view state is stored as data attributes in the HTML.
  Two pseudo classes with the same names are defined in
  `app.css` and can be used to style the component.
  """
  use SharingWeb, :html

  def symbol(assigns) do
    ~H"""
    <div 
      class={
        "p-3 mb-3 center-content transition border-2 border-subtle/40 rounded-full"
        <> " dragging:border-salient dragging:bg-salient/20"
        <> " has-uploads:border-popout has-uploads:bg-popout/20 has-uploads:shadow"
        <> " allow-uploads:cursor-pointer"}>
      <svg
        xmlns="http://www.w3.org/2000/svg"
        fill="none"
        viewBox="0 0 24 24"
        stroke-width="2"
        stroke="currentColor"
        class={
          "size-5 text-subtle/70"
          <> " dragging:text-salient"
          <> " has-uploads:text-popout"}>
        <path
          id="da-tray"
          stroke-linecap="round"
          stroke-linejoin="round"
          d="M3 16.5v2.25A2.25 2.25 0 0 0 5.25 21h13.5A2.25, 2.25 0 0 0 21 18.75V16.5m-13.5-9" />
        <path
          id="da-arrow"
          class="origin-[12px_10px] dragging:-scale-100 transition"
          stroke-linecap="round"
          stroke-linejoin="round"
          d="M7.5 7.5 L12 3m0 0 4.5 4.5M12 3v13.5" />
      </svg>
    </div>
    """
  end

  attr(:input, :string)
  attr(:has_uploads, :boolean, default: true)
  # use to inject <.live_file_input />
  slot(:inner_block, required: true)

  # The drop area morphs into the qr code when the files have been
  # uploaded. This follows the same sliding doors principle as the
  # action button.
  def render(assigns) do
    ~H"""
    <form
      class="h-full w-full"
      phx-submit="upload"
      phx-change="validate">
      <label for={@input} class="allow-uploads:cursor-pointer h-full w-full">
        <div class="p-4 h-full w-full">
          <div class={
            "center-content transition h-full w-full"
            <> " allow-uploads:cursor-pointer"
            <> " border-2 border-transparent rounded-[9px] border-dashed"
            <> " hovering:border-overlay hovering:bg-elevated"
            <> " dragging:border-salient dragging:bg-salient/10"}>
            <button
              :if={@has_uploads}
              type="submit"
              phx-click="submit-files">
              <.symbol />
            </button>
            <.symbol :if={!@has_uploads} />
            <p class="text-sm mb-1.5 font-medium">Upload files</p>
            <p class="text-subtle text-xs">Drag & Drop or click to browse</p>
          </div>
        </div>
      </label>
      <%= render_slot(@inner_block) %>
    </form>
    """
  end
end

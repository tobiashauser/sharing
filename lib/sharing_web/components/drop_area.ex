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

  def render(assigns) do
    ~H"""
    <label
      for={@input}
      id="drop-area"
      phx-hook="MouseEvents"
      class={
        "relative rounded-xl shadow-[0px_0px_15px_3px_rgba(0,0,0,0.1)] overflow-hidden"
        <> " transition-[width] duration-300"
        <> " h-44 w-2/3 max-w-md"
        <> " dark:bg-elevated"
        <> " allow-uploads:cursor-pointer"}>
      <div class={
        "center-content absolute left-4 right-4 top-4 bottom-4"
        <> " border-2 border-transparent rounded-[9px] border-dashed"
        <> " transition"
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
    </label>
    """
  end
end

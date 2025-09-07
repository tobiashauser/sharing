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

  def content(assigns) do
    ~H"""
    <div class={
      "p-3 mb-3 center-content transition"
      <> " border-2 border-subtle/40 rounded-full"
      <> " dragging:border-salient dragging:bg-salient/10"
      }>
      <.icon
        name="hero-arrow-up-tray-mini"
        class="size-5 text-subtle/70 dragging:text-salient"
      />
    </div>
    <p class="text-sm mb-1.5 font-medium">Upload files</p>
    <p class="text-subtle text-xs">Drag & Drop or click to browser</p>
    """
  end

  attr(:class, :string, default: "")

  def render(assigns) do
    ~H"""
    <div
      id="drop-area"
      phx-hook="MouseEvents"
      class={"grid rounded-xl shadow-[0px_0px_15px_3px_rgba(0,0,0,0.1)] dark:bg-elevated" <> " " <> @class}
      phx-click="open-file-picker">
      <div class={
        "m-4 center-content transition" 
        <> " border-2 border-transparent rounded-[9px] border-dashed"
        <> " hovering:border-muted hovering:bg-elevated"
        <> " dragging:border-salient dragging:bg-salient/5"
      }>
        <.content />
      </div>  
    </div>
    """
  end
end

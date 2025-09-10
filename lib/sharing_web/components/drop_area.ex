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

  attr(:class, :string, default: "")
  attr(:icon, :string, default: "hero-arrow-up-tray-mini")

  def button(assigns) do
    ~H"""
    <div>
      <button
        type="submit"
        class={@class <> " not-has-uploads:sr-only"}
        phx-click="submit-files"
      >
        <span class={@icon} />
      </button>
      <div class={@class <> " has-uploads:sr-only"}>
        <span class={@icon} />
      </div>
    </div>
    """
  end

  def content(assigns) do
    ~H"""
    <.button
      class={
        "p-3 mb-3 center-content transition allow-uploads:cursor-pointer"
        <> " border-2 border-subtle/40 rounded-full"
        <> " dragging:border-salient dragging:bg-salient/20"
        <> " has-uploads:border-popout has-uploads:bg-popout/20 has-uploads:shadow"}
      icon={
        "size-5 text-subtle/70 dragging:text-salient transition"
        <> " hero-arrow-up-tray-mini"
        <> " dragging:hero-arrow-down-tray-mini"
        <> " has-uploads:text-popout"}
    />
    <p class="text-sm mb-1.5 font-medium">Upload files</p>
    <p class="text-subtle text-xs">Drag & Drop or click to browse</p>
    """
  end

  attr(:input, :string)
  attr(:class, :string, default: "")

  def render(assigns) do
    ~H"""
    <label
      for={@input}
      id="drop-area"
      phx-hook="MouseEvents"
      class={
        "grid rounded-xl shadow-[0px_0px_15px_3px_rgba(0,0,0,0.1)]"
        <> " dark:bg-elevated allow-uploads:cursor-pointer"
        <> " " <> @class}>
      <div class={
        "m-4 center-content transition" 
        <> " border-2 border-transparent rounded-[9px] border-dashed"
        <> " hovering:border-overlay hovering:bg-elevated"
        <> " dragging:border-salient dragging:bg-salient/10"
      }>
        <.content />
      </div>  
    </label>
    """
  end
end

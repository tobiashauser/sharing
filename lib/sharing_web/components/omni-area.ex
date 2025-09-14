defmodule SharingWeb.OmniArea do
  use SharingWeb, :html
  alias SharingWeb.DropArea
  alias SharingWeb.QRCode

  attr(:input, :string)
  attr(:has_uploads, :boolean, default: true)
  slot(:inner_block, required: true)

  def render(assigns) do
    ~H"""
    <div
      id="omni-area"
      phx-hook="OmniAreaEvents"
      class={"grid shadow-aurora rounded-xl overflow-hidden"
        <> " h-[12rem] w-2/3 max-w-sm sm:max-w-md"
        <> " dark:bg-elevated"}>
      <div
        id="drop-area-container"
        phx-hook="MouseEvents"
        class="center-content col-start-1 row-start-1 z-2 h-[12rem]">
        <DropArea.render input={@input} has_uploads={@has_uploads}>
          <%= render_slot(@inner_block) %>
        </DropArea.render>
      </div>
      <div
        id="qr-code-container"
        class="center-content col-start-1 row-start-1 z-1">
        <QRCode.render />
      </div>
    </div>
    """
  end
end

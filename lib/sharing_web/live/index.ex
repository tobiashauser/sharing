defmodule SharingWeb.Index do
  use SharingWeb, :live_view

  import GSAP
  alias SharingWeb.ActionButton

  def mount(_params, _session, socket) do
    {:ok, socket}
  end

  ### --------------------------------------------------------------------- ###
  ### Actions                                                               ###
  ### --------------------------------------------------------------------- ###

  def handle_event("show-input", _params, socket) do
    {:noreply, ActionButton.show_input(socket)}
  end

  def handle_event("show-button", _params, socket) do
    {:noreply, ActionButton.show_button(socket)}
  end

  def handle_event("show-code", _params, socket) do
    {:noreply, ActionButton.show_code(socket)}
  end

  ### --------------------------------------------------------------------- ###
  ### Components                                                            ###
  ### --------------------------------------------------------------------- ###

  ### Help Button -----------------------------------------

  attr(:class, :string, default: "")

  defp help_button(assigns) do
    ~H"""
    <div class={"flex-col items-center" <> " " <> @class}>
      <.icon name="hero-question-mark-circle" />
    </div>
    """
  end

  ### Drop Area -------------------------------------------

  defp drop_area(assigns) do
    ~H"""
    <div class="mt-[min(max(100vw,40rem)-40rem,max(100vh,40rem)-40rem,10vh)] bg-green-400">Drop Area</div>
    """
  end

  ### Item Cards ------------------------------------------

  defp item_cards(assigns) do
    ~H"""
    <div class="bg-red-400">Item Card</div>
    """
  end

  ### --------------------------------------------------------------------- ###
  ### View                                                                  ###
  ### --------------------------------------------------------------------- ###

  def render(assigns) do
    ~H"""
    <div class="flex justify-between">
      <ActionButton.render
        class="border-2"
      />
      <.help_button
        class="border-2"
      />
    </div>
    <.drop_area />
    <.item_cards />
    """
  end
end

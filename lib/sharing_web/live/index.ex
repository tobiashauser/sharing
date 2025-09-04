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

  ### --------------------------------------------------------------------- ###
  ### Components                                                            ###
  ### --------------------------------------------------------------------- ###

  ### Help Button -----------------------------------------

  defp help_button(assigns) do
    ~H"""
    <div class="bg-blue-400">Help</div>
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

  def handle_event("show-input", _params, socket) do
    {:noreply, ActionButton.show_input(socket)}
  end

  def handle_event("show-button", _params, socket) do
    {:noreply, ActionButton.show_button(socket)}
  end

  def handle_event("show-code", _params, socket) do
    {:noreply, ActionButton.show_code(socket)}
  end

  def handle_event("animate-2", _params, socket) do
    {
      :noreply,
      socket
      |> push_event("gsap.to", %{id: "#my-animation", vars: %{x: 100, y: 20}})
      |> push_event("gsap.to", %{id: "#my-animation", vars: %{rotation: 27}})
    }
  end

  def render(assigns) do
    ~H"""
    <div class="flex justify-between">
      <ActionButton.render />
      <.help_button />
    </div>
    <.drop_area />
    <.item_cards />
    """

    ~H"""
    <div class="m-2">
      <ActionButton.render generatedCode="promiscous-giraffe" />

      <button phx-click="show-input" class="mt-4">Show Input</button>
      <button phx-click="show-button" class="mt-4">Show Button</button>
      <button phx-click="show-code" class="mt-4">Show Code</button>
    </div>
    """
  end
end

defmodule SharingWeb.Index do
  use SharingWeb, :live_view

  def mount(_params, _session, socket) do
    {:ok, socket}
  end

  ### --------------------------------------------------------------------- ###
  ### Actions                                                               ###
  ### --------------------------------------------------------------------- ###

  ### --------------------------------------------------------------------- ###
  ### Components                                                            ###
  ### --------------------------------------------------------------------- ###

  defp action_button(assigns) do
    ~H"""
    <div class="bg-blue-400">Action Button</div>
    """
  end

  defp help_button(assigns) do
    ~H"""
    <div class="bg-blue-400">Help</div>
    """
  end

  defp drop_area(assigns) do
    ~H"""
    <div class="mt-[min(max(100vw,40rem)-40rem,max(100vh,40rem)-40rem,10vh)] bg-green-400">Drop Area</div>
    """
  end

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
      <.action_button />
      <.help_button />
    </div>
    <.drop_area />
    <.item_cards />
    """

    ~H"""
    <.action_button />
    """
  end
end

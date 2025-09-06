defmodule SharingWeb.Info do
  use SharingWeb, :html

  ### --------------------------------------------------------------------- ###
  ### Actions                                                               ###
  ### --------------------------------------------------------------------- ###

  defp show_modal() do
    SharingWeb.CoreComponents.show_modal("info-modal")
  end

  ### --------------------------------------------------------------------- ###
  ### View                                                                  ###
  ### --------------------------------------------------------------------- ###

  attr(:class, :string, default: "")

  def render(assigns) do
    ~H"""
    <div 
      id="show-info-dialog" 
      class={"" <> " " <> @class}
      phx-click={show_modal()}>
      <.icon name="hero-information-circle" />
      <.modal id="info-modal">
        This is a modal.
      </.modal>
    </div>
    """
  end
end

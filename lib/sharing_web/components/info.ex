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
      class={
        "border px-2 center-content border-surface/60 rounded text-subtle"
        <> " bg-surface/60 hover:bg-surface"
        <> " hover:text-foreground cursor-pointer"
        <> " dark:bg-transparent dark:hover:bg-surface/60"
        <> " " <> @class
      }
      phx-click={show_modal()}>
      <span class="hero-information-circle-solid size-4" />
      <.modal id="info-modal">
        This is a modal.
      </.modal>
    </div>
    """
  end
end

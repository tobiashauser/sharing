defmodule SharingWeb.ActionButton do
  @moduledoc """
  The components that build up the action button.
  """
  use SharingWeb, :html

  ### --------------------------------------------------------------------- ###
  ### View                                                                  ###
  ### --------------------------------------------------------------------- ###

  defp input(assigns) do
    ~H"""
    <div id="ab-input" class="center-content w-0">
      <input
        id="ab-input-content"
        type="text"
        placeholder="Enter Code"
        class={"transition py-1 px-2 invisible opacity-0"
          <> " focus:outline-none"
          <> " invalid:text-critical"} /> 
    </div>
    """
  end

  defp download(assigns) do
    ~H"""
    <div
      id="ab-download"
      class={"center-content w-0 cursor-pointer text-subtle transition bg-surface/60"
        <> " hover:text-foreground hover:not-invalid:bg-surface"
        <> " invalid:bg-critical/60"}>
      <span
        id="ab-download-content"
        class={"hero-arrow-down-tray-micro focus:outline-none mx-2 invisible opacity-0"
          <> " focus:outline-none"
          <> " transition invalid:text-critical"} />
    </div>
    """
  end

  defp button(assigns) do
    ~H"""
    <button
      id="ab-enter-code"
      class={"center-content text-subtle cursor-pointer"
        <> " hover:text-foreground hover:bg-surface"
        <> " not-dark:bg-surface/60"}
      phx-click="show-input">
      <span
        id="ab-enter-code-content"
        class="px-2">
        Enter Code
      </span>
    </button>
    """
  end

  defp label(assigns) do
    ~H"""
    <div
      id="ab-code-label"
      class="center-content bg-sruface/60 w-0">
      <span
        id="ab-code-label-content"
        class={"invisible opacity-0 px-2 font-normal text-foreground/80"
          <> " focus:outline-none"}>
        Code
      </span>
    </div>
    """
  end

  attr(:petname, :string, required: true)

  defp code(assigns) do
    ~H"""
    <div id="ab-code" class="center-content w-0">
      <span
        id="ab-code-content"
        class={"invisible opacity-0 px-2 text-popout"
          <> " focus:outline-none"}>
        {@petname}
      </span>
    </div>
    """
  end

  attr(:petname, :string, default: "No code provided")

  def render(assigns) do
    ~H"""
    <div
      id="action-button"
      phx-hook="ActionButtonEvents"
      class="flex justify-start font-medium text-sm">
      <div class={"inline-flex truncate text-clip border transition border-surface/60 rounded"
        <> " invalid:border-critical"}>
        <.input />
        <.download />
        <.button />
        <.label />
        <.code petname={@petname} />
      </div>
    </div>
    """
  end
end

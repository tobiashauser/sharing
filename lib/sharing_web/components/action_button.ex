defmodule SharingWeb.ActionButton do
  @moduledoc """
  The components that build up the action button.
  """
  use SharingWeb, :html

  ### --------------------------------------------------------------------- ###
  ### View                                                                  ###
  ### --------------------------------------------------------------------- ###

  attr(:class, :string, default: "")
  attr(:contentClass, :string, default: "")

  defp action_button_input(assigns) do
    ~H"""
    <div
      id="ab-input"
      class={"center-content " <> @class} >
      <input
        class={@contentClass}
        id="ab-input-content"
        type="text"
        placeholder="Enter Code"
      /> 
    </div>
    """
  end

  attr(:class, :string, default: "")
  attr(:contentClass, :string, default: "")

  defp action_button_download(assigns) do
    ~H"""
    <div
      id="ab-download"
      class={"center-content" <> " " <> @class}>
      <span
        id="ab-download-content"
        class={"hero-arrow-down-tray-micro" <> " " <> @contentClass}
      />
    </div>
    """
  end

  attr(:class, :string, default: "")
  attr(:contentClass, :string, default: "")

  defp action_button_enter_code(assigns) do
    ~H"""
    <button
      id="ab-enter-code"
      class={"center-content " <> @class}
      phx-click="show-input">
      <span
        id="ab-enter-code-content"
        class={@contentClass}>
        Enter Code
      </span>
    </button>
    """
  end

  attr(:class, :string, default: "")
  attr(:contentClass, :string, default: "")

  defp action_button_code_label(assigns) do
    ~H"""
    <div
      id="ab-code-label"
      class={"center-content " <> @class}>
      <span
        id="ab-code-label-content"
        class={@contentClass}>
        Code
      </span>
    </div>
    """
  end

  attr(:class, :string, default: "")
  attr(:contentClass, :string, default: "")
  attr(:generatedCode, :string, required: true)

  defp action_button_code(assigns) do
    ~H"""
    <div
      id="ab-code"
      class={"center-content " <> @class}>
      <span
        id="ab-code-content"
        class={@contentClass}>
        {@generatedCode}
      </span>
    </div>
    """
  end

  attr(:class, :string, default: "")
  attr(:generatedCode, :string, default: "No code provided")

  def render(assigns) do
    ~H"""
    <div
      id="action-button"
      phx-hook="ActionButtonEvents"
      class="flex justify-start font-medium text-sm">
      <div class={"inline-flex truncate text-clip border border-surface/60 rounded" <> " " <> @class}>
        <.action_button_input
          class="w-0"
          contentClass="focus:outline-none py-1 px-2 invisible opacity-0"
        />
        <.action_button_download
          class={
            "w-0 cursor-pointer text-subtle"
            <> " bg-surface/60"
            <> " hover:text-foreground hover:bg-surface"
          }
          contentClass="focus:outline-none mx-2 invisible opacity-0"
        />
        <.action_button_enter_code
          class={
            "text-subtle cursor-pointer"
            <> " not-dark:bg-surface/60"
            <> " hover:text-foreground hover:bg-surface"
          }
          contentClass="px-2"
        />
        <.action_button_code_label
          class="bg-surface/60 w-0"
          contentClass="invisible opacity-0 focus:outline-none px-2"
        />
        <.action_button_code
          class="w-0"
          contentClass="invisible opacity-0 focus:outline-none px-2"
          generatedCode={@generatedCode}
        />
      </div>
    </div>
    """
  end
end

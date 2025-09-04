defmodule SharingWeb.ActionButton do
  @moduledoc """
  The components that build up the action button.
  """
  use SharingWeb, :html

  import GSAP

  ### --------------------------------------------------------------------- ###
  ### Actions                                                               ###
  ### --------------------------------------------------------------------- ###

  def show_input(socket) do
    gsap_timeline()
    |> timeline_to(
      ~w(#ab-enter-code-content #ab-code-label-content #ab-code-content),
      %{autoAlpha: 0},
      "hide"
    )
    |> timeline_to(
      ~w(#ab-enter-code #ab-code-label #ab-code),
      %{width: 0},
      "move"
    )
    |> timeline_to(
      ~w(#ab-input #ab-download),
      %{width: "auto"},
      "move"
    )
    |> timeline_to(
      ~w(#ab-input-content #ab-download-content),
      %{autoAlpha: 1},
      "show"
    )
    |> play_timeline(socket)
  end

  def show_button(socket) do
    gsap_timeline()
    |> timeline_to(
      ~w(#ab-input-content #ab-download-content #ab-code-label-content #ab-code-content),
      %{autoAlpha: 0},
      "hide"
    )
    |> timeline_to(
      ~w(#ab-input #ab-download #ab-code-label #ab-code),
      %{width: 0},
      "move"
    )
    |> timeline_to(
      "#ab-enter-code",
      %{width: "auto"},
      "move"
    )
    |> timeline_to(
      "#ab-enter-code-content",
      %{autoAlpha: 1},
      "show"
    )
    |> play_timeline(socket)
  end

  def show_code(socket) do
    gsap_timeline()
    |> timeline_to(
      ~w(#ab-input-content #ab-download-content #ab-enter-code-content),
      %{autoAlpha: 0},
      "hide"
    )
    |> timeline_to(
      ~w(#ab-input #ab-download #ab-enter-code),
      %{width: 0},
      "move"
    )
    |> timeline_to(
      ~w(#ab-code-label #ab-code),
      %{width: "auto"},
      "move"
    )
    |> timeline_to(
      ~w(#ab-code-label-content #ab-code-content),
      %{autoAlpha: 1},
      "show"
    )
    |> play_timeline(socket)
  end

  ### --------------------------------------------------------------------- ###
  ### View                                                                  ###
  ### --------------------------------------------------------------------- ###

  attr(:class, :string, default: "")
  attr(:contentClass, :string, default: "")

  defp action_button_input(assigns) do
    ~H"""
    <div
      id="ab-input"
      class={@class}>
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
      class={@class}>
      <.icon
        id="ab-download-content"
        class={"flex items-center" <> " " <> @contentClass}
        name="hero-arrow-down-tray-micro"
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
      class={@class}>
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
      class={@class}>
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
      class={@class}>
      <span
        id="ab-code-content"
        class={@contentClass}>
        {@generatedCode}
      </span>
    </div>
    """
  end

  attr(:generatedCode, :string, default: "No code provided")

  def render(assigns) do
    ~H"""
    <div class="flex justify-start">
      <div class="border-2 rounded-sm items-center inline-flex text-sm font-semibold truncate text-clip">
        <.action_button_input
          class="w-0"
          contentClass="invisible opacity-0 focus:outline-none"
        />
        <.action_button_download
          class="w-0"
          contentClass="invisible opacity-0 focus:outline-none"
        />
        <.action_button_enter_code
          class="cursor-pointer"
        />
        <.action_button_code_label
          class="w-0"
          contentClass="invisible opacity-0 focus:outline-none"
        />
        <.action_button_code
          class="w-0"
          contentClass="invisible opacity-0 focus:outline-none"
          generatedCode={@generatedCode}
        />
      </div>
    </div>
    """
  end
end

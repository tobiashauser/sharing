defmodule GSAP do
  @moduledoc """
  A composable interface to GSAP.

  Make sure to import `assets/js/gsap.js` to execute the
  client side code.
  """
  use Phoenix.LiveView

  def gsap_scene() do
    %{}
  end

  def gsap_timeline() do
    []
  end

  def timeline_to(timeline, targets, vars, position) do
    timeline ++ [%{cons: "to", targets: targets, vars: vars, position: position}]
  end

  def gsap_to(socket, targets, vars) do
    push_event(socket, "gsap.to", %{targets: targets, vars: vars})
  end

  def play_timeline(timeline, socket) do
    push_event(socket, "gsap.timeline", %{timeline: timeline})
  end
end

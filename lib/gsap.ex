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
    %{
      config: %{
        onStart: [],
        onComplete: []
      },
      stages: []
    }
  end

  def timeline_to(timeline, targets, vars, position) do
    # timeline ++ [%{cons: "to", targets: targets, vars: vars, position: position}]
    Map.update(
      timeline,
      :stages,
      [%{cons: "to", targets: targets, vars: vars, position: position}],
      fn acc ->
        acc ++ [%{cons: "to", targets: targets, vars: vars, position: position}]
      end
    )
  end

  def gsap_to(socket, targets, vars) do
    push_event(socket, "gsap.to", %{targets: targets, vars: vars})
  end

  def play_timeline(timeline, socket) do
    push_event(socket, "gsap.timeline", timeline)
  end

  def on_complete(timeline, receiver) do
    Map.update(timeline, :config, %{onComplete: [receiver]}, fn acc ->
      Map.update(acc, :onComplete, [receiver], fn acc ->
        acc ++ [receiver]
      end)
    end)
  end

  def on_start(timeline, receiver) do
    Map.update(timeline, :config, %{onStart: [receiver]}, fn acc ->
      Map.update(acc, :onStart, [receiver], fn acc ->
        acc ++ [receiver]
      end)
    end)
  end
end

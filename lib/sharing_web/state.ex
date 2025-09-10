defmodule SharingWeb.State do
  @moduledoc """
  Simple wrapper to share state between the the client and server.
  Works in tandem with the javascript file `state.js'.
  """
  use Phoenix.LiveView

  @doc """
  Push state changes from the server to the client while also updating
  on the server. The state will also be mirrored in a data attribute.

  The state should be considered read-only on the client. Any changes
  are not synced back to the server (yet).

  Do not use this function to set large pieces of data. 
  """

  def set(socket, opts \\ []) when is_list(opts) do
    socket =
      Enum.reduce(opts, socket, fn {k, v}, acc -> assign(acc, k, v) end)

    socket
    |> push_event("set-state", Map.new(opts))
  end
end

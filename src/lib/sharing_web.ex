defmodule SharingWeb do
  @moduledoc """
  This module defines the entrypoint into the web interface, such as
  controllers and components.

  Do NOT define functions inside the quoted expressions
  below. Instead, define additional modules and import
  those modules here.
  """

  def router do
    quote do
      use Phoenix.Router, helpers: false

      # Import common connection and controller functions to use in pipelines.
      import Plug.Conn
      import Phoenix.Controller
      # import Phoenix.LiveView.Router
    end
  end
end

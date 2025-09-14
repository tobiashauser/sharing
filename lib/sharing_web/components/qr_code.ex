defmodule SharingWeb.QRCode do
  use SharingWeb, :html

  def render(assigns) do
    ~H"""
    <img
      id="qr-code"
      class="object-contain opacity-0 invisible p-4 bg-white"
      phx-hook="QRCodeEvents"
    />
    """
  end
end

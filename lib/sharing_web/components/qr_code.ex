defmodule SharingWeb.QRCode do
  use SharingWeb, :html

  def render(assigns) do
    ~H"""
    <div class= "p-4 rounded-xl shadow-[0px_0px_15px_3px_rgba(0,0,0,0.1)] bg-white">
      <img
        id="qr-code"
        class="object-fit"
        phx-hook="QRCodeEvents"
      />
    </div>
    """
  end
end

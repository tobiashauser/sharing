defmodule SharingWeb.QRCode do
  use SharingWeb, :html

  def render(assigns) do
    ~H"""
    <div class= "p-4 bg-white">
      <img
        id="qr-code"
        class="object-fit"
        phx-hook="QRCodeEvents"
        src="/store/test-pet.svg"
      />
    </div>
    """
  end
end

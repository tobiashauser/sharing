defmodule SharingWeb.Helpers do
  @doc """
  Return the path to a sharing for the given petname.
  """
  def sharing(%{assigns: %{petname: petname}}) do
    sharing(petname)
  end

  def sharing(petname) do
    Path.join(Application.app_dir(:sharing, "store"), petname)
  end

  @doc """
  Returns the path of the archive for the given petname.
  """
  def archive(%{assigns: %{petname: petname}}) do
    archive(petname)
  end

  def archive(petname) do
    sharing(petname) <> ".zip"
  end

  @doc """
  Returns the path to the qr code for the given petname.
  """
  def code(%{assigns: %{petname: petname}}) do
    code(petname)
  end

  def code(petname) do
    sharing(petname) <> ".svg"
  end
end

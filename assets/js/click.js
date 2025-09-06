// Simulate a click event initiated from the server.

window.addEventListener("phx:click", (e) => {
  const target = document.getElementById(e.detail.ref)
  target.click()
})

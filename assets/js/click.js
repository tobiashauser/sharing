// Simulate a click event initiated from the server.

window.addEventListener("phx:click", (e) => {
  const target = document.getElementById(e.detail.id)
  target.click()
})

window.addEventListener("phx:focus", (e) => {
  const target = document.getElementById(e.detail.id)
  target.focus()
})

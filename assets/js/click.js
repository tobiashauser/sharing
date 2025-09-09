// Simulate a click event initiated from the server.

window.addEventListener("phx:click", (e) => {
  const target = document.getElementById(e.detail.id)
  console.log("Clicking =>", target)
  // target.click()
  const click = new Event("click")
  target.dispatchEvent(click)
  target.click()
})

window.addEventListener("phx:focus", (e) => {
  const target = document.getElementById(e.detail.id)
  target.focus()
})

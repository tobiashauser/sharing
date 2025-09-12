// Simulate a click event initiated from the server.
import { log } from "./state"

window.addEventListener("phx:click", (e) => {
  const target = document.getElementById(e.detail.id)
  log("[click]", target)
  target.click()
})

window.addEventListener("phx:focus", (e) => {
  const target = document.getElementById(e.detail.id)
  log("[focus]", target)
  target.focus()
})

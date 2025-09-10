// Simulate a click event initiated from the server.
import { log } from "./state"

window.addEventListener("phx:click", (e) => {
  const target = document.getElementById(e.detail.id)
  log("Clicking", target)
  target.click()
  // const click = new Event("click")
  // target.dispatchEvent(click)
  // target.click()
})

window.addEventListener("phx:focus", (e) => {
  const target = document.getElementById(e.detail.id)
  log("Focusing", target)
  target.focus()
})

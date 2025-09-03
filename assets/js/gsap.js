// A small wrapper around gsap that triggers animations coming from the server.
import gsap from "../vendor/gsap";

window.addEventListener("phx:gsap.to", (e) => {
  gsap.to(e.detail.id, e.detail.vars)
})

// A small wrapper around gsap that triggers animations coming from the server.
import gsap from "../vendor/gsap";

window.addEventListener("phx:gsap.to", (e) => {
  gsap.to(e.detail.targets, e.detail.vars)
})

window.addEventListener("phx:gsap.timeline", (e) => {
  timeline = gsap.timeline()

  e.detail.timeline.forEach(stage => {
    timeline[stage.cons](stage.targets, stage.vars, stage.position)
  })
})

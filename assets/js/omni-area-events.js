// Animations of the omni area.
import { ID, Scene } from "./gsap"
import { log } from "./state"

export default OmniAreaEvents = {
  mounted() {
    log("Listening to omni area events")

    this.omniArea = new ID("omni-area")

    this.dropAreaContainer = new ID("drop-area-container")
    this.dropArea = new ID("drop-area")

    this.codeContainer = new ID("qr-code-container")
    this.code = new ID("qr-code")

    const hide = { autoAlpha: 0, "z-index": 1 };
    const show = { autoAlpha: 1, "z-index": 2 };

    const showCode = new Scene()
      .stage("hide", [
        this.dropAreaContainer.to(hide),
        this.dropArea.to(hide)
      ])
      .stage("resize", [
        this.omniArea.to({ width: "auto", height: "auto" })
      ])
      .stage("show", [
        this.codeContainer.to(show),
        this.code.to(show),
      ])

    // This animation looks janky. It's only kept for testing. In
    // production, transition back from QR-Code to the drop area is
    // never needed.
    const showDropArea = new Scene()
      .stage("hide", [
        this.codeContainer.to(hide),
        this.code.to(hide),
      ])
      .stage("resize", [
        // The trick is to set the width to 100% and provide a maximum
        // width separately. This only works one-way: from the 100%.
        this.omniArea.to({ width: "100%", height: "12rem" })
      ])
      .stage("show", [
        this.dropAreaContainer.to(show),
        this.dropArea.to(show)
      ])

    this.handleShowCode = (_) => {
      showCode.animate()
    }

    this.handleShowDropArea = (_) => {
      showDropArea.animate()
    }

    window.addEventListener("phx:oa-show-code", this.handleShowCode)
    window.addEventListener("phx:oa-show-drop-area", this.handleShowDropArea)
  },

  destroyed() {
    window.removeEventListener("phx:oa-show-code", this.handleShowCode)
    window.removeEventListener("phx:oa-show-drop-area", this.handleShowDropArea)
  }
}

import { log } from "./state"

export default QRCodeEvents = {
  mounted() {
    console.log("Listening to qr code events on", this.el)

    this.handleInjectSrc = (e) => {
      const src = e.detail.src
      log("Injecting QR code", src)
      this.el.src = src
    }

    window.addEventListener("phx:inject-src", this.handleInjectSrc)
  },

  destroyed() {
    window.removeEventListener("phx:inject-src", this.handleInjectSrc)
  }
}

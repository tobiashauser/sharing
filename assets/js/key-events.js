export default KeyEvents = {
  mounted() {
    console.log("Listening to key events on", this.el)

    this.handleKeyDown = (e) => {
      if (e.key == "Escape" || e.key == "Enter") {
        console.log(this.el)
        this.pushEvent("keydown", {key: e.key})
      }
    }

    this.el.addEventListener("keydown", this.handleKeyDown)
  },

  destroyed() {
    this.el.removeEventListener("keydown", this.handleKeyDown)
  }
}

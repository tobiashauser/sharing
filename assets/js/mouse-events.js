// A hook that controls data attributes regarding mouse events.
export default MouseEvents = {
  mounted() {
    console.log("Listening to mouse events on", this.el)
    let disabled = false;
    
    this.handleMouseEnter = (_) => {
      if (disabled) return
      this.el.setAttribute("data-hovering", "true")
    };

    this.handleMouseLeave = (_) => {
      if (disabled) return
      this.el.setAttribute("data-hovering", "false")
    };

    // Does this work?
    this.handleDisableMouseEvents = (e) => {
      disabled = e.detail.val;
      if (disabled) {
        this.el.setAttribute("data-hovering", "false")
      }
      console.log("Disabling mouse events ==>", disabled)
    }

    this.el.addEventListener("mouseenter", this.handleMouseEnter);
    this.el.addEventListener("mouseleave", this.handleMouseLeave);
    window.addEventListener("phx:disable-mouse-events", this.handleDisableMouseEvents)
  },

  destroyed() {
    this.el.removeEventListener("mouseenter", this.handleMouseEnter);
    this.el.removeEventListener("mouseleave", this.handleMouseLeave);
    window.removeEventListener("phx:disable-mouse-events", this.handleDisableMouseEvents)
  }
}

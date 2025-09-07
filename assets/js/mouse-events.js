// A hook that controls data attributes regarding mouse events.
export default MouseEvents = {
  mounted() {
    console.log("Listening to mouse events on", this.el)
    
    this.handleMouseEnter = (_) => {
      this.el.setAttribute("data-hovering", "true")
    };

    this.handleMouseLeave = (_) => {
      this.el.setAttribute("data-hovering", "false")
    };

    this.el.addEventListener("mouseenter", this.handleMouseEnter);
    this.el.addEventListener("mouseleave", this.handleMouseLeave);
  },

  destroyed() {
    this.el.removeEventListener("mouseenter", this.handleMouseEnter);
    this.el.removeEventListener("mouseleave", this.handleMouseLeave);
  }
}

// A hook that controls data attributes regarding mouse events.
import { state, log } from "./state"

export default MouseEvents = {
  mounted() {
    log("Listening to mouse events on", this.el)
    
    this.handleMouseEnter = (_) => {
      if (state.allowUploads) return;
      log("mouseenter", "#" + this.el.id)
      this.el.setAttribute("data-hovering", "true")
    };

    this.handleMouseLeave = (_) => {
      if (state.allowUploads) return
      log("mouseleave", "#" + this.el.id)
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

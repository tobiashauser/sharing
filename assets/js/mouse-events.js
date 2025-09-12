// A hook that controls data attributes regarding mouse events.
import { state, log } from "./state"

export default MouseEvents = {
  mounted() {
    console.log("Listening to mouse events on", this.el)
    
    this.handleMouseEnter = (_) => {
      if (!state.allow_uploads) return;
       
      log("mouseenter", "#" + this.el.id)
      this.el.setAttribute("data-hovering", "true")
    };

    this.handleMouseLeave = (_) => {
      if (!state.allow_uploads) return;

      log("mouseleave", "#" + this.el.id)
      this.el.setAttribute("data-hovering", "")
    };

    // Since the upload button is inside the drop area state must be
    // cleanup up manually.
    this.handleRemoveHovering = (_) => {
      log("remove-hovering", "#" + this.el.id)
      this.el.setAttribute("data-hovering", "")
    }

    this.el.addEventListener("mouseenter", this.handleMouseEnter);
    this.el.addEventListener("mouseleave", this.handleMouseLeave);
    window.addEventListener("phx:remove-hovering", this.handleRemoveHovering);
  },

  destroyed() {
    this.el.removeEventListener("mouseenter", this.handleMouseEnter);
    this.el.removeEventListener("mouseleave", this.handleMouseLeave);
    window.removeEventListener("phx:remove-hovering", this.handleRemoveHovering);
  }
}

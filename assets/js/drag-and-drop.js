// Hook up some basic event listeners to the window.
//
// For documentation of the lifecycle (and why `pushEvent` is in
// scope) see:
// https://hexdocs.pm/phoenix_live_view/js-interop.html#client-hooks-via-phx-hook

// Keep track of a little internal state to work around the event
// triggers for every element.
let state = 0

export default WindowDragEvents = {
  mounted() {
    state = 0;
    
    this.handleDragEnter = (event) => {
      event.preventDefault();
      event.stopPropagation();
      state++;

      if (state == 1) {
	this.pushEvent("dragenter");
      }
    };

    this.handleDragLeave = (event) => {
      event.preventDefault();
      event.stopPropagation();
      state--;

      if (state == 0) {
	this.pushEvent("dragleave");
      };
    };

    this.handleDragOver = (event) => {
      event.preventDefault();
      event.stopPropagation();
    };

    this.handleDrop = (event) => {
      event.preventDefault();
      event.stopPropagation();
      state = 0;

      let file = event.dataTransfer.files[0]
      this.upload("avatar", file)

      // The drag event has ended.
      this.pushEvent("dragleave");
    };

    window.addEventListener("dragenter", this.handleDragEnter);
    window.addEventListener("dragleave", this.handleDragLeave);
    window.addEventListener("dragover", this.handleDragOver);
    window.addEventListener("drop", this.handleDrop);
  },

  destroyed() {
    window.removeEventListener("dragenter", this.handleDragEnter);
    window.removeEventListener("dragleave", this.handleDragLeave);
    window.removeEventListener("dragover", this.handleDragOver);
    window.removeEventListener("drop", this.handleDrop);
  }
};

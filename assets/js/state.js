// Phoenix will overwrite the attributes set above on rerenders. Add a
// custom prefix to the livesocket, to persist the attributes. See
// https://hexdocs.pm/phoenix_live_view/js-interop.html, especcially
// the `onBeforeElUpdated` function.

// Implement client state as a singleton.
export let state = {};

export default state;

// Should be mounted close to the root.
export const StateEvents = {
  mounted() {
     log("Listening to state events")

    this.handleSetAttribute = (e) => {
      Object.assign(state, e.detail)

      for (const [k, v] of Object.entries(e.detail)) {
        this.el.setAttribute("data-state-"+k, v)
      }

      log("Updating state", e.detail)
    }

    window.addEventListener("phx:set-state", this.handleSetAttribute)
  },

  destroyed() {
    window.removeEventListener("phx:set-state", this.handleSetAttribute)
  }
}

export function log(...args) {
  if (state.debug) {
     console.log(...args)
  }
}


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
        this.el.setAttribute("data-"+k, v)
      }

       log("State", state)
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


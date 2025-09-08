// A small wrapper around gsap that triggers animations initiated by
// the server.
import gsap from "../vendor/gsap";

export default GsapEvents = {
  mounted () {
    console.log("Listening to gsap events")
    
    this.handleGsapTo = (e) => {
      gsap.to(e.detail.targets, e.detail.vars)
    }

    this.handleGsapTimeline = (e) => {
      const config = e.detail.config
      const stages = e.detail.stages

      timeline = gsap.timeline({
        onStart: () => {
          config.onStart.forEach(receiver => {
            this.pushEvent(receiver)
          })
        },
        onComplete: () => {
          config.onComplete.forEach(receiver => {
            this.pushEvent(receiver)
          })
        }
      })

      stages.forEach(stage => {
        timeline[stage.cons](stage.targets, stage.vars, stage.position)
      })
    }

    window.addEventListener("phx:gsap.to", this.handleGsapTo)
    window.addEventListener("phx:gsap.timeline", this.handleGsapTimeline)
  },

  destroyed() {
    window.removeEventListener("phx:gsap.to", this.handleGsapTo)
    window.removeEventListener("phx:gsap.timeline", this.handleGsapTimeline)
  }
}

// Provide also a wrapper for JavaScript.

// type Callbacks = Map<gsap.CallbackType, gsap.Callback>;

// type Animation = {
//   constructor: (
//     targets: gsap.TweenTarget,
//     vars: gsap.TweenVars,
//   ) => gsap.core.Tween;
//   targets: gsap.TweenTarget;
//   vars: gsap.TweenVars;
//   position?: gsap.Position;
// };

export class Scene {
  _animations = [];

  // Keep track of the animations up to which `ease' has been applied.
  _easeIdx = undefined;

  // A timeline object can have various callbacks.
  _callbacks = new Map();

  stage(name, animations = [], vars = {},) {
    animations.forEach((animation) => {
      animation.position = name;
      animation.vars = { ...vars, ...animation.vars };
      this._animations.push(animation);
    });
    return this;
  }

  // Applies EASE to all added animations since the last call to `withEase'.
  withEase(ease) {
    if (this._animations.length == 0) return this;

    while ((this._easeIdx ?? 0) < this._animations.length) {
      this._animations[this._easeIdx ?? 0].vars = {
        ease: ease,
        ...this._animations[this._easeIdx ?? 0].vars,
      };
      this._easeIdx = (this._easeIdx ?? 0) + 1;
    }

    return this;
  }

  // Add a callback to the created timeline.
  callback(event, callback) {
    this._callbacks.set(event, callback);
  }

  // Provide various callback types.
  onComplete(callback) {
    this.callback("onComplete", callback);
    return this;
  }

  onStart(callback) {
    this.callback("onStart", callback);
    return this;
  }

  // Run the scene.
  animate() {
    runAnimations(this._animations, this._callbacks);
  }
}

// Actually use a class to provide proper typing.
export class ID {
  _value;

  constructor(value) {
    this._value = value || `_${crypto.randomUUID()}`;
  }

  get id() {
    return this._value;
  }

  get selector() {
    return `#${this._value}`;
  }

  get element() {
    return document.getElementById(this.id);
  }

  // A simple wrapper around `gsap.to'.
  to(vars, position, predicate = () => true) {
    return {
      constructor: gsap.to,
      targets: this.selector,
      vars: vars,
      position: position,
      predicate: predicate
    };
  }
}

// A convenience function that applies state.
export function runAnimations(animations, callbacks) {
  const tl = gsap.timeline();

  // Add the callbacks to the timeline.
  if (callbacks) {
    for (const [event, callback] of callbacks) {
      tl.eventCallback(event, callback);
    }
  }

  // Add the animations to the timeline.
  for (const { constructor, targets, vars, position, predicate = () => true } of animations) {
    if (predicate()) {
      const tween = constructor(targets, vars);
      tl.add(tween, position);
    }
  }
}

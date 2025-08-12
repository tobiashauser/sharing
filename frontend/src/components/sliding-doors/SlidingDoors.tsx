import gsap from "gsap";
import { JSX } from "solid-js";

type Callbacks = Map<gsap.CallbackType, gsap.Callback>;

type Animation = {
  constructor: (
    targets: gsap.TweenTarget,
    vars: gsap.TweenVars,
  ) => gsap.core.Tween;
  targets: gsap.TweenTarget;
  vars: gsap.TweenVars;
  position?: gsap.Position;
};

export class Scene {
  private _animations: Animation[] = [];

  // Keep track of the animations up to which `ease' has been applied.
  private _easeIdx: number | undefined = undefined;

  // A timeline object can have various callbacks.
  private _callbacks: Callbacks = new Map();

  stage(
    name: string,
    animations: Animation[] = [],
    vars: gsap.TweenVars = {},
  ): Scene {
    animations.forEach((animation) => {
      animation.position = name;
      animation.vars = { ...vars, ...animation.vars };
      this._animations.push(animation);
    });
    return this;
  }

  // Applies EASE to all added animations since the last call to `withEase'.
  withEase(ease: string): Scene {
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
  private callback(event: gsap.CallbackType, callback: gsap.Callback) {
    this._callbacks.set(event, callback);
  }

  // Provide various callback types.
  onComplete(callback: gsap.Callback): Scene {
    this.callback("onComplete", callback);
    return this;
  }

  animate() {
    runAnimations(this._animations, this._callbacks);
  }
}

// Actually use a class to provide proper typing.
export class ID {
  private readonly _value: string;

  constructor() {
    this._value = `_${crypto.randomUUID()}`;
  }

  get id(): string {
    return this._value;
  }

  get selector(): string {
    return `#${this._value}`;
  }

  get element(): HTMLElement | null {
    return document.getElementById(this.selector);
  }

  // A simple wrapper around `gsap.to'.
  to(vars: gsap.TweenVars, position?: gsap.Position): Animation {
    return {
      constructor: gsap.to,
      targets: this.selector,
      vars: vars,
      position: position,
    };
  }
}

// A convenience function that applies state.
export function runAnimations(animations: Animation[], callbacks?: Callbacks) {
  const tl = gsap.timeline();

  // Add the callbacks to the timeline.
  if (callbacks) {
    for (const [event, callback] of callbacks) {
      tl.eventCallback(event, callback);
    }
  }

  // Add the animations to the timeline.
  for (const { constructor, targets, vars, position } of animations) {
    const tween = constructor(targets, vars);
    tl.add(tween, position);
  }
}

interface SlidingDoorsAttributes {
  children?: JSX.Element;
  fix?: "left" | "center" | "right";
}

// Unfortunately, providing an initial frame with `setFrame' leads to
// to jumps in each last frame. Put the <SlidingDoors> in a flex
// container so that it will only take up as much horizontal space as
// needed.
export function SlidingDoors(props: SlidingDoorsAttributes): JSX.Element {
  const { children, fix } = props;
  return (
    <div
      classList={{
        flex: true,
        "border-blue-400 border-2": false,
        "justify-start": fix === "left",
        "justify-center": fix === "center",
        "justify-end": fix === "right",
      }}
    >
      {children}
    </div>
  );
}

import gsap from "gsap";
import { createSignal, JSX } from "solid-js";

export type Frame = Map<gsap.TweenTarget | ID, gsap.TweenVars>;

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
}

// A convenience constructure for STATE.
export function createFrame(build: (frame: Frame) => void): Frame {
  const frame = new Map<gsap.TweenTarget | ID, gsap.TweenVars>();
  build(frame);
  return frame;
}

// A convenience function that applies state.
function modulateFrame(
  modulate: (target: gsap.TweenTarget, vars: gsap.TweenVars) => void,
) {
  return (frame: Frame) => {
    for (const [target, vars] of frame) {
      modulate(target instanceof ID ? target.selector : target, vars);
    }
  };
}

export function setFrame(frame: Frame) {
  return () => {
    modulateFrame(gsap.set)(frame);
  };
}

export function toFrame(frame: Frame) {
  return () => {
    modulateFrame(gsap.to)(frame);
  };
}

interface SlidingDoorsAttributes {
  children?: JSX.Element;
}

// Animating `grid-template-columns' is very shaky if more than one
// column is changed at once.
export function GridSlidingDoors(props: SlidingDoorsAttributes): JSX.Element {
  const [hovering, setHovering] = createSignal(false);

  return (
    <div
      onmouseenter={() => setHovering(true)}
      onmouseleave={() => setHovering(false)}
      style={{
        display: "grid",
        "grid-template-columns": hovering() ? "1fr 0fr 0fr" : "1fr 1fr 1fr",
        transition: "1000ms",
      }}
    >
      {props.children}
    </div>
  );
}

import gsap from "gsap";
import { JSX, onMount } from "solid-js";

export type Frame = Map<gsap.TweenTarget, gsap.TweenVars>;

// A convenience constructure for STATE.
export function createFrame(build: (frame: Frame) => void): Frame {
  const frame = new Map<gsap.TweenTarget, gsap.TweenVars>();
  build(frame);
  return frame;
}

// A convenience function that applies state.
function modulateFrame(
  modulate: (target: gsap.TweenTarget, vars: gsap.TweenVars) => void,
) {
  return (frame: Frame) => {
    for (const [target, vars] of frame) {
      modulate(target, vars);
    }
  };
}

function setFrame(frame: Frame) {
  modulateFrame(gsap.set)(frame);
}

export function toFrame(frame: Frame) {
  console.log("to", frame);
  modulateFrame(gsap.to)(frame);
}

interface SlidingDoorsAttributes {
  children?: JSX.Element;
  start?: Frame;
}

export function SlidingDoors(props: SlidingDoorsAttributes): JSX.Element {
  const { start, children } = props;

  onMount(() => {
    if (start) {
      setFrame(start);
    }
  });

  return <div class="inline-flex overflow-hidden">{children}</div>;
}

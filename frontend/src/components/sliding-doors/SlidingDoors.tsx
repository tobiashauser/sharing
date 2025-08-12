import gsap from "gsap";
import { createSignal, JSX } from "solid-js";

type SceneValue = {
  constructor: (
    targets: gsap.TweenTarget,
    vars: gsap.TweenVars,
  ) => gsap.core.Tween;
  targets: gsap.TweenTarget;
  vars: gsap.TweenVars;
  position?: gsap.Position;
};

export type Scene = SceneValue[];

export class _Scene {}

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

  // A simple wrapper around `gsap.to'.
  to(vars: gsap.TweenVars, position?: gsap.Position): SceneValue {
    return {
      constructor: gsap.to,
      targets: this.selector,
      vars: vars,
      position: position,
    };
  }
}

// A convenience constructure for STATE.
// NOT NEEDED since the change to an array.
export function newScene(build: (scene: Scene) => void): Scene {
  const scene: Scene = new Array();
  build(scene);
  return scene;
}

// A convenience function that applies state.
export function animateScene(scene: Scene) {
  const tl = gsap.timeline();
  for (const { constructor, targets, vars, position } of scene) {
    const tween = constructor(targets, vars);
    tl.add(tween, position);
  }
}

interface SlidingDoorsAttributes {
  children?: JSX.Element;
  scenes?: Scene[];
  fix?: "left" | "center" | "right";
}

// Unfortunately, providing an initial frame with `setFrame' leads to
// to jumps in each last frame. Put the <SlidingDoors> in a flex
// container so that it will only take up as much horizontal space as
// needed.
export function SlidingDoors(props: SlidingDoorsAttributes): JSX.Element {
  const { children, scenes, fix } = props;
  const [currentScene, setCurrentScene] = createSignal<number | undefined>(
    undefined,
  );

  const nextScene = () => {
    console.log("next scene");
    if (!scenes) return;
    if (scenes.length === 0) return;

    const nextScene = ((currentScene() ?? -1) + 1) % scenes.length;
    animateScene(scenes[nextScene]);
    setCurrentScene(nextScene);
  };

  return (
    <div
      onclick={nextScene}
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

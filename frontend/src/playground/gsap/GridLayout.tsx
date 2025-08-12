import gsap from "gsap";
import { JSX, onMount } from "solid-js";

export function GridLayout(): JSX.Element {
  // Get references to all the children.
  let fst!: HTMLDivElement;
  let snd!: HTMLDivElement;
  let thd!: HTMLDivElement;
  let four!: HTMLDivElement;

  const state1 = new Map<gsap.TweenTarget, gsap.TweenVars>();

  onMount(() => {
    // Should be passed in...
    state1.set(fst, { width: "100px" });
    state1.set(snd, { width: "auto" });
    state1.set(thd, { width: 0 });
    state1.set(four, { width: 0 });

    for (const [target, props] of state1) {
      gsap.to(target, { ...props, delay: 1 });
    }
  });

  return (
    <div
      class="m-10 border-black border-2"
      style={{
        display: "inline-grid",
      }}
    >
      <div
        class="bg-blue-400 py-10 px-20"
        style={{
          "grid-column": "1",
          "grid-row": "1",
        }}
      />
      <div
        id="target"
        class="bg-red-400"
        style={{
          "grid-column": "2",
          "grid-row": "1",
          width: 0,
        }}
      >
        <span>Enter Code</span>
      </div>
      <div
        class="bg-green-400 py-10 px-10"
        style={{
          "grid-column": "3",
          "grid-row": "1",
        }}
      />
    </div>
  );
}

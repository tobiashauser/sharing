import { createSignal } from "solid-js";
import {
  createFrame,
  SlidingDoors,
  toFrame,
} from "../components/sliding-doors";

export function SlidingDoorsTest() {
  const frames = Array.from({ length: 9 }, (_, i) => {
    const one = ["#red", "#blue", "#green"][i % 3];
    const two = ["#red", "#blue", "#green"][(i + 1) % 3];
    const three = ["#red", "#blue", "#green"][(i + 2) % 3];
    return createFrame((frame) => {
      frame.set(one, { width: (i + 2) * 10 });
      frame.set(two, { width: (i + 1) * 10 });
      frame.set(three, { width: i * 10 });
    });
  });

  const [idx, setIdx] = createSignal(0);

  return (
    <div class="m-4 border">
      <div class="flex">
        <div class="border-black rounded-xl overflow-hidden border-2">
          <SlidingDoors>
            <div id="red" class="bg-red-400 py-4">
              RED
            </div>
            <div id="blue" class="bg-blue-400 py-4">
              BLUE
            </div>
            <div id="green" class="bg-green-400 py-4">
              GREEN
            </div>
          </SlidingDoors>
        </div>
      </div>

      <button
        onclick={() => {
          toFrame(frames[idx() % frames.length]);
          setIdx((prev) => prev + 1);
        }}
      >
        next
      </button>
    </div>
  );
}

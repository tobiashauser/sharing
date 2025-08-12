// Proof of concept for <SlidingDoors>
import { ID, newScene, Scene, SlidingDoors } from "./SlidingDoors";

export function SlidingDoorsTest() {
  const red = new ID();
  const blue = new ID();
  const hello = new ID();

  const scenes: Scene[] = [
    newScene((s) => {
      s.push(red.to({ width: "180px" }));
      s.push(blue.to({ width: "40px" }));
      s.push(hello.to({ autoAlpha: 1 }));
    }),

    // See special values for the position parameter her:
    // https://gsap.com/docs/v3/GSAP/Timeline
    newScene((s) => {
      s.push(hello.to({ autoAlpha: 0 }, 0));
      s.push(red.to({ width: "40px" }, "1"));
      s.push(blue.to({ width: "180px" }, "<"));
    }),
  ];

  return (
    <div class="m-10">
      <SlidingDoors scenes={scenes} fix="left">
        <div class="border-black rounded-xl flex overflow-hidden border-4">
          <div id={red.id} class="bg-red-400 py-2 flex w-[40px] justify-center">
            <p id={hello.id} class="invisible truncate text-clip opacity-0">
              Hello, world!
            </p>
          </div>
          <div id={blue.id} class="bg-blue-400 w-[180px]" />
          <div class="bg-green-400 w-[40px]" />
        </div>
      </SlidingDoors>
    </div>
  );
}

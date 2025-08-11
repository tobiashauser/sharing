import { FiDownload } from "solid-icons/fi";
import { JSX } from "solid-js";
import { SlidingDoors, createFrame, toFrame } from "./components/sliding-doors";
import "./convenience.css";

export function Code(): JSX.Element {
  let enterCodeButton = createFrame((frame) => {
    frame.set("#input", { width: 0 });
    frame.set("#span", { width: "100px" });
    frame.set("#icon", { width: 0 });
  });

  let inputCode = createFrame((frame) => {
    frame.set("#input", { width: "180px" });
    frame.set("#span", { width: 0 });
    frame.set("#icon", { width: "20px" });
  });

  return (
    <div class="border-slate-600 border">
      <SlidingDoors start={inputCode}>
        <input id="input" type="text" placeholder="Enter Code" />
        <span
          id="span"
          class="text-white bg-slate-600 text-sm cursor-pointer truncate overflow-hidden"
          onclick={() => toFrame(inputCode)}
        >
          Enter Code
        </span>
        <div
          id="icon"
          class="text-white bg-slate-600 flex cursor-pointer items-center"
          onclick={() => toFrame(enterCodeButton)}
        >
          <FiDownload class="size-5" />
        </div>
      </SlidingDoors>
    </div>
  );

  // return (
  //   <>
  //     <div
  //       classList={{
  //         "bg-slate-600 px-2 py-1 transition-all duration-1000": true,
  //         "rounded-sm": state() == State.Button,
  //         "rounded-r-sm translate-x-32": state() == State.Input,
  //         "rounded-l-sm": state() == State.Code,
  //       }}
  //     >
  //       <div class="text-white text-sm">
  //         <Switch>
  //           <Match when={state() == State.Button}>
  //             <span class="text-sm">Enter Code</span>
  //           </Match>
  //           <Match when={state() == State.Input}>
  //             <div class="center-content cursor-pointer">
  //               <FiDownload />
  //             </div>
  //           </Match>
  //           <Match when={state() == State.Code}>
  //             <span>Code</span>
  //           </Match>
  //         </Switch>
  //       </div>
  //     </div>
  //     <div class="border-slate-600 px-2 py-1 rounded-l-sm border">
  //       <input
  //         required
  //         type="text"
  //         placeholder="Enter Code"
  //         class="text-sm w-32 text-slate-600 caret-slate-600 focus:outline-none"
  //       ></input>
  //     </div>

  //     <div class="gap-2 flex">
  //       <button onclick={() => setState(State.Code)}>Code</button>
  //       <button onclick={() => setState(State.Button)}>Button</button>
  //       <button onclick={() => setState(State.Input)}>Input</button>
  //     </div>
  //   </>
  // );

  // return (
  //   <div class="flex transition-all hover:flex-row-reverse">
  //     <div class="border-slate-600 px-2 py-1 rounded-l-sm border">
  //       <input
  //         required
  //         type="text"
  //         placeholder="Enter Code"
  //         class="text-sm w-32 text-slate-600 caret-slate-600 focus:outline-none"
  //       ></input>
  //     </div>
  //     <div class="bg-slate-600 px-2 py-1 rounded-r-sm center-content cursor-pointer">
  //       <FiDownload class="text-white" />
  //     </div>
  //   </div>
  // );

  // return (
  //   <div class="flex">
  //     <div class="bg-slate-600 px-2 py-1 rounded-l-sm">
  //       <span class="text-white text-sm">Code</span>
  //     </div>
  //     <div class="border-slate-600 px-2 py-1 rounded-r-sm border">
  //       <span class="text-slate-600 text-sm">seasonend-bonito</span>
  //     </div>
  //   </div>
  // );

  // return (
  //   <div class="bg-slate-600 px-2 py-1 rounded-sm">
  //     <span class="text-white text-sm">Enter Code</span>
  //   </div>
  // );

  // return <span>CODE</span>;
}

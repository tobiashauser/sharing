import { FiDownload } from "solid-icons/fi";
import { JSX } from "solid-js";
import {
  ID,
  SlidingDoors,
  createFrame,
  toFrame,
} from "./components/sliding-doors";
import "./convenience.css";

export function Code(): JSX.Element {
  const input = new ID();
  const button = new ID();
  const icon = new ID();
  const label = new ID();
  const code = new ID();

  let enterCode = createFrame((frame) => {
    frame.set(input, { width: 0 });
    frame.set(button, { width: "auto" });
    frame.set(icon, { width: 0 });
    frame.set(label, { width: 0 });
    frame.set(code, { width: 0 });
  });

  let inputCode = createFrame((frame) => {
    frame.set(input, { width: "auto" });
    frame.set(button, { width: 0 });
    frame.set(icon, { width: "auto" });
    frame.set(label, { width: 0 });
    frame.set(code, { width: 0 });
  });

  let showCode = createFrame((frame) => {
    frame.set(input, { width: 0 });
    frame.set(button, { width: 0 });
    frame.set(icon, { width: 0 });
    frame.set(label, { width: "auto" });
    frame.set(code, { width: "auto" });
  });

  return (
    <>
      <button onclick={toFrame(showCode)}>show code</button>
      <button onclick={toFrame(enterCode)}>enter code</button>
      <div class="border-slate-600 rounded-sm shadow overflow-hidden border">
        <SlidingDoors>
          {/* The input element to enter a download code. */}
          <input
            id={input.id}
            type="text"
            placeholder="Enter Code"
            style={{ width: 0 }}
          />

          {/* That is the button that should be shown initially. */}
          <span
            id={button.id}
            class="text-white bg-slate-600 text-sm py-1 cursor-pointer truncate overflow-hidden"
            onclick={toFrame(inputCode)}
          >
            Enter Code
          </span>

          {/* Here we have the download icon. It is shown when entering a code. */}
          <div
            id={icon.id}
            class="text-white bg-slate-600 flex cursor-pointer items-center"
            onclick={toFrame(enterCode)}
            style={{ width: 0 }}
          >
            <FiDownload class="size-5" />
          </div>

          {/* We must also show the code if files should be sent. */}
          <span
            id={label.id}
            class="text-white bg-slate-600 text-sm truncate overflow-hidden"
            style={{ width: 0 }}
          >
            Code
          </span>

          <span
            id={code.id}
            class="text-sm truncate overflow-hidden"
            style={{ width: 0 }}
          >
            modern-woodlouse
          </span>
        </SlidingDoors>
      </div>
    </>
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

import { FiDownload } from "solid-icons/fi";
import { JSX, onMount } from "solid-js";
import {
  animateScene,
  ID,
  Scene,
  SlidingDoors,
} from "./components/sliding-doors";
import "./convenience.css";

export function Code(): JSX.Element {
  // We need a whole bunch of IDs to reference everthing.
  const input = new ID();
  const inputContent = new ID();

  const button = new ID();
  const buttonContent = new ID();

  const icon = new ID();
  const iconContent = new ID();

  const codeLabel = new ID();
  const codeLabelContent = new ID();

  const code = new ID();
  const codeContent = new ID();

  const showInput: Scene = [
    // Remove all content.
    buttonContent.to({ autoAlpha: 0 }, "remove"),
    // Change all sizes.
    input.to({ width: "auto" }, "resize"),
    button.to({ width: 0 }, "resize"),
    icon.to({ width: "auto" }, "resize"),
    // Show the contents.
    inputContent.to({ autoAlpha: 1 }, "insert"),
    iconContent.to({ autoAlpha: 1 }, "insert"),
  ];

  const showCode: Scene = [
    // Remove all content.
    buttonContent.to({ autoAlpha: 0 }, "remove"),
    // Change all sizes.
    button.to({ width: 0 }, "resize"),
    codeLabel.to({ width: "auto" }, "resize"),
    code.to({ width: "auto" }, "resize"),
    // Show the content again.
    codeLabelContent.to({ autoAlpha: 1 }, "insert"),
    codeContent.to({ autoAlpha: 1 }, "insert"),
  ];

  onMount(() => {
    animateScene(showInput);
    animateScene(showCode);
  });

  return (
    <SlidingDoors>
      <div class="border-red-400 inline-flex items-center border-2">
        {/* The input element to enter a download code. */}
        <div id={input.id} class="w-0">
          <input
            id={inputContent.id}
            type="text"
            placeholder="Enter Code"
            class="text=sm placeholder:text-sm caret-slate-600"
          />
        </div>

        {/* That is the button that should be shown initially. */}
        <div
          id={button.id}
          class="text-white py-1 bg-slate-600 text-sm truncate text-clip"
        >
          <span id={buttonContent.id} class="px-2">
            Enter Code
          </span>
        </div>

        {/* Here we have the download icon. It is shown when entering a code. */}
        <div
          id={icon.id}
          class="text-white py-1 bg-slate-600 w-0 truncate text-clip"
        >
          <div id={iconContent.id} class="px-2 invisible opacity-0">
            <FiDownload class="size-5" />
          </div>
        </div>

        {/* We must also show the code if files should be sent. */}
        <div
          id={codeLabel.id}
          class="text-white py-1 bg-slate-600 text-sm w-0 truncate text-clip"
        >
          <span id={codeLabelContent.id} class="px-2 invisible opacity-0">
            Code
          </span>
        </div>

        <div id={code.id} class="text-sm w-0 truncate text-clip">
          <span id={codeContent.id} class="px-2 invisible opacity-0">
            modern-woodlouse
          </span>
        </div>
      </div>
    </SlidingDoors>
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

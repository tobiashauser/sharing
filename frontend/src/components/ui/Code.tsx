import { FiDownload } from "solid-icons/fi";
import { createEffect, JSX, Signal } from "solid-js";
import { ID, Scene, SlidingDoors } from "~/components/sliding-doors";
import "~/lib/convenience.css";

export enum State {
  dropFiles,
  showShareCode,
  enterShareCode,
}

interface CodeAttributes {
  state: Signal<State>;
  shareCode: Signal<string | undefined>;
}

export function Code(props: CodeAttributes): JSX.Element {
  const [state, setState] = props.state;
  const [shareCode, setShareCode] = props.shareCode;

  // A reference to the input element.
  let inputElement!: HTMLInputElement;

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

  const expand = { width: "auto" };
  const contract = { width: 0 };
  const hide = { autoAlpha: 0 };
  const show = { autoAlpha: 1 };

  const showButton = new Scene()
    .stage("remove", [
      inputContent.to(hide),
      iconContent.to(hide),
      codeContent.to(hide),
      codeLabelContent.to(hide),
    ])
    .stage("resize", [
      button.to(expand),
      input.to(contract),
      icon.to(contract),
      code.to(contract),
    ])
    .stage("show", [buttonContent.to(show)]);

  const showCode = new Scene()
    .stage("remove", [buttonContent.to(hide), inputContent.to(hide)])
    .stage("resize", [
      button.to(contract),
      codeLabel.to(contract),
      code.to(contract),
    ])
    .stage("insert", [codeLabelContent.to(show), codeContent.to(show)]);

  const showInput = new Scene()
    .stage("remove", [buttonContent.to(hide)])
    .stage("resize", [input.to(expand), button.to(contract), icon.to(expand)])
    .stage("insert", [inputContent.to(show), iconContent.to(show)])
    .onComplete(() => {
      inputElement.focus();
    });

  // Add some event listeners to the input element to submit and
  // cancel the input.
  setTimeout(() => {
    inputElement.addEventListener("keydown", (e: KeyboardEvent) => {
      if (e.code === "Enter" && e.target instanceof HTMLInputElement) {
        setShareCode(e.target.value);
        console.log("Submit code:", shareCode());
        setState(State.dropFiles);
      } else if (e.code === "Escape") {
        setState(State.dropFiles);
      }
    });
  });

  // Update whenever STATE changes. DO NOT USE ANIMATE DIRECTLY.
  createEffect(() => {
    if (state() === State.dropFiles) {
      showButton.animate();
    } else if (state() === State.showShareCode) {
      showCode.animate();
    } else if (state() === State.enterShareCode) {
      showInput.animate();
    } else {
      console.warn("Unhandled state", state());
    }
  });

  return (
    <SlidingDoors>
      <div class="border-slate-700 rounded-sm inline-flex items-center border-2 shadow-[0px_0px_5px_2px_rgba(0,0,0,0.1)]">
        {/* The input element to enter a download code. */}
        <div id={input.id} class="w-0">
          <input
            id={inputContent.id}
            ref={inputElement}
            type="text"
            placeholder="Enter Code"
            class="text-sm text-slate-700 placeholder:text-sm caret-slate-700 px-2 invisible opacity-0 focus:outline-none"
            onblur={() => setState(State.dropFiles)}
            onchange={(e) => setShareCode(e.target.value)}
          />
        </div>

        {/* That is the button that should be shown initially. */}
        <div
          id={button.id}
          class="text-white py-1 bg-slate-700 text-sm hover:bg-slate-800 cursor-pointer truncate text-clip"
          onclick={() => {
            setState(State.enterShareCode);
          }}
        >
          <span id={buttonContent.id} class="px-2">
            Enter Code
          </span>
        </div>

        {/* Here we have the download icon. It is shown when entering a code. */}
        <div
          id={icon.id}
          class="text-white py-1 bg-slate-700 w-0 truncate text-clip"
        >
          <div id={iconContent.id} class="px-2 invisible opacity-0">
            <FiDownload class="size-5" />
          </div>
        </div>

        {/* We must also show the code if files should be sent. */}
        <div
          id={codeLabel.id}
          class="text-white py-1 bg-slate-700 text-sm w-0 truncate text-clip"
        >
          <span id={codeLabelContent.id} class="px-2 invisible opacity-0">
            Code
          </span>
        </div>

        <div id={code.id} class="text-sm w-0 truncate text-clip">
          <span id={codeContent.id} class="px-2 invisible opacity-0">
            {shareCode() ?? "No code found here..."}
          </span>
        </div>
      </div>
    </SlidingDoors>
  );
}

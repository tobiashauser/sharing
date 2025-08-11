import gsap from "gsap";
import { createSignal, JSX, Setter } from "solid-js";
import "../../convenience.css";

enum State {
  Button,
  Code,
  Input,
}

function labelStyle(state: State) {
  const defaults = { height: "28px" };
  if (state === State.Button) {
    return {
      ...defaults,
      width: "88px",
    };
  } else if (state === State.Code) {
    return {
      ...defaults,
      width: "60px",
    };
  } else if (state === State.Input) {
    return {
      ...defaults,
      width: "26px",
    };
  } else {
    return {};
  }
}

function inputStyle(state: State) {
  const defaults = { height: "28px" };
  if (state === State.Button) {
    return {
      ...defaults,
      width: "0px",
      autoAlpha: 0,
    };
  } else if (state === State.Code) {
    return {
      ...defaults,
      width: "0px",
      autoAlpha: 0,
    };
  } else if (state === State.Input) {
    return {
      ...defaults,
      width: "120px",
      autoAlpha: 1,
    };
  } else {
    return {};
  }
}

function codeStyle(state: State) {
  const defaults = { height: "28px" };
  if (state === State.Button) {
    return {
      ...defaults,
      width: "0px",
    };
  } else if (state === State.Code) {
    return {
      ...defaults,
      width: "140px",
    };
  } else if (state === State.Input) {
    return {
      ...defaults,
      width: "0px",
    };
  } else {
    return {};
  }
}

function animate(to: State, setState: Setter<State>) {
  const defaults = { ease: "power4.out", duration: 0.3 };

  // Build up a timeline.
  var tl = gsap.timeline();

  // Animate the containers.
  tl.to("#input", { ...defaults, ...inputStyle(to) }, 0.1);
  tl.to("#label", { ...defaults, ...labelStyle(to) }, 0.1);
  tl.to("#code", { ...defaults, ...codeStyle(to) }, 0.1);
}

function animateToButton(setState: Setter<State>): EventListener {
  return () => {
    animate(State.Button, setState);
  };
}

function animateToCode(setState: Setter<State>): EventListener {
  return () => {
    animate(State.Code, setState);
  };
}

function animateToInput(setState: Setter<State>): EventListener {
  return () => {
    animate(State.Input, setState);
  };
}

export function Main(): JSX.Element {
  const [getState, setState] = createSignal(State.Button);

  const inputStyling = inputStyle(getState());
  const labelStyling = labelStyle(getState());
  const codeStyling = codeStyle(getState());

  return (
    <div class="gap-2 flex flex-col">
      <div class="m-10 justify-left text-sm flex items-center">
        <div
          id="input"
          class="border-blue-400 border"
          style={inputStyling}
        ></div>

        <div
          id="label"
          class="bg-green-400 rounded-sm"
          style={labelStyling}
        ></div>

        <div id="code" class="border-red-400 border" style={codeStyling}></div>
      </div>

      <button onclick={animateToButton(setState)}>Button</button>
      <button onclick={animateToCode(setState)}>Code</button>
      <button onclick={animateToInput(setState)}>Input</button>
    </div>
  );
}

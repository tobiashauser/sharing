import { type Component } from "solid-js";

import "./App.css";
import { createWindowDropzone } from "./components/window-drop-zone";

// The main entrypoint into the app. This component is reponsible to
// manage state and build the layout of the components. Don't push any
// top-level layout logic into other components.
const App: Component = () => {
  const { errors, files, getInputProps, isDragging, setRefs } =
    createWindowDropzone();

  // Finish the setup for the window drop zone.
  let inputRef!: HTMLInputElement;
  setTimeout(() => {
    setRefs(inputRef);
  });

  return (
    <>
      {/* This is needed as a target for the drop zone. */}
      <input {...getInputProps()} class="sr-only" ref={inputRef} />
      {/* This div covers the entire viewport. It has the standard padding-2 on all sides. */}
      <div class="flex justify-between">
        <span class="bg-red-200">ENTER CODE</span>
        <span class="bg-red-200">HELP</span>
      </div>
      {/* The top margin depends on the height and width of the viewport. */}
      <div class="bg-yellow-200 mt-[min(max(100vw,40rem)-40rem,max(100vh,40rem)-40rem,20vh)]">
        <div class="flex justify-center">
          {/* Add enough margin for the shadow. */}
          <span class="m-6 bg-red-200">ADD FILES</span>
        </div>
        {/* Set the width of each cell with `auto-cols'. */}
        <div class="md:snap-x md:grid-flow-col md:grid-rows-5 gap-2 grid snap-mandatory auto-cols-[minmax(300px,400px)] justify-center-safe overflow-scroll [scrollbar-width:none] [&::-webkit-scrollbar]:hidden">
          <span class="bg-green-200 snap-start">FILE_1</span>
          <span class="bg-green-200 snap-start">FILE_2</span>
          <span class="bg-green-200 snap-start">FILE_3</span>
          <span class="bg-green-200 snap-start">FILE_4</span>
          <span class="bg-green-200 snap-start">FILE_5</span>
          <span class="bg-green-200 snap-start">FILE_6</span>
          <span class="bg-green-200 snap-start">FILE_7</span>
          <span class="bg-green-200 snap-start">FILE_8</span>
          <span class="bg-green-200 snap-start">FILE_1</span>
          <span class="bg-green-200 snap-start">FILE_2</span>
          <span class="bg-green-200 snap-start">FILE_3</span>
          <span class="bg-green-200 snap-start">FILE_4</span>
          <span class="bg-green-200 snap-start">FILE_5</span>
          <span class="bg-green-200 snap-start">FILE_6</span>
          <span class="bg-green-200 snap-start">FILE_7</span>
          <span class="bg-green-200 snap-start">FILE_8</span>
          <span class="bg-green-200 snap-start">FILE_1</span>
          <span class="bg-green-200 snap-start">FILE_2</span>
          <span class="bg-green-200 snap-start">FILE_3</span>
          <span class="bg-green-200 snap-start">FILE_4</span>
          <span class="bg-green-200 snap-start">FILE_5</span>
          <span class="bg-green-200 snap-start">FILE_6</span>
          <span class="bg-green-200 snap-start">FILE_7</span>
          <span class="bg-green-200 snap-start">FILE_8</span>
        </div>
      </div>
    </>
  );
};

export default App;

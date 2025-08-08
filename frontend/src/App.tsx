import { type Component } from "solid-js";

import { FiUpload } from "solid-icons/fi";

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
      <div class="mt-[min(max(100vw,40rem)-40rem,max(100vh,40rem)-40rem,20vh)]">
        <div class="flex justify-center">
          {/* Add enough margin for the shadow. */}
          <div class="rounded-xl border shadow-[0px_0px_15px_3px_rgba(0,0,0,0.1)]">
            <div
              data-dragging={isDragging()}
              class="data-[dragging=true]:bg-blue-100/25 data-[dragging=true]:border-blue-200 hover:bg-accent/50 flex cursor-pointer flex-col items-center justify-center rounded-[9px] hover:border hover:border-dashed data-[dragging=true]:border-2 data-[dragging=true]:border-dashed"
            >
              <FiUpload size={24} />
              <p class="mb-1.5 text-sm font-medium">Upload files</p>
              <p class="text-muted-foreground mb-2 text-xs">
                Drag & Drop or click to browse
              </p>
            </div>
          </div>
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
        </div>
      </div>
    </>
  );
};

export default App;

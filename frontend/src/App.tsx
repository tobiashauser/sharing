import type { Component } from "solid-js";

import "./App.css";

// The main entrypoint into the app. This component is reponsible to
// manage state and build the layout of the components. Don't push any
// top-level layout logic into other components.
const App: Component = () => {
  return (
    // This div covers the entire viewport. It has the standard padding-2 on all sides.
    <div class="bg-blue-200 p-2 h-screen">
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
        <div class="md:grid-flow-col md:grid-rows-5 gap-2 grid auto-cols-[minmax(100px,200px)] justify-center">
          <span class="bg-green-200">FILE_1</span>
          <span class="bg-green-200">FILE_2</span>
          <span class="bg-green-200">FILE_3</span>
          <span class="bg-green-200">FILE_4</span>
          <span class="bg-green-200">FILE_5</span>
          <span class="bg-green-200">FILE_6</span>
          <span class="bg-green-200">FILE_7</span>
          <span class="bg-green-200">FILE_8</span>
        </div>
      </div>
    </div>
  );
};

export default App;

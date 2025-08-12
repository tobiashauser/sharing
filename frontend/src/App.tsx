import { createSignal, For, type Component } from "solid-js";
import "./App.css";
import { Code, State } from "./Code";
import { dropzone } from "./components/drop-zone";
import { Droparea } from "./Droparea";
import { Info } from "./Info";
import { Itemcard } from "./Itemcard";

// The main entrypoint into the app. This component is reponsible to
// manage state and build the layout of the components. Don't push any
// top-level layout logic into other components.
const App: Component = () => {
  const {
    bindInputElement,
    configureInputElement,
    getDragging,
    getItems,
    removeItem,
    openFileDialog,
  } = dropzone(window, true);

  const appState = createSignal(State.dropFiles);
  const shareCode = createSignal<string | undefined>(undefined);

  const [currentAppState, setAppState] = appState;
  const [getShareCode, setShareCode] = shareCode;

  // Finish the setup for the window drop zone.
  let inputRef!: HTMLInputElement;
  setTimeout(() => {
    bindInputElement(inputRef);
  });

  return (
    <div class="m-2">
      {/* This is needed as a target for the drop zone. */}
      <input
        {...configureInputElement({ refKey: "ref" })}
        class="sr-only"
        ref={inputRef}
      />
      {/* This div covers the entire viewport. It has the standard padding-2 on all sides. */}
      <div class="flex justify-between">
        <Code state={appState} shareCode={shareCode} />
        <Info />
        {/* <span class="bg-red-200">HELP</span> */}
      </div>
      {/* The top margin depends on the height and width of the viewport. */}
      <div class="mt-[min(max(100vw,40rem)-40rem,max(100vh,40rem)-40rem,10vh)]">
        <div class="flex justify-center">
          {/* Add enough margin for the shadow and explicitely set the
          height and width, otherwise the blue border with an
          increased width will grow the entire element.  */}
          <div class="m-6 h-40 max-w-md w-2/3">
            {/* The height and width are specified in the parent. The
      component itself always takes as much space as possible. */}
            <Droparea
              dragging={getDragging}
              getItems={getItems}
              openFileDialog={openFileDialog}
            />
          </div>
        </div>
        {/* Set the width of each cell with `auto-cols'. */}
        <div class="md:snap-x md:grid-flow-col md:grid-rows-5 gap-2 grid snap-mandatory auto-cols-[minmax(300px,400px)] justify-center-safe overflow-scroll [scrollbar-width:none] [&::-webkit-scrollbar]:hidden">
          <For each={getItems()}>
            {(item) => <Itemcard item={item} remove={removeItem(item)} />}
          </For>
        </div>
      </div>
    </div>
  );
};

export default App;

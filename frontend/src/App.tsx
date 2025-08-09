import {
  createEffect,
  JSX,
  ParentComponent,
  ParentProps,
  type Component,
} from "solid-js";

import { FiUpload } from "solid-icons/fi";

import "./App.css";
import { dropzone } from "./components/drop-zone/dropzone";

import { cn } from "./prelude";

const WithBorder: ParentComponent = (props) => {
  return <div class="flex">{props.children}</div>;
};

const Centered: Component<
  { fill?: boolean } & ParentProps & JSX.HTMLAttributes<HTMLDivElement>
> = (props) => {
  return (
    <div
      {...props}
      class={cn(
        "flex flex-col items-center justify-center",
        props.class,
        props.fill ? "h-1/1 w-1/1" : "",
      )}
    >
      {props.children}
    </div>
  );
};

const DropArea: Component<
  { isDragging: boolean } & JSX.HTMLAttributes<HTMLDivElement>
> = (props) => {
  return (
    <Centered
      {...props}
      class={cn(
        "p-4 rounded-xl border shadow-[0px_0px_15px_3px_rgba(0,0,0,0.1)]",
        props.class,
      )}
      data-dragging={props.isDragging}
    >
      <Centered
        class={cn(
          "cursor-pointer rounded-[9px] border-dashed",
          "hover:bg-accent/50 hover:border",
          "data-[dragging=true]:border-blue-200 data-[dragging=true]:bg-blue-100/25 data-[dragging=true]:border-2",
          "ease-in-out transition",
        )}
        data-dragging={props.isDragging}
        fill={true}
      >
        <Centered
          class={cn(
            "mb-3 size-11 ease-in-out text-muted-foreground rounded-full border transition",
            "data-[dragging=true]:border-blue-300 data-[dragging=true]:bg-blue-200/40 data-[dragging=true]:text-blue-600 data-[dragging=true]:border-2",
          )}
          data-dragging={props.isDragging}
        >
          <FiUpload class="size-5 opacity-60" />
        </Centered>
        <p class="text-sm mb-1.5 font-medium">Upload files</p>
        <p class="text-muted-foreground text-xs">
          Drag & Drop or click to browse
        </p>
      </Centered>
    </Centered>
  );
};

// The main entrypoint into the app. This component is reponsible to
// manage state and build the layout of the components. Don't push any
// top-level layout logic into other components.
const App: Component = () => {
  // const { errors, files, getInputProps, isDragging, setRefs, openFileDialog } =
  //   createWindowDropzone();
  const { bindInputElement, configureInputElement, getDragging } =
    dropzone(window);

  // Finish the setup for the window drop zone.
  let inputRef!: HTMLInputElement;
  // setTimeout(() => {
  //   setRefs(inputRef);
  // });
  setTimeout(() => {
    bindInputElement(inputRef);
  });

  createEffect(() => {
    console.log("isDragging", getDragging());
  });

  return (
    <>
      {/* This is needed as a target for the drop zone. */}
      {/* <input {...getInputProps()} class="sr-only" ref={inputRef} /> */}
      <input
        {...configureInputElement({ refKey: "ref" })}
        class="sr-only"
        ref={inputRef}
      />
      {/* This div covers the entire viewport. It has the standard padding-2 on all sides. */}
      <div class="flex justify-between">
        <span class="bg-red-200">ENTER CODE</span>
        <span class="bg-red-200">HELP</span>
      </div>
      {/* The top margin depends on the height and width of the viewport. */}
      <div class="mt-[min(max(100vw,40rem)-40rem,max(100vh,40rem)-40rem,20vh)]">
        <div class="flex justify-center">
          {/* Add enough margin for the shadow and explicitely set the
          height and width, otherwise the blue border with an
          increased width will grow the entire element.  */}
          <DropArea
            class="m-6 h-40 max-w-md w-2/3"
            isDragging={getDragging()}
            // onclick={openFileDialog}
          />
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

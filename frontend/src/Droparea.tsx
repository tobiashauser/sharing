import { FiUpload } from "solid-icons/fi";
import { Accessor, createEffect, createSignal, JSX } from "solid-js";
import { ZStack } from "./components/zstack";
import "./convenience.css";

interface DropareaAttributes {
  dragging: Accessor<boolean>;
}

export function Droparea(props: DropareaAttributes): JSX.Element {
  const { dragging } = props;
  const [hovering, setHovering] = createSignal(false);

  createEffect(() => {
    // console.log(hovering());
  });

  return (
    <div class="rounded-xl p-4 h-full w-full shadow-[0px_0px_15px_3px_rgba(0,0,0,0.1)]">
      <ZStack>
        {/* A dashed border that appears when hovering. Dragging
        doesn't count as hovering. */}
        <div
          classList={{
            "border border-dashed rounded-[9px]": true,
            "transition ease-in-out": true,
            "border-transparent": !hovering(),
            "bg-accent/50": hovering(),
          }}
        />
        {/* And now the border when dragging. */}
        <div
          classList={{
            "border-2 border-dashed rounded-[9px]": true,
            "transition ease-in-out": true,
            "border-transparent": !dragging(),
            "border-blue-200 bg-blue-100/25": dragging(),
          }}
        />
        {/* This is the top layer, hence it must handle events. */}
        <div
          class="center-content"
          onmouseenter={() => setHovering(true)}
          onmouseleave={() => setHovering(false)}
        >
          <div class="mb-3 size-11 center-content rounded-full border">
            <FiUpload class="size-5 opacity-60" />
          </div>
          <p class="text-sm mb-1.5 font-medium">Upload files</p>
          <p class="text-muted-foreground text-xs">
            Drag & Drop or click to browse
          </p>
        </div>
      </ZStack>
    </div>
  );
}

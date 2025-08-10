import { Accessor, createSignal, JSX } from "solid-js";

interface DropareaAttributes {
  dragging: Accessor<boolean>;
}

export function Droparea(props: DropareaAttributes): JSX.Element {
  const { dragging } = props;
  const [hovering, setHovering] = createSignal(false);

  return (
    <div class="rounded-xl p-4 h-full w-full border shadow-[0px_0px_15px_3px_rgba(0,0,0,0.1)] [&>*]:col-start-1 [&>*]:row-start-1">
      <div
        class="grid h-full w-full cursor-pointer"
        onmouseenter={() => setHovering(true)}
        onmouseleave={() => setHovering(false)}
      >
        <div
          classList={{
            "rounded-[9px]": true,
            "transition duration-1000 ease-in-out": true,
            "border border-dashed": true,
            "border-transparent": !hovering(),
            "bg-accent/50": hovering(),
          }}
        />
        <div
          classList={{
            "rounded-[9px]": true,
            "transition duration-1000 ease-in-out": true,
            "border-2 border-dashed": true,
            "border-transparent": !dragging(),
            "border-blue-200 bg-blue-100/25": dragging(),
          }}
        />
        <p class="col-start-1 row-start-1">DROPAREA</p>
      </div>
    </div>
  );
}

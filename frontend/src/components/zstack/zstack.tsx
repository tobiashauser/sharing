import { children, createEffect, JSX } from "solid-js";

interface ZStackAttributes {
  children?: JSX.Element;
}

// A simple component that stacks its children.
export function ZStack(props: ZStackAttributes): JSX.Element {
  const resolved = children(() => props.children);
  createEffect(() => {
    for (const child of resolved.toArray()) {
      (child as HTMLElement).classList.add("row-start-1", "col-start-1");
    }
  });

  return <div class="grid h-full w-full">{resolved()}</div>;
}

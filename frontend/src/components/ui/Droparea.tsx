import { BsSendFill } from "solid-icons/bs";
import { FiShare } from "solid-icons/fi";
import { Accessor, createSignal, JSX, Match, Switch } from "solid-js";
import { Item } from "~/components/drop-zone";
import { Magnetic } from "~/components/magnetic";
import { ZStack } from "~/components/zstack";
import "~/lib/convenience.css";

interface DropareaAttributes {
  dragging: Accessor<boolean>;
  getItems: Accessor<Item[]>;
  openFileDialog: () => void;
}

export default function DropArea(props: DropareaAttributes): JSX.Element {
  const { dragging, getItems, openFileDialog } = props;
  const [hovering, setHovering] = createSignal(false);

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
          class="center-content cursor-pointer"
          onmouseenter={() => setHovering(true)}
          onmouseleave={() => setHovering(false)}
          onclick={openFileDialog}
        >
          <Switch>
            {/* We show a button to trigger sending the files, when
              any have already been uploaded and none is being dragged
              in. */}
            <Match when={getItems().length > 0 && !dragging()}>
              <Magnetic>
                <div
                  onclick={(e) => {
                    e.preventDefault();
                    e.stopPropagation();
                    console.log("send files");
                  }}
                  class="mb-3 size-11 text-emerald-400 border-emerald-400 bg-emerald-200/40 center-content rounded-full border border-2 shadow-[0px_0px_5px_1px_rgba(0,0,0,0.1)]"
                >
                  <BsSendFill class="size-5" />
                </div>
              </Magnetic>
            </Match>
            {/* In any other case we show our upload symbol. */}
            <Match when={true}>
              <div
                classList={{
                  "mb-3 size-11 center-content": true,
                  "transition ease-in-out": true,
                  "border rounded-full": true,
                  "border-2 border-blue-300 text-blue-600 bg-blue-200/40":
                    dragging(),
                }}
              >
                <FiShare class="size-5 opacity-60" />
              </div>
            </Match>
          </Switch>
          <p class="text-sm mb-1.5 font-medium">Upload files</p>
          <p class="text-muted-foreground text-xs">
            Drag & Drop or click to browse
          </p>
        </div>
      </ZStack>
    </div>
  );
}

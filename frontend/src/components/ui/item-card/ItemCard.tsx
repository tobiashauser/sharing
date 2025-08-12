import gsap from "gsap";
import { FaSolidXmark } from "solid-icons/fa";
import { createEffect, createSignal, onMount } from "solid-js";
import { Item } from "~/components/drop-zone";
import { ID } from "~/components/sliding-doors";
import Icon from "./Icon";
import { info } from "./utils";

/// Itemcard

// This component displays an item to be shared. It initiates the
// upload and provides visual feedback.

interface ItemCardAttributes {
  item: Item;
  remove: EventListener;
}

export function ItemCard(props: ItemCardAttributes) {
  const { item, remove } = props;
  const [hovering, setHovering] = createSignal(false);
  const [uploaded, setUploaded] = createSignal(false);
  const [progress, setProgress] = createSignal(0);

  // (Ab)use `onMount' to initiate the upload. This hook is only called
  // once when the component is first rendered.
  onMount(() => {
    // uploadItem(item, "/upload");
    Array.from({ length: 100 }).forEach((_, idx) => {
      setTimeout(() => setProgress((prev) => prev + 1), idx * 40);
    });
  });

  // Manage the progress indications.
  // let bg!: HTMLDivElement;
  // createEffect(() => {
  //   if (bg) {
  //     bg.style.width = progress() + "%";
  //   }
  // });
  let progressBar = new ID();
  createEffect(() => {
    gsap.to(progressBar.selector, { width: `${progress()}%` });
  });

  return (
    <div
      onmouseenter={() => setHovering(true)}
      onmouseleave={() => setHovering(false)}
      class="rounded-lg p-2 relative break-inside-avoid overflow-hidden border"
    >
      {/* Finally, the content of the card. */}
      <div class="gap-3 flex items-center justify-between">
        <div class="gap-3 flex items-center overflow-hidden">
          <div class="size-10 rounded flex aspect-square items-center justify-center border">
            <Icon item={item} uploaded={progress() === 100} />
          </div>

          <div class="min-w-0 gap-0.5 flex flex-col">
            <p
              classList={{
                "font-medium truncate text-[13px]": true,
                "text-slate-800": progress() === 100,
                "text-muted-foreground": progress() < 100,
              }}
            >
              {item.name}
            </p>
            <p class="text-muted-foreground text-xs">{info(item)}</p>
          </div>
        </div>

        <button
          onclick={remove}
          classList={{
            "text-muted-foreground/80 rounded-full": true,
            "text-red-300 bg-red-100": hovering(),
            "hover:bg-red-200 hover:text-red-400": true,
            "transition ease-in-out": true,
          }}
        >
          <FaSolidXmark class="size-3 m-1" />
        </button>
      </div>

      {/* This div is going to be the progress bar. Its width will be animated. */}
      <div
        // ref={bg}
        id={progressBar.id}
        class="left-0 top-0 bottom-0 w-0 bg-blue-100/25 absolute -z-1"
      />
    </div>
  );
}

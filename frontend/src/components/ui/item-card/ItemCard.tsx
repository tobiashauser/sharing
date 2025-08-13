import gsap from "gsap";
import { FaSolidXmark } from "solid-icons/fa";
import { createEffect, createSignal, onMount } from "solid-js";
import { Item, UploadRequest, UploadStatus } from "~/components/drop-zone";
import { ID } from "~/components/sliding-doors";
import Icon from "./Icon";
import { info } from "./utils";

/// Itemcard

// This component displays an item to be shared. It initiates the
// upload and provides visual feedback.

interface ItemCardAttributes {
  item: Item;
  remove: EventListener;
  session: string;
}

export function ItemCard(props: ItemCardAttributes) {
  const { item, remove, session } = props;
  const [hovering, setHovering] = createSignal(false);
  const [uploaded, setUploaded] = createSignal(false);
  const [progress, setProgress] = createSignal(0);

  const request = new UploadRequest(item, "http://localhost:3000", session);
  const status = request.status;

  // (Ab)use `onMount' to initiate the upload. This hook is only called
  // once when the component is first rendered.
  onMount(() => {
    request.send();
  });

  createEffect(() => {
    console.log("request", status());
  });

  // Increase the the background fill in relation to `progress'.
  let progressBar = new ID();
  createEffect(() => {
    gsap.to(progressBar.selector, { width: `${progress()}%` });
  });
  // And remove it once it is uploaded.
  createEffect(() => {
    if (uploaded()) {
      gsap.to(progressBar.selector, {
        autoAlpha: 0,
        ease: "power2.in",
        delay: 0.3,
      });
    }
  });

  return (
    <div
      onmouseenter={() => setHovering(true)}
      onmouseleave={() => setHovering(false)}
      classList={{
        "p-2 relative break-inside-avoid overflow-hidden": true,
        "rounded-lg border": true,
        "border-red-200 bg-red-100/25": status() == UploadStatus.failed,
      }}
    >
      {/* Finally, the content of the card. */}
      <div class="gap-3 flex items-center justify-between">
        <div class="gap-3 flex items-center overflow-hidden">
          <div
            classList={{
              "size-10 rounded flex aspect-square items-center justify-center border": true,
              "border-red-200": status() === UploadStatus.failed,
            }}
          >
            <Icon item={item} status={status} />
          </div>

          <div class="min-w-0 gap-0.5 flex flex-col">
            <p
              classList={{
                "font-medium truncate text-[13px]": true,
                "text-slate-800": true,
                "text-neutral-300": status() === UploadStatus.ongoing,
                "transition ease-in": true,
              }}
            >
              {item.name}
            </p>
            <p
              classList={{
                "text-xs": true,
                "text-muted-foreground": true,
                "text-neutral-200": status() === UploadStatus.ongoing,
                "transition ease-in": true,
              }}
            >
              {info(item)}
            </p>
          </div>
        </div>

        <button
          onclick={() => console.log("remove item")}
          classList={{
            "text-muted-foreground/80 rounded-full": true,
            "text-red-300 bg-red-100": hovering(),
            "hover:bg-red-200 hover:text-red-400": true,
            "transition ease-in": true,
          }}
        >
          <FaSolidXmark class="size-3 m-1" />
        </button>
      </div>

      {/* This div is going to indicate the progress by slowly filling the background. */}
      <div
        id={progressBar.id}
        class="left-0 top-0 bottom-0 w-0 bg-blue-100/25 absolute -z-1"
      />
    </div>
  );
}

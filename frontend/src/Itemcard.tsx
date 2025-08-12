import { BsFileEarmarkSpreadsheet } from "solid-icons/bs";
import {
  FaSolidFile,
  FaSolidFileAudio,
  FaSolidFileImage,
  FaSolidFileInvoice,
  FaSolidFileLines,
  FaSolidFileVideo,
  FaSolidFileZipper,
  FaSolidFolder,
  FaSolidXmark,
} from "solid-icons/fa";
import { createSignal } from "solid-js";
import { Folder, Item } from "./components/drop-zone";

function formatBytes(bytes: number, decimals = 2): string {
  if (bytes === 0) return "0 Bytes";

  const k = 1024;
  const dm = decimals < 0 ? 0 : decimals;
  const sizes = ["Bytes", "KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB"];

  const i = Math.floor(Math.log(bytes) / Math.log(k));

  return (
    Number.parseFloat((bytes / Math.pow(k, i)).toFixed(dm)) + " " + sizes[i]
  );
}

function info(item: Item): string {
  if (item instanceof File) {
    return formatBytes(item.size);
  } else if (item as Folder) {
    return `${item.contents.length} items`;
  } else {
    return "";
  }
}

interface IconAttributes {
  item: Item;
}

function Icon({ item }: IconAttributes) {
  const classes = "size-5";

  if (item instanceof File) {
    const fileType = item.type;
    const fileName = item.name;

    if (
      fileType.includes("pdf") ||
      fileName.endsWith(".pdf") ||
      fileType.includes("word") ||
      fileName.endsWith(".doc") ||
      fileName.endsWith(".docx")
    ) {
      return <FaSolidFileInvoice class={classes} />;
    } else if (
      fileType.includes("txt") ||
      fileName.endsWith(".txt") ||
      fileName.endsWith(".md") ||
      fileName.endsWith(".markdown") ||
      fileName.endsWith(".org")
    ) {
      return <FaSolidFileLines class={classes} />;
    } else if (
      fileType.includes("zip") ||
      fileType.includes("archive") ||
      fileName.endsWith(".zip") ||
      fileName.endsWith(".rar")
    ) {
      return <FaSolidFileZipper class={classes} />;
    } else if (
      fileType.includes("excel") ||
      fileName.endsWith(".xls") ||
      fileName.endsWith(".xlsx")
    ) {
      return <BsFileEarmarkSpreadsheet />;
    } else if (fileType.includes("video/")) {
      return <FaSolidFileVideo class={classes} />;
    } else if (fileType.includes("audio/")) {
      return <FaSolidFileAudio class={classes} />;
    } else if (fileType.startsWith("image/")) {
      return <FaSolidFileImage class={classes} />;
    }
    return <FaSolidFile class={classes} />;
  } else {
    return <FaSolidFolder />;
  }
}

interface ItemcardAttributes {
  item: Item;
  remove: EventListener;
}

export function Itemcard(props: ItemcardAttributes) {
  const { item, remove } = props;

  const [hovering, setHovering] = createSignal(false);

  return (
    <div
      onmouseenter={() => setHovering(true)}
      onmouseleave={() => setHovering(false)}
      class="rounded-lg p-2 break-inside-avoid border"
    >
      <div class="gap-3 flex items-center justify-between">
        <div class="gap-3 flex items-center overflow-hidden">
          <div class="size-10 rounded flex aspect-square items-center justify-center border">
            <Icon item={item} />
          </div>
          <div class="min-w-0 gap-0.5 flex flex-col">
            <p class="font-medium truncate text-[13px]">{item.name}</p>
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
    </div>
  );
}

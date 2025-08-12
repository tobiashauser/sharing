import { Folder, Item } from "~/components/drop-zone";

export function formatBytes(bytes: number, decimals = 2): string {
  if (bytes === 0) return "0 Bytes";

  const k = 1024;
  const dm = decimals < 0 ? 0 : decimals;
  const sizes = ["Bytes", "KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB"];

  const i = Math.floor(Math.log(bytes) / Math.log(k));

  return (
    Number.parseFloat((bytes / Math.pow(k, i)).toFixed(dm)) + " " + sizes[i]
  );
}

export function info(item: Item): string {
  if (item instanceof File) {
    return formatBytes(item.size);
  } else if (item as Folder) {
    return `${item.contents.length} items`;
  } else {
    return "";
  }
}

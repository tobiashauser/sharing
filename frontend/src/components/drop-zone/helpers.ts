import { Item } from "./types";

export function allFiles(item: Item): File[] {
  if (item instanceof File) {
    return [item];
  } else {
    return item.contents.flatMap(allFiles);
  }
}

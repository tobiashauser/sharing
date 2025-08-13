// A (simple) implementation of a drop zone, because all others are
// too complicated. This implementation uses the *File and Directory Entries API*!

import { Accessor, createSignal, JSX, Setter } from "solid-js";
import { Folder, Item } from "./types";

// Maybe I want to change this type at some point.
type ItemId = string;
type ItemIds = Set<ItemId>;

// This must ensure that the same ID is created regardless of input type.
function createId(item: Item): ItemId {
  if (item instanceof File) {
    return (
      "file" +
      (item.webkitRelativePath ? item.webkitRelativePath : item.name) +
      item.size
    );
  } else {
    return "folder" + item.name;
  }
}

/// Internal API

function readFileEntry(entry: FileSystemFileEntry): Promise<File> {
  return new Promise((resolve, reject) => {
    entry.file(
      (file) => {
        resolve(file);
      },
      (error) => {
        reject(error);
      },
    );
  });
}

function readDirectoryEntries(
  reader: FileSystemDirectoryReader,
): Promise<FileSystemEntry[]> {
  return new Promise((resolve, reject) => {
    reader.readEntries(
      (entries) => {
        resolve(entries);
      },
      (error) => {
        reject(error);
      },
    );
  });
}

async function processDirectory(directory: FileSystemDirectoryEntry) {
  const reader = directory.createReader();
  let contents: Item[] = await processDirectoryEntries(
    await readDirectoryEntries(reader),
  );

  return {
    name: directory.name,
    contents: contents,
  } satisfies Folder;
}

async function processDirectoryEntries(entries: FileSystemEntry[]) {
  let contents: Item[] = [];

  for (const entry of entries) {
    if (entry instanceof FileSystemFileEntry) {
      const file = await readFileEntry(entry);
      contents.push(file);
    } else if (entry instanceof FileSystemDirectoryEntry) {
      const subfolderContents = await processDirectory(entry);
      contents.push(subfolderContents);
    }
  }

  return contents;
}

function addItem(
  setItems: Setter<Item[]>,
  getItemIds: Accessor<ItemIds>,
  setItemIds: Setter<ItemIds>,
  onItemAdded?: (item: Item) => void,
) {
  return (item: Item) => {
    const id = createId(item);
    if (!getItemIds().has(id)) {
      setItemIds((prev) => prev.add(id));
      setItems((prev) => [...prev, item]);

      if (onItemAdded) {
        onItemAdded(item);
      }
    }
  };
}

/// Handlers
//
//  They can be thought as user actions such as dropping a folder or
//  selecting a file in the file picker.

function handleDroppedItem(addItem: (item: Item) => void) {
  return async (item: FileSystemEntry) => {
    if (item instanceof FileSystemFileEntry) {
      // Item dropped is a file.
      const file = await readFileEntry(item);
      addItem(file);
    } else if (item instanceof FileSystemDirectoryEntry) {
      // Item dropped is a directory, which must be processed recursively.
      const folder = await processDirectory(item);
      addItem(folder);
    }
  };
}

// Handle file(s) selected in the file picker. Directories are not
// possible. This function must be added to the InputElement's
// `onchange' attribute. A selection is always added at the top level.
function handleSelectedItems(addFile: (file: File) => void): EventListener {
  return (e) => {
    const files = (e.currentTarget as HTMLInputElement).files;
    if (files) {
      for (let i = 0; i < files.length; i++) {
        addFile(files[i]);
      }
    }
  };
}

function handleRemoveItem(
  setItems: Setter<Item[]>,
  setItemIds: Setter<ItemIds>,
) {
  return (item: Item) => {
    return () => {
      const id = createId(item);
      setItems((prev) =>
        prev.filter((storedItem) => createId(storedItem) !== id),
      );
      setItemIds((prev) => {
        prev.delete(id);
        return prev;
      });
    };
  };
}

/// Events
//
//  The dropzone needs to configure various event handlers that handle
//  dragging and clicking.

function onDragEnter(
  setDragging: Setter<boolean>,
  setLastDrag: Setter<number>,
): EventListener {
  return (e) => {
    e.preventDefault();
    e.stopPropagation();

    setDragging(true);
    setLastDrag(Date.now());
  };
}

function onDragLeave(
  setDragging: Setter<boolean>,
  getLastDrag: Accessor<number>,
  delay: number,
  windowDropZone: boolean,
): EventListener {
  return (e) => {
    e.preventDefault();
    e.stopPropagation();

    // This works around unfortunate behaviour when target the entire
    // window as a drop zone.
    if (windowDropZone) {
      const now = Date.now();
      setTimeout(() => {
        if (now > getLastDrag()) {
          setDragging(false);
        }
      }, delay);
    } else {
      setDragging(false);
    }
  };
}

function onDrop(
  setDragging: Setter<boolean>,
  handleDroppedItem: (item: FileSystemEntry) => void,
): EventListener {
  return (e) => {
    e.preventDefault();
    e.stopPropagation();

    setDragging(false);

    // "Type"script...
    if (e instanceof DragEvent) {
      if (e.dataTransfer) {
        const items = e.dataTransfer.items;
        // Since objects aren't really objects, we have to manually iterate.
        for (let i = 0; i < items.length; i++) {
          // @ts-ignore - getAsEntry is not available yet, webkitGetAsEntry may get renamed to getAsEntry.
          // So we are coding defensively here for that. See https://developer.mozilla.org/en-US/docs/Web/API/DataTransferItem/webkitGetAsEntry
          const item = items[i].getAsEntry
            ? // @ts-ignore
              items[i].getAsEntry()
            : items[i].webkitGetAsEntry();
          if (item) {
            handleDroppedItem(item);
          }
        }
      }
    }
  };
}

function onDragOver(): EventListener {
  return (e) => {
    // This necessary to keep the browser from taking over the file(s).
    // See https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/drop_event
    e.preventDefault();
    e.stopPropagation();
  };
}

/// dropzone
//
//  This is the main function that kickstart the creation of a dropzone.
export function dropzone(
  element: Window | HTMLElement,
  multiple?: boolean,
  onItemAdded?: (item: Item) => void,
) {
  // The dropzone must be connected to a <input> element. This
  // requires a bit of a convoluated setup in which we return a setter
  // that must be called inside `setTimout'.
  let inputElement!: HTMLInputElement;
  function bindInputElement(inputReference: HTMLInputElement) {
    inputElement = inputReference;
  }

  // For the attributes of the input element to match the
  // options of the dropzone, the function `configureInputElement' can
  // be used.
  const configureInputElement = ({
    refKey = "ref",
    ...rest
  }): JSX.InputHTMLAttributes<HTMLInputElement> => {
    return {
      [refKey]: inputElement,
      type: "file",
      multiple: multiple,
      onchange: handleSelectedItems(
        addItem(setItems, getItemIds, setItemIds, onItemAdded),
      ),
      ...rest,
    };
  };

  const openFileDialog = () => {
    if (inputElement) {
      inputElement.click();
    }
  };

  // Alright, now that the setup is finally done, we can declare the
  // state we want to manage.
  const [getDragging, setDragging] = createSignal(false);
  const [getLastDrag, setLastDrag] = createSignal(Date.now());
  const [getItems, setItems] = createSignal<Item[]>([]);
  const [getItemIds, setItemIds] = createSignal<ItemIds>(new Set());

  // Finally, we need to attach all kinds of event handlers.
  element.addEventListener("dragenter", onDragEnter(setDragging, setLastDrag));
  element.addEventListener(
    "dragleave",
    onDragLeave(setDragging, getLastDrag, 200, element instanceof Window),
  );
  element.addEventListener("dragover", onDragOver());
  element.addEventListener(
    "drop",
    onDrop(
      setDragging,
      handleDroppedItem(addItem(setItems, getItemIds, setItemIds, onItemAdded)),
    ),
  );

  // And resolve some handlers.
  const removeItem = handleRemoveItem(setItems, setItemIds);

  return {
    bindInputElement,
    configureInputElement,
    getDragging,
    getItems,
    removeItem,
    openFileDialog,
  };
}

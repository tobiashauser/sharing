// A (simple) implementation of a drop zone, because all others are
// too complicated. This implementation uses the *File and Directory Entries API*!

import { Accessor, createSignal, JSX, Setter } from "solid-js";

// Unfortunately, I need to consolidate items that can either be a
// `FileSystemEntry' when dragged or a File when selected. Eventually
// everything needs to be a file so that I can send it to the backend.
// However, that means I have to manually keep track of (recursive)
// directories.

type Item = FileSystemEntry | File;
type Items = Item[];

/// Handlers
//
//  They can be thought as user actions such as dropping a folder or
//  selecting a file in the file picker.

function handleDroppedItem(setItems: Setter<Items>) {
  return (item: FileSystemEntry) => {
    if (item instanceof FileSystemFileEntry) {
      item.file((file) => console.log("-->", file));
    }

    setItems((prev) => {
      if (prev.some((prevItem) => prevItem.fullPath === item.fullPath)) {
        return prev;
      } else {
        return [...prev, item];
      }
    });
  };
}

// Handle file(s) selected in the file picker. Directories are not
// possible. This function must be added to the InputElement's
// `onchange' attribute.
function handleSelectedItems(): EventListener {
  return (e) => {
    const files = (e.currentTarget as HTMLInputElement).files;
    if (files) {
      console.log("==>", files);
    }
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
          const item = items[i].webkitGetAsEntry();
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
export function dropzone(element: Window | HTMLElement, multiple?: boolean) {
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
      onchange: handleSelectedItems(),
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
  const [getItems, setItems] = createSignal<(FileSystemEntry | File)[]>([]);

  // Finally, we need to attach all kinds of event handlers.
  element.addEventListener("dragenter", onDragEnter(setDragging, setLastDrag));
  element.addEventListener(
    "dragleave",
    onDragLeave(setDragging, getLastDrag, 200, element instanceof Window),
  );
  element.addEventListener("dragover", onDragOver());
  element.addEventListener(
    "drop",
    onDrop(setDragging, handleDroppedItem(setItems)),
  );

  return {
    bindInputElement,
    configureInputElement,
    getDragging,
    getItems,
    openFileDialog,
  };
}

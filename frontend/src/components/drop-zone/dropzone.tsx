// A (simple) implementation of a drop zone, because all others are
// too complicated. This implementation uses the *File and Directory Entries API*!

import { Accessor, createSignal, JSX, Setter } from "solid-js";

/// Actions
//
//  Various functions that manage state.

function handleDroppedItem(item: FileSystemEntry) {
  if (item instanceof FileSystemFileEntry) {
    console.log("file", item);
  } else if (item instanceof FileSystemDirectoryEntry) {
    console.log("dir", item);
  } else {
    // [todo] error handling
    console.log("unrecognized", item);
  }
}

/// Events
//
//  The dropzone needs to configure various event handlers that handle
//  dragging and clicking.

function onDragEnter(
  setDragging: Setter<boolean>,
  setLastDrag: Setter<number>,
): EventListenerOrEventListenerObject {
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
): EventListenerOrEventListenerObject {
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
): EventListenerOrEventListenerObject {
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
export function dropzone(element: Window | HTMLElement) {
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
      ...rest,
    };
  };

  // Alright, now that the setup is finally done, we can declare the
  // state we want to manage.
  const [getDragging, setDragging] = createSignal(false);
  const [getLastDrag, setLastDrag] = createSignal(Date.now());
  const [getItems, setItems] = createSignal<FileSystemFileEntry[]>([]);

  // Finally, we need to attach all kinds of event handlers.
  element.addEventListener("dragenter", onDragEnter(setDragging, setLastDrag));
  element.addEventListener(
    "dragleave",
    onDragLeave(setDragging, getLastDrag, 200, element instanceof Window),
  );
  element.addEventListener("dragover", onDragOver());
  element.addEventListener("drop", onDrop(setDragging));

  return {
    bindInputElement,
    configureInputElement,
    getDragging,
  };
}

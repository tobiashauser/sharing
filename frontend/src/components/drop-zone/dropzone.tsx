// A (simple) implementation of a drop zone, because all others are
// too complicated. This implementation uses the *File and Directory Entries API*!

import { Accessor, createSignal, JSX, Setter } from "solid-js";

/// Actions
//
//  The dropzone needs to configure various event handlers that handle
//  dragging and clicking.

function dragenter(
  setIsDragging: Setter<boolean>,
  setLastDrag: Setter<number>,
): EventListenerOrEventListenerObject {
  return (e) => {
    e.preventDefault();
    e.stopPropagation();

    setIsDragging(true);
    setLastDrag(Date.now());
  };
}

function dragleave(
  setIsDragging: Setter<boolean>,
  lastDrag: Accessor<number>,
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
        if (now > lastDrag()) {
          setIsDragging(false);
        }
      }, delay);
    } else {
      setIsDragging(false);
    }
  };
}

function drop(
  setIsDragging: Setter<boolean>,
): EventListenerOrEventListenerObject {
  return (e) => {
    e.preventDefault();
    e.stopPropagation();

    setIsDragging(false);

    console.log(new DataTransferItem().webkitGetAsEntry());
  };
}

function dragover(): EventListener {
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
  const [isDragging, setIsDragging] = createSignal(false);
  const [lastDrag, setLastDrag] = createSignal(Date.now());

  // Finally, we need to attach all kinds of event handlers.
  element.addEventListener("dragenter", dragenter(setIsDragging, setLastDrag));
  element.addEventListener(
    "dragleave",
    dragleave(setIsDragging, lastDrag, 200, element instanceof Window),
  );
  element.addEventListener("dragover", dragover());
  element.addEventListener("drop", drop(setIsDragging));

  return {
    bindInputElement,
    configureInputElement,
    isDragging,
  };
}

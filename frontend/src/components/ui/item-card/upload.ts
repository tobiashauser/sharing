// Utilities and functions to make it easy to upload dropped items.
import { Folder, Item } from "~/components/drop-zone";

/// HELPERS

function formData(payload: File): FormData {
  const formData = new FormData();
  formData.append("file", payload);
  return formData;
}

/// XMLHttpRequest

// The state of a request can be managed with callbacks (as usual..).

// The 'load' event is fired when an XMLHttpRequest transaction completes successfully.
// This is called before `handleReqeustLoadEnd'.
// function handleRequestLoad(req: XMLHttpRequest) {
//   return (e: ProgressEvent) => {
//     console.log("handleRequestLoad");
//     if (req.readyState === XMLHttpRequest.DONE && req.status === 200) {
//       console.info("Finished upload successfully.");
//       // Request finished. Do processing here.
//     } else {
//       console.warn("Unhandled state of request:", req.statusText);
//     }
//   };
// }

// The 'progress' event is fired periodically when a request receives more data.
function handleRequestProgress(event: ProgressEvent) {
  if (event.lengthComputable) {
    const percentComplete = Math.floor((event.loaded / event.total) * 100);
    console.log("==>", percentComplete);
    // …
  } else {
    // Unable to compute progress information since the total size is unknown
  }
}

/// Internal API

// A basic function that handles uploading a single file.
function uploadFile(file: File, address: string) {
  const payload = formData(file);

  // Create and configure the request.
  const request = new XMLHttpRequest();
  // request.upload.onabort = handleRequestAbort;
  // request.upload.onerror = handleRequestError;
  // request.upload.onload = handleRequestLoad(request);
  // request.upload.onloadstart = handleRequestLoadStart;
  request.upload.onprogress = handleRequestProgress;
  // request.upload.ontimeout = handleRequestTimeout;

  // Finally, we need to open and send the configured request.
  request.open("POST", address, true);
  request.send(payload);
}

function uploadFolder(folder: Folder, address: string) {}

export function uploadItem(item: Item, address: string) {
  console.log("Uploading:", item.name);

  // aaand dispatch!
  if (item instanceof File) {
    uploadFile(item, address);
  } else {
    uploadFolder(item, address);
  }
}

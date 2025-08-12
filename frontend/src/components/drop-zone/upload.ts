// Utilities and functions to make it easy to upload dropped items.

/// HELPERS

function formData(payload: File): FormData {
  const formData = new FormData();
  formData.append("file", payload);
  return formData;
}

/// XMLHttpRequest

// The state of a request can be managed with callbacks (as usual..).

function handleRequestAbort(e: ProgressEvent) {
  console.log("abort");
}

function handleRequestError(e: ProgressEvent) {
  console.log("error");
}

// The 'load' event is fired when an XMLHttpRequest transaction completes successfully.
// This is called before `handleReqeustLoadEnd'.
function handleRequestLoad(e: ProgressEvent) {
  console.log("finished upload");
}

function handleRequestLoadStart(e: ProgressEvent) {
  console.log("starting upload");
}

function handleRequestLoadEnd(e: ProgressEvent) {
  console.log("ended upload");
}

// The 'progress' event is fired periodically when a request receives more data.
function handleRequestProgress(e: ProgressEvent) {
  console.log("progress");
}

function handleRequestTimeout(e: ProgressEvent) {
  console.log("timed out");
}

/// API

// A basic function that handles uploading a single file.
export function uploadFile(address: string) {
  return (file: File) => {
    console.log("uploading file:", file.name);
    const payload = formData(file);

    // Create and configure the request.
    const request = new XMLHttpRequest();
    request.upload.onabort = handleRequestAbort;
    request.upload.onerror = handleRequestError;
    request.upload.onload = handleRequestLoad;
    request.upload.onloadend = handleRequestLoadEnd;
    request.upload.onloadstart = handleRequestLoadStart;
    request.upload.onprogress = handleRequestProgress;
    request.upload.ontimeout = handleRequestTimeout;

    // Finally, we need to open and send the configured request.
    request.open("POST", address, true);
    request.send(payload);
  };
}

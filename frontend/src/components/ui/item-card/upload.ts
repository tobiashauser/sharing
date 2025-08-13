// Utilities and functions to make it easy to upload dropped items.
import { Setter } from "solid-js";
import { Folder, Item } from "~/components/drop-zone";

/// HELPERS

function formData(payload: File): FormData {
  const formData = new FormData();
  formData.append("file", payload);
  return formData;
}

function url(
  base: string,
  endpoint: string,
  build: (pairs: Map<string, any>) => void = () => {},
): URL {
  const url = new URL(endpoint, base);
  const params: Map<string, any> = new Map();
  build(params);
  for (const [key, value] of params) {
    url.searchParams.append(key, encodeURIComponent(value));
  }
  return url;
}

/// XMLHttpRequest

// The state of a request can be managed with callbacks (as usual..).

function handleRequestLoadEnd(
  req: XMLHttpRequest,
  setUploaded: Setter<boolean>,
) {
  return () => {
    if (req.readyState === XMLHttpRequest.DONE && req.status === 200) {
      setUploaded(true);
    } else {
      console.warn("Unhandled end of loading.");
    }
  };
}

// The 'progress' event is fired periodically when a request receives more data.
function handleRequestProgress(setProgress: Setter<number>) {
  return (event: ProgressEvent) => {
    if (event.lengthComputable) {
      const percentComplete = Math.floor((event.loaded / event.total) * 100);
      setProgress(percentComplete);
    } else {
      console.warn("Unhandled control flow: event's length is not computable.");
    }
  };
}

/// Internal API

// A basic function that handles uploading a single file.
function uploadFile(
  file: File,
  baseUrl: string,
  endpoint: string,
  setProgress: Setter<number>,
  setUploaded: Setter<boolean>,
  sessionToken: string,
) {
  const payload = formData(file);

  // Create and configure the request.
  const requestUrl = url(baseUrl, endpoint);
  const request = new XMLHttpRequest();

  // Event handlers.
  request.onloadend = handleRequestLoadEnd(request, setUploaded);
  // This must be added to the upload request.
  request.upload.onprogress = handleRequestProgress(setProgress);

  // Finally, we need to open and send the configured request.
  request.open("POST", requestUrl, true);
  request.setRequestHeader("SESSION", sessionToken);
  // Pass along the path to the (sub)directory for folders.
  if (file.webkitRelativePath) {
    request.setRequestHeader("PATH", file.webkitRelativePath);
  }
  request.send(payload);
}

function uploadFolder(
  folder: Folder,
  baseUrl: string,
  endpoint: string,
  setProgress: Setter<number>,
  setUploaded: Setter<boolean>,
  sessionToken: string,
) {
  for (const item of folder.contents) {
    uploadItem(item, baseUrl, endpoint, setProgress, setUploaded, sessionToken);
  }
}

/// Public API

export function uploadItem(
  item: Item,
  baseUrl: string,
  endpoint: string,
  setProgress: Setter<number>,
  setUploaded: Setter<boolean>,
  sessionToken: string,
) {
  // aaand dispatch!
  if (item instanceof File) {
    uploadFile(item, baseUrl, endpoint, setProgress, setUploaded, sessionToken);
  } else {
    uploadFolder(
      item,
      baseUrl,
      endpoint,
      setProgress,
      setUploaded,
      sessionToken,
    );
  }
}

// This function sends a request to the server to remove a selected file.
export function removeItem(
  item: Item,
  baseUrl: string,
  endpoint: string,
  sessionToken: string,
  onComplete?: EventListener,
) {
  const requestURL = url(baseUrl, endpoint, (pairs) => {
    pairs.set("path", item.name);
  });
  const request = new XMLHttpRequest();

  // Add various handlers.
  request.onloadend = (e) => {
    if (request.readyState === XMLHttpRequest.DONE && request.status === 200) {
      if (onComplete) {
        onComplete(e);
      }
    }
  };

  request.open("DELETE", requestURL, true);
  request.setRequestHeader("SESSION", sessionToken);
  request.send();
}

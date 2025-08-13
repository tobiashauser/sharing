// Wrap XMLHttpRequest to make it confortable to live with. For,
// instance I don't really care what error occurred, just that one
// happened. Furthermore, I need provide loopholes to update some
// external state, because this should not be mixed with the request
// logic.

import { Accessor, createSignal, Setter } from "solid-js";
import { allFiles } from "../helpers";
import { Item } from "../types";
import { UploadStatus } from "./types";

/// Helpers

function createUrl(
  base: string,
  endpoint: string,
  build: (params: Map<string, any>) => void = () => {},
): URL {
  const url = new URL(endpoint, base);
  const params: Map<string, any> = new Map();
  build(params);
  for (const [key, value] of params) {
    url.searchParams.append(key, encodeURIComponent(value));
  }
  return url;
}

function formData(payload: File): FormData {
  const formData = new FormData();
  formData.append("file", payload);
  return formData;
}

/// UploadRequest

export class UploadRequest {
  private readonly _item: Item;
  private readonly _server: string;
  private readonly _session: string;
  private readonly _requests = new Set<XMLHttpRequest>();
  private readonly _data = new Map<XMLHttpRequest, FormData>();

  private readonly _status = createSignal(UploadStatus.unsent);

  private readonly _getStatus: Accessor<UploadStatus>;
  private readonly _setStatus: Setter<UploadStatus>;

  constructor(item: Item, server: string, session: string) {
    const [getStatus, setStatus] = createSignal(UploadStatus.unsent);

    this._item = item;
    this._server = server;
    this._session = session;
    this._getStatus = getStatus;
    this._setStatus = setStatus;
  }

  /// Internal API

  createFileRequest = (file: File): XMLHttpRequest => {
    const request = new XMLHttpRequest();
    const url = createUrl(this._server, "/upload");

    // Configure the event handlers.
    request.onloadend = () => {
      if (request.status === 200) {
        this._setStatus(UploadStatus.succeeded);
        return;
      }

      if (request.status !== 0) {
        this._setStatus(UploadStatus.failed);
        return;
      }
    };

    if (!this._data.has(request)) {
      console.log("Creating FormData");
      this._data.set(request, formData(file));
    }

    // Open an configure the request.
    request.open("POST", url, true);
    request.setRequestHeader("SESSION", this._session);
    if (file.webkitRelativePath) {
      request.setRequestHeader("PATH", file.webkitRelativePath);
    }
    return request;
  };

  /// API

  // Send the request to the server.
  send() {
    allFiles(this._item)
      .map(this.createFileRequest)
      .forEach((req) => {
        this._requests.add(req);
        if (this._data.has(req)) {
          req.send(this._data.get(req));
        }
      });

    console.log("Sending", this._item.name);
    this._setStatus(UploadStatus.ongoing);
  }

  // Abort the request.
  abort() {
    this._requests.forEach((req) => req.abort());
  }

  get status() {
    return this._getStatus;
  }
}

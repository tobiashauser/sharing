// Hook up some basic event listeners to the window.
//
// For documentation of the lifecycle (and why `pushEvent` is in
// scope) see:
// https://hexdocs.pm/phoenix_live_view/js-interop.html#client-hooks-via-phx-hook

function readFileEntry(entry) {
  return new Promise((resolve, reject) => {
    entry.file(file => {
      resolve([ref, file])
      ref += 1
    }, reject)
  })
}

async function readDirectoryEntry(dirEntry) {
  const result = [];

  // Helper: grab ALL entries from a reader (handles pagination).
  const readAll = (reader) =>
    new Promise((resolve, reject) => {
      const all = [];
      (function read() {
        reader.readEntries(
          (batch) => {
            if (batch.length === 0) resolve(all);
            else {
              all.push(...batch);
              read(); // keep reading until empty
            }
          },
          reject
        );
      })();
    });

  async function walk(entry, path = "") {
    if (entry.isFile) {
      const [ref, file] = await readFileEntry(entry);
      // Only push to the result if an id for the file doesn't exist
      // yet. 
      if (!fileIds.includes(fileId(file.name, path))) {
        result.push([ref, path, file]);
        console.log("==>", ref, path, file)
      }
    } else if (entry.isDirectory) {
      const entries = await readAll(entry.createReader());
      await Promise.all(entries.map(e => walk(e, e.fullPath)));
    }
  }

  await walk(dirEntry);
  return result;
}

function fileId(prefix, suffix = "") {
  return prefix + ":" + suffix
}

// Keep track of a little internal state to work around the event
// triggers for every element.
let state = 0
let fileIds = []

// Keep count of the current ref.
let ref = 0

// Add this object to the livesocket configuration:
//
//    new LiveSocket(hooks: { ..., WindowDragEvents })
export default WindowDragEvents = {
  mounted() {
    console.log("Mounted window drag events")
    state = 0;
    
    this.handleDragEnter = (event) => {
      event.preventDefault();
      event.stopPropagation();
      state++;

      if (state == 1) {
        this.el.setAttribute("data-dragging", true);
      }
    };

    this.handleDragLeave = (event) => {
      event.preventDefault();
      event.stopPropagation();
      state--;

      if (state == 0) {
        this.el.setAttribute("data-dragging", "false");
      };
    };

    this.handleDragOver = (event) => {
      event.preventDefault();
      event.stopPropagation();
    };

    this.handleDrop = (event) => {
      event.preventDefault();
      event.stopPropagation();
      
      // The drag event has ended.
      this.el.setAttribute("data-dragging", "false");
      state = 0;

      const entries = event.dataTransfer.items;
      for (let i = 0; i < entries.length; i++) {
        const entry = entries[i].getAsEntry ? entries[i].getAsEntry() : entries[i].webkitGetAsEntry();
        if (entry instanceof FileSystemFileEntry) {
	  readFileEntry(entry)
	    .then(([_, file]) => {
              // This check is uncomplicated since we can only assume
              // that we are at the top level and have no nesting.
	      if (!fileIds.includes(fileId(file.name))) {
                // Inject the file into the socket.
		this.upload("files", [file])
	      }
	    })
        } else {
	  readDirectoryEntry(entry)
	    .then(files => {
              const paths = files.reduce((acc, [ref, path, _]) => {
                acc.set(String(ref), path)
                return acc
              }, new Map())
              console.log("==>", paths)
              this.pushEvent("directories", Object.fromEntries(paths))
              // Inject the files into the socket. This will call
              // "validate" on the server.
              this.upload("files", files.map(([ref, path, file]) => file))
	    })
	}
      }
    };

    // An event triggered from the server that sends a list of of ids
    // of the files that are currently selected in the uploader. This
    // allows us to skip adding duplicates.
    //
    // [FIXME] Currently, we are not hooking into the file selection
    // from the picker, only the drag and drop events. So it is still
    // possible to add the same file multiple times.
    //
    // The id is <file.name>:<file.webkitRelativePath>.
    this.handleFileIds = (event) => {
      fileIds = event.detail.ids
      console.log("fileIds", fileIds)
    };

    window.addEventListener("dragenter", this.handleDragEnter);
    window.addEventListener("dragleave", this.handleDragLeave);
    window.addEventListener("dragover", this.handleDragOver);
    window.addEventListener("drop", this.handleDrop);
    window.addEventListener("phx:file-ids", this.handleFileIds);
  },

  destroyed() {
    window.removeEventListener("dragenter", this.handleDragEnter);
    window.removeEventListener("dragleave", this.handleDragLeave);
    window.removeEventListener("dragover", this.handleDragOver);
    window.removeEventListener("drop", this.handleDrop);
    window.removeEventListener("phx:file-ids", this.handleFileIds);
  }
};

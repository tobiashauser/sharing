// Hook up some basic event listeners to the window.
//
// For documentation of the lifecycle (and why `pushEvent` is in
// scope) see:
// https://hexdocs.pm/phoenix_live_view/js-interop.html#client-hooks-via-phx-hook

function readFileEntry(entry, path) {
  return new Promise((resolve, reject) => {
    entry.file(
      file => resolve([file, path]),
      reject
    )
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

  async function walk(entry, path) {
    if (entry.isFile) {
      const file = await readFileEntry(entry, path);
      result.push(file);
    } else if (entry.isDirectory) {
      const entries = await readAll(entry.createReader());
      await Promise.all(entries.map(e => walk(e, e.fullPath)));
    }
  }

  await walk(dirEntry);
  return result;
}

// Keep track of a little internal state to work around the event
// triggers for every element.
let state = 0
let uploadedFiles = []
let ref = 0
let disabled = false

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
      if (disabled) return;
      state++;

      if (state == 1) {
        this.el.setAttribute("data-dragging", true);
      }
    };

    this.handleDragLeave = (event) => {
      event.preventDefault();
      event.stopPropagation();
      if (disabled) return;
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
      if (disabled) return;
      
      // The drag event has ended.
      this.el.setAttribute("data-dragging", "false");
      state = 0;

      const entries = event.dataTransfer.items;
      for (let i = 0; i < entries.length; i++) {
        const entry = entries[i].getAsEntry ? entries[i].getAsEntry() : entries[i].webkitGetAsEntry();

        // Handle a single dropped file.
        if (entry.isFile) {
	  readFileEntry(entry, "")
	    .then(([file, _]) => {
              if (!uploadedFiles.includes(file.name)) {
		this.upload("files", [file])
                ++ref;
                uploadedFiles.push(file.name)
	      }
	    })
        }

        // Handle a dropped directory.
        else if (entry.isDirectory) {
	  readDirectoryEntry(entry)
	    .then(files => {
              let filesAndRef = files
                .filter(([_ , path]) => !uploadedFiles.includes(path))
                .map(([file, path]) => {
                  ++ref;
                  uploadedFiles.push(path)
                  return [file, path, ref - 1]
                })

              const filteredFiles = filesAndRef
                .map(([file, _]) => file)
              this.upload("files", filteredFiles)

              // Send the paths to the server. This must be called AFTER `upload'!
              const directories = filesAndRef
                .reduce((acc, [_, path, ref]) => {
                  acc.set(ref, path)
                  return acc
                }, new Map())
             this.pushEvent("directories", Object.fromEntries(directories))
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
      uploadedFiles = event.detail.ids
      console.log(uploadedFiles)
    };

    this.handleCloseUploads = (e) => {
      disabled = e.detail.val
    };

    window.addEventListener("dragenter", this.handleDragEnter);
    window.addEventListener("dragleave", this.handleDragLeave);
    window.addEventListener("dragover", this.handleDragOver);
    window.addEventListener("drop", this.handleDrop);
    window.addEventListener("phx:file-ids", this.handleFileIds);
    window.addEventListener("phx:close-uploads", this.handleCloseUploads);
  },

  destroyed() {
    window.removeEventListener("dragenter", this.handleDragEnter);
    window.removeEventListener("dragleave", this.handleDragLeave);
    window.removeEventListener("dragover", this.handleDragOver);
    window.removeEventListener("drop", this.handleDrop);
    window.removeEventListener("phx:file-ids", this.handleFileIds);
    window.removeEventListener("phx:close-uploads", this.handleCloseUploads);
  }
};

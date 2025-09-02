// Hook up some basic event listeners to the window.
//
// For documentation of the lifecycle (and why `pushEvent` is in
// scope) see:
// https://hexdocs.pm/phoenix_live_view/js-interop.html#client-hooks-via-phx-hook

function readFileEntry(entry) {
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

function readDirectoryEntry(entry) {
  return new Promise(resolve => {
    // console.log(entry)
    let result = [];
    processEntry(entry, result)
    // console.log(result)
    resolve(result)
  })
}

function processEntry(entry, result) {
  if (entry instanceof FileSystemFileEntry) {
    entry.file(file => result.push(file))
  } else {
    const reader = entry.createReader();
    reader.readEntries(entries => {
      entries.forEach(entry => {
	processEntry(entry, result)
      })
    })
  }
}

// Keep track of a little internal state to work around the event
// triggers for every element.
let state = 0

export default WindowDragEvents = {
  mounted() {
    state = 0;
    
    this.handleDragEnter = (event) => {
      event.preventDefault();
      event.stopPropagation();
      state++;

      if (state == 1) {
	this.pushEvent("dragenter");
      }
    };

    this.handleDragLeave = (event) => {
      event.preventDefault();
      event.stopPropagation();
      state--;

      if (state == 0) {
	this.pushEvent("dragleave");
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
      this.pushEvent("dragleave");
      state = 0;

      const entries = event.dataTransfer.items;
      for (let i = 0; i < entries.length; i++) {
        const entry = entries[i].getAsEntry ? entries[i].getAsEntry() : entries[i].webkitGetAsEntry();
        if (entry instanceof FileSystemFileEntry) {
	  readFileEntry(entry)
	    .then(file => this.upload("files", [file]))
        } else {
	  readDirectoryEntry(entry)
	    .then(files => console.log(files.length == 0))
	}
      }
    };

    window.addEventListener("dragenter", this.handleDragEnter);
    window.addEventListener("dragleave", this.handleDragLeave);
    window.addEventListener("dragover", this.handleDragOver);
    window.addEventListener("drop", this.handleDrop);
  },

  destroyed() {
    window.removeEventListener("dragenter", this.handleDragEnter);
    window.removeEventListener("dragleave", this.handleDragLeave);
    window.removeEventListener("dragover", this.handleDragOver);
    window.removeEventListener("drop", this.handleDrop);
  }
};

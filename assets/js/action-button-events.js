// Animations of the action button.
import { ID, Scene } from "./gsap"
import { log } from "./state"

function dark() {
  return window.matchMedia
    ? window.matchMedia('(prefers-color-scheme: dark)').matches
    : false; // safe fallback for very old browsers
}

export default ActionButtonEvents = {
  mounted() {
    log("Listening to action button events")

    this.actionButton = new ID("action-button")
    
    this.input = new ID("ab-input");
    this.inputContent = new ID("ab-input-content");

    this.download = new ID("ab-download");
    this.downloadContent = new ID("ab-download-content");

    this.button = new ID("ab-enter-code");
    this.buttonContent = new ID("ab-enter-code-content");

    this.codeLabel = new ID("ab-code-label");
    this.codeLabelContent = new ID("ab-code-label-content");

    this.code = new ID("ab-code");
    this.codeContent = new ID("ab-code-content");

    const expand = { width: "auto" };
    const contract = { width: 0 };
    const hide = { autoAlpha: 0 };
    const show = { autoAlpha: 1 };

    const showInput = new Scene()
      .onStart(() => {
        this.button.element.classList.add("pointer-events-none")
        this.button.element.classList.add("dark:bg-surface/60")
      })
      .stage("remove", [
        this.buttonContent.to(hide),
        this.download.to(show)
      ])
      .stage("resize", [
        this.input.to(expand),
        this.button.to(contract),
        this.download.to(expand)
      ])
      .stage("insert", [
        this.inputContent.to(show),
        this.downloadContent.to(show)
      ])
      .onComplete(() => {
        this.inputContent.element.focus()
        this.button.element.classList.remove("pointer-events-none")
      })

    const showButton = new Scene()
      .onStart(() => {
        this.button.element.classList.add("pointer-events-none")
        this.button.element.classList.remove("dark:bg-surface/60")
      })
      .stage("remove", [
        this.inputContent.to(hide),
        this.downloadContent.to(hide),
        this.codeContent.to(hide),
        this.codeLabelContent.to(hide),
        this.download.to(hide, "remove", dark)
      ])
      .stage("resize", [
        this.button.to(expand),
        this.input.to(contract),
        this.download.to(contract),
        this.code.to(contract),
      ])
      .stage("show", [
        this.buttonContent.to(show)
      ])
      .onComplete(() => {
        this.button.element.classList.remove("pointer-events-none")
      })

    const showCode = new Scene()
      .onStart(() => {
        this.inputContent.element.value = ""
        this.button.element.classList.add("pointer-events-none")
        this.button.element.classList.add("dark:bg-surface/60")
      })
      .stage("remove", [
        this.buttonContent.to(hide),
        this.inputContent.to(hide)
      ])
      .stage("resize", [
        this.button.to(contract),
        this.codeLabel.to(expand),
        this.code.to(expand),
      ])
      .stage("insert", [
        this.codeLabelContent.to(show),
        this.codeContent.to(show)
      ])
      .onComplete(() => {
        this.button.element.classList.remove("pointer-events-none")
      })

    this.handleShowInput = (_) => {
      showInput.animate()
    }

    this.handleShowButton = (_) => {
      showButton.animate()
    }

    this.handleShowCode = (e) => {
      showCode.animate()

      // Also display the qr code in the drop area.
      // img = document.getElementById("qr-code")
      // img.src = "/store/" + e.detail.code + ".svg"

      // Delay showing the image.
      // setTimeout(() => {
      //   img.classList.remove("opacity-0")
      // }, 300)
    }

    // Initiates a download from a given code, while performing some
    // validation that the file actually exists.
    this.getCode = (code) => {
      this.pushEvent("submit-code", { code: code })
        .then(reply => {
          if (reply.continue) {
            showButton.animate()
            // Fetch the download asynchronously.
            fetch(code)
              .then(res => res.blob())
              .then(blob => {
                const url = URL.createObjectURL(blob);
                const a = document.createElement("a");
                a.classList.add("sr-only")
                a.href = url;
                a.download = code + ".zip";
                a.click();
                URL.revokeObjectURL(url);

                // Now we can empty the field again.
                this.inputContent.element.value = ""
              });
          } else {
            this.actionButton.element.setAttribute("data-invalid", true)
          }
        })
    }

    this.handleInputContentKeydown = (e) => {
      const value = e.target.value
      if (e.key == "Escape") {
        showButton.animate()
        this.actionButton.element.setAttribute("data-invalid", "")
      } else if (e.key == "Enter" && value != "" && value != undefined) {
        this.getCode(e.target.value.trim())
      } else {
        // Remove any possible invalid state from the action button.
        this.actionButton.element.setAttribute("data-invalid", "")
      }
    }

    this.handleDownloadClick = (_) => {
      const code = this.inputContent.element.value
      if (code != "" && code != undefined) {
        this.getCode(code.trim())
      }
    }

    this.handleInputContentBlur = (e) => {
      const value = e.target.value
      if (value == "" || value == undefined) {
        showButton.animate()
      }
    }

    window.addEventListener("phx:show-input", this.handleShowInput)
    window.addEventListener("phx:show-button", this.handleShowButton)
    window.addEventListener("phx:show-code", this.handleShowCode)
    this.inputContent.element.addEventListener("blur", this.handleInputContentBlur)
    this.inputContent.element.addEventListener("keydown", this.handleInputContentKeydown)
    this.download.element.addEventListener("click", this.handleDownloadClick)
  },

  destroyed() {
    window.removeEventListener("phx:show-input", this.handleShowInput)
    window.removeEventListener("phx:show-button", this.handleShowButton)
    window.removeEventListener("phx:show-code", this.handleShowCode)
    this.inputContent.element.removeEventListener("blur", this.handleInputContentBlur)
    this.inputContent.element.removeEventListener("keydown", this.handleInputKeydown)
    this.download.element.removeEventListener("click", this.handleDownloadClick)
  }
}

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
    
    const input = new ID("ab-input");
    const inputContent = new ID("ab-input-content");

    const download = new ID("ab-download");
    const downloadContent = new ID("ab-download-content");

    const button = new ID("ab-enter-code");
    const buttonContent = new ID("ab-enter-code-content");

    const codeLabel = new ID("ab-code-label");
    const codeLabelContent = new ID("ab-code-label-content");

    const code = new ID("ab-code");
    const codeContent = new ID("ab-code-content");

    const expand = { width: "auto" };
    const contract = { width: 0 };
    const hide = { autoAlpha: 0 };
    const show = { autoAlpha: 1 };

    const showInput = new Scene()
      .onStart(() => {
        inputContent.element.value = ""
        button.element.classList.add("pointer-events-none")
        button.element.classList.add("dark:bg-surface/60")
      })
      .stage("remove", [buttonContent.to(hide), download.to(show)])
      .stage("resize", [input.to(expand), button.to(contract), download.to(expand)])
      .stage("insert", [inputContent.to(show), downloadContent.to(show)])
      .onComplete(() => {
        inputContent.element.focus()
        button.element.classList.remove("pointer-events-none")
      })

    const showButton = new Scene()
      .onStart(() => {
        button.element.classList.add("pointer-events-none")
        button.element.classList.remove("dark:bg-surface/60")
      })
      .stage("remove", [
        inputContent.to(hide),
        downloadContent.to(hide),
        codeContent.to(hide),
        codeLabelContent.to(hide),
        download.to(hide, "remove", dark)
      ])
      .stage("resize", [
        button.to(expand),
        input.to(contract),
        download.to(contract),
        code.to(contract),
      ])
      .stage("show", [buttonContent.to(show)])
      .onComplete(() => {
        button.element.classList.remove("pointer-events-none")
      })

    const showCode = new Scene()
      .onStart(() => {
        inputContent.element.value = ""
        button.element.classList.add("pointer-events-none")
        button.element.classList.add("dark:bg-surface/60")
      })
      .stage("remove", [buttonContent.to(hide), inputContent.to(hide)])
      .stage("resize", [
        button.to(contract),
        codeLabel.to(expand),
        code.to(expand),
      ])
      .stage("insert", [codeLabelContent.to(show), codeContent.to(show)])
      .onComplete(() => {
        button.element.classList.remove("pointer-events-none")
      })

    this.handleShowInput = (_) => {
      showInput.animate()
    }

    this.handleShowButton = (_) => {
      showButton.animate()
    }

    this.handleShowCode = (_) => {
      showCode.animate()
    }

    this.handleInputContentBlur = (_) => {
      showButton.animate()
    }

    this.handleInputContentKeydown = (e) => {
      if (e.key == "Escape") {
        showButton.animate()
      } else if (e.key == "Enter" && e.target.value != "" && e.target.value != undefined) {
        this.pushEvent("submit-code", { code: e.target.value })
        e.target.blur()
      }
    }

    this.handleDownloadClick = (_) => {
      if (inputContent.element.value != "" && inputContent.element.value != undefined) {
        this.pushEvent("submit-code", { code: inputContent.element.value })
        inputContent.element.blur()
      }
    }

    window.addEventListener("phx:show-input", this.handleShowInput)
    window.addEventListener("phx:show-button", this.handleShowButton)
    window.addEventListener("phx:show-code", this.handleShowCode)
    inputContent.element.addEventListener("blur", this.handleInputContentBlur)
    inputContent.element.addEventListener("keydown", this.handleInputContentKeydown)
    download.element.addEventListener("click", this.handleDownloadClick)
  },

  destroyed() {
    window.removeEventListener("phx:show-input", this.handleShowInput)
    window.removeEventListener("phx:show-button", this.handleShowButton)
    window.removeEventListener("phx:show-code", this.handleShowCode)
    inputContent.element.removeEventListener("blur", this.handleInputContentBlur)
    inputContent.element.removeEventListener("keydown", this.handleInputKeydown)
  }
}

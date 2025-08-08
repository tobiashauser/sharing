import { createSignal, JSX } from "solid-js";

import {
  defaultWindowDropzoneOptions,
  parseAccept,
  transformFiles,
  validateFiles,
} from "./helpers";

import {
  FileErrors,
  GetInputPropsOptions,
  UploadFile,
  WindowDropzone,
  WindowDropzoneOptions,
} from "./types";

export const createWindowDropzone = (
  props: WindowDropzoneOptions = defaultWindowDropzoneOptions,
): WindowDropzone => {
  // Options.
  const { disabled, accept } = props;

  // State.
  let inputRef!: HTMLInputElement;
  const [files, setFiles] = createSignal<UploadFile[]>([]);
  const [isDragging, setIsDragging] = createSignal(false);
  const [_enteredTime, _setEnteredTime] = createSignal(Date.now());
  const [errors, setErrors] = createSignal<FileErrors>({});

  // Setup.
  const getInputProps = ({
    refKey = "ref",
    ...rest
  }: GetInputPropsOptions = {}): JSX.InputHTMLAttributes<HTMLInputElement> => {
    return {
      [refKey]: inputRef,
      tabIndex: -1,
      type: "file",
      ...rest,
    };
  };

  const setRefs = (inputElement: HTMLInputElement) => {
    inputRef = inputElement;
  };

  // Internal.
  const _parcedAccept = parseAccept(accept);
  const _handleFiles = (files: FileList) => {
    if (disabled) return;

    const transformedFiles = transformFiles(files);

    const newErrors: FileErrors = validateFiles(
      transformedFiles,
      _parcedAccept,
    );
    const validFiles = transformedFiles.filter((file) => !newErrors[file.name]);

    // Combine the errors with any existing ones.
    setErrors((prev) => {
      return {
        ...prev,
        ...newErrors,
      };
    });

    setFiles((prev) => [...prev, ...validFiles]);

    // I don't know for what this could be useful...
    if (inputRef) {
      const dataTransfer = new DataTransfer();
      validFiles.forEach((file) => dataTransfer.items.add(file.file));
      const filesList = dataTransfer.files;
      inputRef.files = filesList;
    }
  };

  // Actions.
  const handleDrop = (e: DragEvent) => {
    e.preventDefault();
    e.stopPropagation();

    if (disabled) return;

    const droppedFiles = e.dataTransfer?.files;

    if (droppedFiles && droppedFiles.length) {
      _handleFiles(droppedFiles);
    }
  };

  window.addEventListener("drop", handleDrop);

  const handleDragOver = (e: DragEvent) => {
    e.preventDefault();
    e.stopPropagation();

    if (disabled) return;
  };

  window.addEventListener("dragover", handleDragOver);

  const handleDragEnter = (e: DragEvent) => {
    e.preventDefault();
    e.stopPropagation();

    if (disabled) return;

    setIsDragging(true);
    _setEnteredTime(Date.now());
  };

  window.addEventListener("dragenter", handleDragEnter);

  const handleDragLeave = (e: DragEvent) => {
    e.preventDefault();
    e.stopPropagation();

    if (disabled) return;

    const now = Date.now();
    setTimeout(() => {
      if (now > _enteredTime()) {
        setIsDragging(false);
      }
    }, 100);
  };

  window.addEventListener("dragleave", handleDragLeave);

  // Return.
  return {
    errors,
    files,
    getInputProps,
    isDragging,
    setRefs,
  };
};

export default createWindowDropzone;

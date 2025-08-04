import './App.css'

import { Button } from "@/components/ui/button"

import {
  AlertCircleIcon,
  FileArchiveIcon,
  FileIcon,
  FileSpreadsheetIcon,
  FileTextIcon,
  FileUpIcon,
  HeadphonesIcon,
  ImageIcon,
  VideoIcon,
  XIcon,
} from "lucide-react"

import {
  formatBytes,
  useFileUpload,
} from "@/hooks/use-file-upload"

const getFileIcon = (file: { file: File | { type: string; name: string } }) => {
  const fileType = file.file instanceof File ? file.file.type : file.file.type
  const fileName = file.file instanceof File ? file.file.name : file.file.name

  if (
    fileType.includes("pdf") ||
      fileName.endsWith(".pdf") ||
      fileType.includes("word") ||
      fileName.endsWith(".doc") ||
      fileName.endsWith(".docx")
  ) {
    return <FileTextIcon className="size-4 opacity-60" />
  } else if (
    fileType.includes("zip") ||
      fileType.includes("archive") ||
      fileName.endsWith(".zip") ||
      fileName.endsWith(".rar")
  ) {
    return <FileArchiveIcon className="size-4 opacity-60" />
  } else if (
    fileType.includes("excel") ||
      fileName.endsWith(".xls") ||
      fileName.endsWith(".xlsx")
  ) {
    return <FileSpreadsheetIcon className="size-4 opacity-60" />
  } else if (fileType.includes("video/")) {
    return <VideoIcon className="size-4 opacity-60" />
  } else if (fileType.includes("audio/")) {
    return <HeadphonesIcon className="size-4 opacity-60" />
  } else if (fileType.startsWith("image/")) {
    return <ImageIcon className="size-4 opacity-60" />
  }
  return <FileIcon className="size-4 opacity-60" />
}

export default function App() {
  const [
    { files, isDragging, errors },
    {
      handleDragEnter,
      handleDragLeave,
      handleDragOver,
      handleDrop,
      openFileDialog,
      removeFile,
      clearFiles,
      getInputProps
    }
  ] = useFileUpload({
    multiple: true
  })

  const handleUpload = () => {
    console.log("==> ", files[0].file)
  };


  return (
    <>
      {/* Drop area */}
      <div role="button"
	className="h-screen"
	onDragEnter={handleDragEnter}
	onDragLeave={handleDragLeave}
	onDragOver={handleDragOver}
	onDrop={handleDrop}
	data-dragging={isDragging || undefined}>
	<input {...getInputProps()} className="sr-only"/>

	{/* Visible drop area and file list */}
	<div className="flex flex-col items-center gap-2">

	  {/* Visible drop area */}
	  <div
	    data-dragging={isDragging || undefined}
	    className="border-input hover:bg-accent/50 data-[dragging=true]:bg-accent/50
	    has-[input:focus]:border-ring has-[input:focus]:ring-ring/50 flex min-h-40
	    flex-col items-center justify-center rounded-xl border border-dashed p-4
	    transition-colors has-disabled:pointer-events-none has-disabled:opacity-50
	    has-[input:focus]:ring-[3px] w-2/3 max-w-md"
	    onClick={openFileDialog}>
	    <div className="flex flex-col items-center justify-center text-center">
	      <div
		className="bg-background mb-2 flex size-11 shrink-0 items-center
		justify-center rounded-full border"
		aria-hidden="true">
		<FileUpIcon className="size-4 opacity-60" />
	      </div>
	      <p className="mb-1.5 text-sm font-medium">Upload files</p>
	      <p className="text-muted-foreground mb-2 text-xs">
		<span>Drag & drop or click to browse</span>
	      </p>
	    </div>
	  </div>

	  {/* Error handling */}
	  {errors.length > 0 && (
            <div
	      className="text-destructive flex items-center gap-1 text-xs"
	      role="alert">
	      <AlertCircleIcon className="size-3 shrink-0" />
	      <span>{errors[0]}</span>
	    </div>)}

	  {/* File list */}
	  {files.length > 0 && (
	    <div className="space-y-2 w-2/3 max-w-md">
	      {files.map((file) => (
		<div
		  key={file.id}
		  className="bg-background flex items-center justify-between gap-2
		  rounded-lg border p-2 pe-3">
		  <div className="flex items-center gap-3 overflow-hidden">
		    <div className="flex aspect-square size-10 shrink-0 items-center
		    justify-center rounded border">
		      {getFileIcon(file)}
		    </div>
		    <div className="flex min-w-0 flex-col gap-0.5">
		      <p className="truncate text-[13px] font-medium">
			{file.file instanceof File
			  ? file.file.name
			  : file.file.name}
		      </p>
		      <p className="text-muted-foreground text-xs">
			{formatBytes(
			  file.file instanceof File
			    ? file.file.size
			    : file.file.size
			)}
		      </p>
		    </div>
		  </div>

		  <Button
                    size="icon"
                    variant="ghost"
                    className="text-muted-foreground/80 hover:text-foreground -me-2
		    size-8 hover:bg-transparent"
                    onClick={() => removeFile(file.id)}
                    aria-label="Remove file">
                    <XIcon className="size-4" aria-hidden="true" />
		  </Button>
		</div>
	      ))}

	      <div className="flex justify-between">
		{/* Remove all files button */}
		<div>
		  <Button size="sm" variant="outline" onClick={clearFiles}>
		    <span>Alle löschen</span>
		  </Button>
		</div>

		{/* Share all files button */}
		<div>
		  <Button size="sm" onClick={handleUpload}>
		    <span>Teilen</span>
		  </Button>
		</div>
	      </div>
            </div>
	  )}
	</div>
      </div>
    </>
  )
}


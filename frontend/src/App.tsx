import './App.css'

import { useState } from 'react'
import { useFileUpload } from "./hooks/use-file-upload"
import { cn } from "@/lib/utils"
import { Send, XIcon } from "lucide-react"

import { Columns } from "./Columns.tsx"
import { DropZone, DropArea } from "./Drop.tsx"
import { mockFile, FileCard } from "./File.tsx"
import { Magnet } from "./Magnet.tsx"
import { Button } from "@/components/ui/button"
import * as Upload from "./Upload.ts"

export default function App() {
  // Globally toggle debugging.
  const debug = false

  // Handle all the state at the entrypoint into the website.
  const [uploadProgress, setUploadProgress] = useState<Upload.Progress[]>([])

  const [
    {
      files,
      isDragging,
      // errors  // TODO error handling
    },
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
    multiple: true,
    // TODO filter out all files that have file.type == "". They are
    // most likely directories which are not supported.
    // onFilesAdded: Upload.queueAndStart("/upload", setUploadProgress),
    onFilesAdded: Upload.queue(setUploadProgress),
  })

  return (
    <DropZone
      className="h-dvh"
      debug={debug} 
      onDragEnter={handleDragEnter}
      onDragOver={handleDragOver}
      onDragLeave={handleDragLeave}
      onDrop={handleDrop}>

      {/* This div positions the drop area */}
      <div className={cn(
	"absolute w-full",
	"top-[min(max(100vw,40rem)-40rem,max(100vh,40rem)-40rem,20vh)]",
	debug ? "border-2" : "",
      )}>

	<div className="flex items-center justify-center gap-8">
	  <div className="invisible">
	    <Button
	      onClick={clearFiles}
	      variant="outline"
	      size="icon"
	      className="size-11 rounded-full text-muted-foreground/80">
	      <XIcon className="size-5" strokeWidth={2} />
	    </Button>
	  </div>

	  <DropArea
	    className={cn(
	      "h-40 w-2/3 mb-6 max-w-md",
	    )}
	    debug={debug}
	    onClick={openFileDialog}
	    inputProps={getInputProps}
	    isDragging={isDragging} />

	  <div className={files.length == 0 ? "invisible" : ""}>
	    <Magnet>
	      <div
		className={cn(
		  "flex items-center justify-center",
		  "rounded-full size-11 shadow-none",
		  "text-blue-400 bg-blue-100 border-none",
		  "hover:text-blue-500 hover:bg-blue-300/80 hover:shadow-[0px_0px_15px_3px_rgba(0,0,0,0.1)]",
		  "transition ease-in-out",
		)}>
		<Send className="size-5" strokeWidth={files.length > 0 ? 3 : 2}/>
	      </div>
	    </Magnet>
	  </div>
	</div>


	<Columns className="">
	  {!debug
	    ? files.map((file) => {
	      return <FileCard
		       id={file.id}
		       className="w-sm"
		       removeFile={removeFile}
		       file={file.file}
		       preview={file.preview}
		       debug={debug}
		       key={file.id} />
	    })
	    // This creates a whole bunch of dummy files. Some of the interaction
	    // does not work here.
	    : Array.from({ length: 8 }, (_, idx) => idx).map((idx) => {
	      return <FileCard
		       removeFile={removeFile}
		       className="w-sm"
		       file={mockFile(idx)}
		       debug={debug}
		       key={idx} />
	    })
	  }
	</Columns>
      </div>
    </DropZone>
  )
}

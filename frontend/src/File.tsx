import * as React from "react"
import { formatBytes, type FileMetadata } from "./hooks/use-file-upload"
import { cn } from "@/lib/utils"

import {
  FileArchiveIcon,
  FileIcon,
  FileSpreadsheetIcon,
  FileTextIcon,
  HeadphonesIcon,
  ImageIcon,
  VideoIcon,
  XIcon,
} from "lucide-react"

// A helper function that returns a mock file.
export function mockFile(idx: number) {
  return new File([""], `${idx}.txt`)
}

interface FileIconOrPreviewInterface extends React.HTMLAttributes<HTMLDivElement> {
  file: File | FileMetadata | string
}

function FileIconOrPreview({ file, className }: FileIconOrPreviewInterface) {
  if (typeof file === 'string') {
    return (
      <img src={file} className="aspect-square"/>
    )
    
  } else {
    const fileType = file instanceof File ? file.type : file.type
    const fileName = file instanceof File ? file.name : file.name

    if (
      fileType.includes("pdf") ||
	fileName.endsWith(".pdf") ||
	fileType.includes("word") ||
	fileName.endsWith(".doc") ||
	fileName.endsWith(".docx")
    ) {
      return <FileTextIcon className={className} />
    } else if (
      fileType.includes("zip") ||
	fileType.includes("archive") ||
	fileName.endsWith(".zip") ||
	fileName.endsWith(".rar")
    ) {
      return <FileArchiveIcon className={className} />
    } else if (
      fileType.includes("excel") ||
	fileName.endsWith(".xls") ||
	fileName.endsWith(".xlsx")
    ) {
      return <FileSpreadsheetIcon className={className} />
    } else if (fileType.includes("video/")) {
      return <VideoIcon className={className} />
    } else if (fileType.includes("audio/")) {
      return <HeadphonesIcon className={className} />
    } else if (fileType.startsWith("image/")) {
      return <ImageIcon className={className} />
    }
    return <FileIcon className={className} />
  }
}

interface FileInterface extends React.HTMLAttributes<HTMLDivElement> {
  file: File | FileMetadata
  preview?: string
  debug?: boolean
  removeFile: (id: string) => void
  id?: string
}

// Don't forget to set a key when called from a map.
export function FileCard({ file, preview, className, debug, removeFile, id, ...props }: FileInterface) {
  const innerDebug = !debug ? false : false
  const [isHovered, setIsHovered] = React.useState(false);
  
  return (
    <div
      onMouseEnter={() => setIsHovered(true)}  
      onMouseLeave={() => setIsHovered(false)}
      className={cn(
	"border rounded-lg p-2",
	"break-inside-avoid",
	className,
	debug ? "border border-blue-400" : "",
      )}
      {...props}>
      <div className={cn(
	"flex items-center justify-between gap-3",
	innerDebug ? "border" : "",
      )}>
	<div className="flex items-center gap-3 overflow-hidden">
	  <div className="flex aspect-square size-10 items-center justify-center rounded border">
	    <FileIconOrPreview
	      file={file}
	      className="size-4 opacity-60" />
	  </div>
	  <div className={cn(
	    "flex min-w-0 flex-col gap-0.5",
	    innerDebug ? "border" : "",
	  )}>
	    <p className="truncate whitespace-nowrap text-[13px] font-medium">{file.name}</p>
	    <p className="text-muted-foreground text-xs">{formatBytes(file.size)}</p>
	  </div>
	</div>

	<button
	  onClick={id ? () => removeFile(id) : () => {}}
	  className={cn(
	    "text-muted-foreground/80 rounded-full",
	    isHovered ? "text-red-300 bg-red-100" : "",
	    "hover:bg-red-200 hover:text-red-400",
	    "transition ease-in-out",
	    innerDebug ? "border" : "",
	  )}>
	  <XIcon className="size-3 m-1" strokeWidth={4}/>
	</button>
      </div>
    </div>
  )
}

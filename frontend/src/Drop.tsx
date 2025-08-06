import * as React from "react"

import { cn } from "@/lib/utils"

import { FileUpIcon } from "lucide-react"

interface DropZoneInterface extends React.HTMLAttributes<HTMLDivElement> {
  children: React.ReactNode
  debug?: boolean
  isDragging?: boolean
}

// A component that defines a drop zone that expands to as much space as its
// parent allows. Add the class `h-screen' to its parent (or the component
// itself) to take all vertical space.
//
// Remember to hook the component to your state that you got from
// `useFileUpload': `onDragEnter', `onDragOver', `onDragLeave' and `onDrop'.
export function DropZone({ children, className, debug, isDragging, ...props }: DropZoneInterface) {
  return (
    <div
      className={cn(
	"data-[dragging=true]:bg-accent/50",
	className,
	debug ? "border-blue-500 border-4" : "",
      )}
      data-dragging={isDragging}
      {...props}>
      {children}
    </div>
  )
}

interface DropAreaInterface extends React.HTMLAttributes<HTMLDivElement> {
  debug?: boolean
  isDragging?: boolean
  onClick: () => void
  inputProps: () => React.InputHTMLAttributes<HTMLInputElement>
  children?: React.ReactNode
}

// A visible drop area. Make sure to hook it up to `useFileUpload'.
//
// You are required to pass the input props from `getInputProps' along
// to `inputProps'.
//
// This component should always be enclosed in a `DropZone'. Hence the
// DropZone has the same size as the DropArea or is larger.
export function DropArea({ children, className, debug, isDragging, onClick, inputProps, ...props }: DropAreaInterface) {
  return (
    <div
      className={cn(
	"grid",
	"rounded-xl shadow-[0px_0px_15px_3px_rgba(0,0,0,0.1)]",
	className,
	debug ? "border-blue-200 border-2" : "",
      )}
      data-dragging={isDragging}
      onClick={onClick}
      {...props}>
      <input {...inputProps()} className="sr-only" />
      <div
	className={cn(
	  "m-4",
	  isDragging ? "border-blue-200 bg-blue-100/25 border-2 border-dashed rounded-[9px]" : "",
	  "hover:border hover:border-dashed rounded-[9px] hover:bg-accent/50 cursor-pointer",
	  "transition ease-in-out",
	  "flex flex-col items-center justify-center",
	  debug ? "border-blue-200 border-2" : "",
	)}>
	<div className={cn(
	  "flex items-center justify-center size-11 border rounded-full mb-2",
	  isDragging ? "border-2 border-blue-300 text-blue-600 bg-blue-200/40" : "",
	  "transition ease-in-out",
	)}>
	  <FileUpIcon className="size-5 opacity-60" />
	</div>
	<p className="mb-1.5 text-sm font-medium">Upload files</p>
	<p className="text-muted-foreground mb-2 text-xs">Drag & Drop or click to browse</p>
      </div>
      {children}
    </div>
  )
}

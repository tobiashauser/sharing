import * as React from "react"

import { cn } from "@/lib/utils"

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
}

// A visible drop area. Make sure to hook it up to `useFileUpload'.
//
// You are required to pass the input props from `getInputProps' along
// to `inputProps'.
//
// This component should always be enclosed in a `DropZone'. Hence the
// DropZone has the same size as the DropArea or is larger.
export function DropArea({ className, debug, isDragging, inputProps, ...props }: DropAreaInterface) {
  return (
    <div
      className={cn(
	"hover:bg-accent/50 cursor-pointer",
	"border rounded-xl",
	isDragging ? "border-sky-100 bg-sky-50/50 shadow-xs" : "border-dashed border-input",
	"min-h-40 w-2/3 max-w-md",
	"transition-all",
	className,
	debug ? "border-blue-200 border-4" : "",
      )}
      data-dragging={isDragging}
      {...props}>
      <input {...inputProps} className="sr-only" />
    </div>
  )
}

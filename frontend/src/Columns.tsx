import * as React from "react"

import { cn } from "@/lib/utils"

interface ColumnsInterface extends React.HTMLAttributes<HTMLDivElement> {
  children: React.ReactNode
  debug?: boolean
}

export function Columns({ children, className, debug, ...props }: ColumnsInterface) {
  return (
    <div
      className={cn(
	"overflow-x-scroll",
	"[scrollbar-width:none] [&::-webkit-scrollbar]:hidden",
	"justify-center-safe gap-2",
	"grid md:grid-flow-col md:grid-rows-5 md:auto-rows-max",
	className,
	debug ? "border-2" : "",
      )}
      {...props}>
      {children}
    </div>
  )
}

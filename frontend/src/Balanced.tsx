import * as React from "react"

import { cn } from "@/lib/utils"

interface BalancedInterface extends React.HTMLAttributes<HTMLDivElement> {
  children: React.ReactNode
  debug?: boolean
}

// This currently hides all items to the left.
export function Balanced({ children, className, debug, ...props }: BalancedInterface) {
  const childrenArray = React.Children.toArray(children)
  const invisibleChildren = childrenArray.slice(1).map((child) => {
    return (
      <div className="invisible">
	{child}
      </div>
    )
  })
  const balancedChildren = [...invisibleChildren, ...childrenArray]
  
  return (
    <div
      className={cn(
	"flex items-center justify-center",
	className,
	debug ? "border-2" : "",
      )}
      {...props}>
      {balancedChildren}
    </div>
  )
}

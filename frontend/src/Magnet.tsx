import * as React from "react"
import { useRef, useState } from 'react'
import { motion } from 'framer-motion';

interface MagnetInterface extends React.HTMLAttributes<HTMLDivElement> {
  children: React.ReactNode
  disabled?: boolean
}

export function Magnet({ children, disabled }: MagnetInterface) {
  const ref = useRef<HTMLDivElement>(null);
  const [position, setPosition] = useState({ x: 0, y: 0 });

  const handleMouse: React.MouseEventHandler = (e) => {
    if (ref.current) {
      const { clientX, clientY } = e;
      const { height, width, left, top } = ref.current.getBoundingClientRect();
      const middleX = clientX - (left + width/2)
      const middleY = clientY - (top + height/2)
      setPosition({ x: middleX, y: middleY })
    }
  }

  const reset = () => {
    setPosition({ x: 0, y: 0 })
  }

  const { x, y } = position;
  
  return (
    <motion.div 
      style={{ position: "relative" }}
      ref={ref}
      onMouseMove={!disabled ? handleMouse : () => {}}
      onMouseLeave={!disabled ? reset : () => {}}
      animate={!disabled ? { x, y } : {}}
      transition={!disabled ? { type: "spring", stiffness: 150, damping: 15, mass: 0.1 } : {}}>
      {children}
    </motion.div>
  )
}

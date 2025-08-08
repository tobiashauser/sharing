// Finally functional typescript.
import { clsx, type ClassValue } from "clsx";
import { twMerge } from "tailwind-merge";

// The pseudo-selector shorthands don't work reliably...

// Applies SELECTOR to the list of CLASSES.
export function pseudo(selector: string, classes: string): string {
  return (
    " " +
    classes
      .split(/[ \t\v\f\r]+/)
      .map((cls) => cls.trim())
      .filter(Boolean)
      .map((cls) => `${selector}:${cls}`)
      .join(" ") +
    " "
  );
}

export function hover(classes: string): string {
  return pseudo("hover", classes);
}

export function dragging(classes: string): string {
  return pseudo("data-[dragging=true]", classes);
}

export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs));
}

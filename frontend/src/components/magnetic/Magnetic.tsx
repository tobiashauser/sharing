import gsap from "gsap";
import { JSX, onMount } from "solid-js";

interface MagnetInterface {
  children?: JSX.Element;
  disabled?: boolean;
}

export function Magnetic(props: MagnetInterface) {
  let ref!: HTMLDivElement;

  onMount(() => {
    const xTo = gsap.quickTo(ref, "x", {
      duration: 1,
      ease: "elastic.out(1, 0.6)",
    });
    const yTo = gsap.quickTo(ref, "y", {
      duration: 1,
      ease: "elastic.out(1, 0.6)",
    });

    const mouseMove = (e: MouseEvent) => {
      const { clientX, clientY } = e;
      const { height, width, left, top } = ref.getBoundingClientRect();
      const x = clientX - (left + width / 2);
      const y = clientY - (top + height / 2);
      xTo(x);
      yTo(y);
    };

    const mouseLeave = (_: MouseEvent) => {
      gsap.to(ref, { x: 0, duration: 0.3 });
      gsap.to(ref, { y: 0, duration: 0.3 });
      xTo(0);
      yTo(0);
    };

    ref.addEventListener("mousemove", mouseMove);
    ref.addEventListener("mouseleave", mouseLeave);

    return () => {
      ref.removeEventListener("mousemove", mouseMove);
      ref.removeEventListener("mouseleave", mouseLeave);
    };
  });

  return <div ref={ref}>{props.children}</div>;
}

export default Magnetic;

import { FaSolidQuestion } from "solid-icons/fa";

export function Info() {
  return (
    <div
      class="text-white py-1 px-2 rounded-sm bg-slate-700 hover:bg-slate-800 flex cursor-pointer items-center justify-center shadow-[0px_0px_5px_2px_rgba(0,0,0,0.1)]"
      onclick={() => console.log("show help")}
    >
      <FaSolidQuestion class="size-4" />
    </div>
  );
}

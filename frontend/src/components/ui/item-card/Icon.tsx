import { BsFileEarmarkSpreadsheet } from "solid-icons/bs";
import {
  FaSolidFile,
  FaSolidFileAudio,
  FaSolidFileImage,
  FaSolidFileInvoice,
  FaSolidFileLines,
  FaSolidFileVideo,
  FaSolidFileZipper,
  FaSolidFolder,
} from "solid-icons/fa";
import { Accessor } from "solid-js";
import { Item, UploadStatus } from "~/components/drop-zone";

interface IconAttributes {
  item: Item;
  status: Accessor<UploadStatus>;
}

export default function Icon({ item, status }: IconAttributes) {
  const classList = () => {
    return {
      "size-5": true,
      "text-slate-800": true,
      "text-neutral-300": status() === UploadStatus.ongoing,
      // "text-red-300": status() === UploadStatus.failed,
      "transition ease-in-out": true,
    };
  };

  if (item instanceof File) {
    const fileType = item.type;
    const fileName = item.name;

    if (
      fileType.includes("pdf") ||
      fileName.endsWith(".pdf") ||
      fileType.includes("word") ||
      fileName.endsWith(".doc") ||
      fileName.endsWith(".docx")
    ) {
      return <FaSolidFileInvoice classList={classList()} />;
    } else if (
      fileType.includes("txt") ||
      fileName.endsWith(".txt") ||
      fileName.endsWith(".md") ||
      fileName.endsWith(".markdown") ||
      fileName.endsWith(".org")
    ) {
      return <FaSolidFileLines classList={classList()} />;
    } else if (
      fileType.includes("zip") ||
      fileType.includes("archive") ||
      fileName.endsWith(".zip") ||
      fileName.endsWith(".rar")
    ) {
      return <FaSolidFileZipper classList={classList()} />;
    } else if (
      fileType.includes("excel") ||
      fileName.endsWith(".xls") ||
      fileName.endsWith(".xlsx")
    ) {
      return <BsFileEarmarkSpreadsheet />;
    } else if (fileType.includes("video/")) {
      return <FaSolidFileVideo classList={classList()} />;
    } else if (fileType.includes("audio/")) {
      return <FaSolidFileAudio classList={classList()} />;
    } else if (fileType.startsWith("image/")) {
      return <FaSolidFileImage classList={classList()} />;
    }
    return <FaSolidFile classList={classList()} />;
  } else {
    return <FaSolidFolder classList={classList()} />;
  }
}

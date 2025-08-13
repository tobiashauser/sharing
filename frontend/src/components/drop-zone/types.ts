// Unfortunately, I need to consolidate items that can either be a
// `FileSystemEntry' when dragged or a File when selected. Eventually
// everything needs to be a file so that I can send it to the backend.
// However, that means I have to manually keep track of (recursive)
// directories.

// I need to attach some more state to a file object.
export type Folder = { name: string; contents: Item[] };
export type Item = File | Folder;

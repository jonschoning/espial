export type BookmarkId = number;
export type TagId = number;

export type Bookmark = {
  url: string;
  title: string;
  description: string;
  tags: string;
  private: boolean;
  toread: boolean;
  bid: BookmarkId;
  slug: string;
  selected: boolean;
  time: string;
  archiveUrl: string | null;
};

export type NoteId = number;
export type NoteSlug = string;

export type Note = {
  id: NoteId;
  slug: NoteSlug;
  title: string;
  text: string;
  length: number;
  isMarkdown: boolean;
  shared: boolean;
  created: string;
  updated: string;
};

export type AccountSettings = {
  archiveDefault: boolean;
  privateDefault: boolean;
  privacyLock: boolean;
};

export type TagCloudMode = {
  mode: string;
  value: unknown;
  expanded: boolean;
};

export type TagCloud = Record<string, number>;

export type TagCloudModeF =
  | { kind: "top"; expanded: boolean; value: number }
  | { kind: "lowerBound"; expanded: boolean; value: number }
  | { kind: "related"; expanded: boolean; value: string[] }
  | { kind: "none" };

export function tagCloudModeToF(mode: TagCloudMode): TagCloudModeF {
  switch (mode.mode) {
    case "top": {
      const n = typeof mode.value === "number" ? mode.value : Number(mode.value);
      return Number.isFinite(n) ? { kind: "top", expanded: mode.expanded, value: n } : { kind: "none" };
    }
    case "lowerBound": {
      const n = typeof mode.value === "number" ? mode.value : Number(mode.value);
      return Number.isFinite(n) ? { kind: "lowerBound", expanded: mode.expanded, value: n } : { kind: "none" };
    }
    case "related": {
      const s = typeof mode.value === "string" ? mode.value : "";
      const tags = s ? s.split(" ") : [];
      return { kind: "related", expanded: mode.expanded, value: tags };
    }
    default:
      return { kind: "none" };
  }
}

export function tagCloudModeFromF(mode: TagCloudModeF): TagCloudMode {
  switch (mode.kind) {
    case "top":
      return { mode: "top", value: mode.value, expanded: mode.expanded };
    case "lowerBound":
      return { mode: "lowerBound", value: mode.value, expanded: mode.expanded };
    case "related":
      return { mode: "related", value: mode.value.join(" "), expanded: mode.expanded };
    case "none":
      return { mode: "none", value: "", expanded: false };
  }
}

export function isExpanded(mode: TagCloudModeF): boolean {
  return mode.kind === "none" ? false : mode.expanded;
}

export function isRelated(mode: TagCloudModeF): boolean {
  return mode.kind === "related";
}

export function setExpanded(mode: TagCloudModeF, expanded: boolean): TagCloudModeF {
  return mode.kind === "none" ? mode : { ...mode, expanded };
}

export function showMode(mode: TagCloudModeF): string {
  switch (mode.kind) {
    case "top":
      return "top";
    case "lowerBound":
      return "lowerBound";
    case "related":
      return "related";
    case "none":
      return "";
  }
}


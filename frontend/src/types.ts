/** User's preferred color scheme. */
export type ColorSchemePreference = 'light' | 'dark';

/** Unique identifier for a bookmark. */
export type BookmarkId = number;

export type Bookmark = {
  /** The bookmarked URL. */
  url: string;
  /** Display title of the bookmark. */
  title: string;
  /** Optional user-provided description. */
  description: string;
  /** Space-separated list of tags. */
  tags: string;
  /** Whether the bookmark is private (not visible to others). */
  private: boolean;
  /** Whether the bookmark is marked for later reading. */
  toread: boolean;
  /** Whether the bookmark should be archived (request). */
  archiveRequested?: boolean | null;
  /** Database ID of the bookmark. */
  bid: BookmarkId;
  /** URL-friendly slug for the bookmark. */
  slug: string;
  /** Whether the bookmark is starred. */
  selected: boolean;
  /** ISO 8601 creation timestamp. */
  time: string;
  /** URL of the archived copy, or null if not archived. */
  archiveUrl: string | null;
};

/** Unique identifier for a note. */
export type NoteId = number;
/** URL-friendly slug for a note. */
export type NoteSlug = string;

export type Note = {
  /** Database ID of the note. */
  id: NoteId;
  /** URL-friendly slug for the note. */
  slug: NoteSlug;
  /** Display title of the note. */
  title: string;
  /** Raw text content of the note. */
  text: string;
  /** Character length of the note text. */
  length: number;
  /** Whether the note body should be rendered as Markdown. */
  isMarkdown: boolean;
  /** Whether the note is publicly visible. */
  shared: boolean;
  /** ISO 8601 creation timestamp. */
  created: string;
  /** ISO 8601 last-updated timestamp. */
  updated: string;
};

export type Filter =
  | { tag: 'FilterAll' }
  | { tag: 'FilterUnread' }
  | { tag: 'FilterUntagged' }
  | { tag: 'FilterStarred' }
  | { tag: 'FilterSingle'; contents: string };

export type AccountSettings = {
  /** Whether bookmarks are archived by default. */
  archiveDefault: boolean;
  /** Whether new bookmarks are private by default. */
  privateDefault: boolean;
  /** Whether tag suggestions are enabled. */
  suggestTags: boolean;
  /** Whether the account is locked to private-only mode. */
  privacyLock: boolean;
  /** Whether the archive backend is enabled. */
  archiveBackendEnabled: boolean;
};

/** Raw tag cloud mode as stored/sent by the server. */
export type TagCloudMode = {
  /** The mode name (e.g. 'top', 'lowerBound', 'related', 'none'). */
  mode: string;
  /** Mode-specific parameter value. */
  value: unknown;
  /** Whether the tag cloud panel is expanded. */
  expanded: boolean;
};

/** Map of tag name to usage count. */
export type TagCloud = Record<string, number>;

/** A single tag suggestion with its usage count. */
export type TSuggestion = {
  term: string;
  count: number;
};

/** Request payload for fetching tag suggestions based on a query and current tags. */
export type TagSuggestionRequest = {
  query: string;
  currentTags: string[];
};

/** Response payload containing a list of tag suggestions. */
export type TagSuggestionResponse = {
  suggestions: TSuggestion[];
};

/** Discriminated union representing the tag cloud filter mode. */
export type TagCloudModeF =
  | { kind: 'top'; expanded: boolean; value: number }
  | { kind: 'lowerBound'; expanded: boolean; value: number }
  | { kind: 'related'; expanded: boolean; value: string[] }
  | { kind: 'none' };

export function tagCloudModeToF(mode: TagCloudMode): TagCloudModeF {
  switch (mode.mode) {
    case 'top': {
      const n = typeof mode.value === 'number' ? mode.value : Number(mode.value);
      return Number.isFinite(n)
        ? { kind: 'top', expanded: mode.expanded, value: n }
        : { kind: 'none' };
    }
    case 'lowerBound': {
      const n = typeof mode.value === 'number' ? mode.value : Number(mode.value);
      return Number.isFinite(n)
        ? { kind: 'lowerBound', expanded: mode.expanded, value: n }
        : { kind: 'none' };
    }
    case 'related': {
      const s = typeof mode.value === 'string' ? mode.value : '';
      const tags = s ? s.split(' ') : [];
      return { kind: 'related', expanded: mode.expanded, value: tags };
    }
    default:
      return { kind: 'none' };
  }
}

export function tagCloudModeFromF(mode: TagCloudModeF): TagCloudMode {
  switch (mode.kind) {
    case 'top':
      return { mode: 'top', value: mode.value, expanded: mode.expanded };
    case 'lowerBound':
      return { mode: 'lowerBound', value: mode.value, expanded: mode.expanded };
    case 'related':
      return { mode: 'related', value: mode.value.join(' '), expanded: mode.expanded };
    case 'none':
      return { mode: 'none', value: '', expanded: false };
  }
}

export function isExpanded(mode: TagCloudModeF): boolean {
  return mode.kind === 'none' ? false : mode.expanded;
}

export function isRelated(mode: TagCloudModeF): boolean {
  return mode.kind === 'related';
}

export function setExpanded(mode: TagCloudModeF, expanded: boolean): TagCloudModeF {
  return mode.kind === 'none' ? mode : { ...mode, expanded };
}

export function showMode(mode: TagCloudModeF): string {
  switch (mode.kind) {
    case 'top':
      return 'top';
    case 'lowerBound':
      return 'lowerBound';
    case 'related':
      return 'related';
    case 'none':
      return '';
  }
}

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
  /** Whether the Return key applies a tag suggestion (Tab always does). */
  suggestTagsUseReturnKey: boolean;
  /** Whether the account is locked to private-only mode. */
  privacyLock: boolean;
  /** Whether the tag cloud is visible to non-authenticated visitors. */
  publicTagCloud: boolean;
  /** Whether note listings show a text preview, or just the title. */
  previewNotes: boolean;
  /** Whether the archive backend is enabled. */
  archiveBackendEnabled: boolean;
  /** Whether the user currently has an API key set. */
  hasApiKey: boolean;
  /** The user's preferred language (null means server default). */
  language: string | null;
};

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

export type BulkAction = 'read' | 'unread' | 'star' | 'unstar' | 'delete' | 'private' | 'public';
export type SharedP = 'all' | 'public' | 'private';

type BulkEditRequestBase = {
  action: BulkAction | null;
  addTags: string;
  removeTags: string;
  selectionCount: number;
};

export type BulkEditRequest =
  | (BulkEditRequestBase & { selection: 'page'; bids: number[] })
  | (BulkEditRequestBase & {
      selection: 'all';
      filter: Filter;
      sharedp: SharedP;
      tags: string[];
      query: string | null;
    });

export type BulkEditResponse = {
  ok: boolean;
  status: number;
  bodyText?: string;
  data?: { editedCount: number };
};

export type NoteBulkAction = 'private' | 'public' | 'markdown' | 'plaintext' | 'delete';

type NoteBulkEditRequestBase = {
  action: NoteBulkAction;
  selectionCount: number;
};

export type NoteBulkEditRequest =
  | (NoteBulkEditRequestBase & { selection: 'page'; nids: number[] })
  | (NoteBulkEditRequestBase & { selection: 'all'; sharedp: SharedP; query: string | null });

/** Raw tag cloud mode as stored/sent by the server. */
export type TagCloudMode = {
  /** The mode name (e.g. 'top', 'lowerBound', 'related', 'none'). */
  mode: string;
  /** Mode-specific parameter value. */
  value: unknown;
  /** Whether the tag cloud panel is expanded. */
  expanded: boolean;
  /** Minimum bookmark count for related tags (only used in 'related' mode). */
  lowerBound?: number;
};

/** Map of tag name to usage count. */
export type TagCloud = Record<string, number>;

/** Discriminated union representing the tag cloud filter mode. */
export type TagCloudModeF =
  | { kind: 'top'; expanded: boolean }
  | { kind: 'lowerBound'; expanded: boolean; value: number }
  | { kind: 'related'; expanded: boolean; value: string[] }
  | { kind: 'relatedLowerBound'; expanded: boolean; value: string[]; lowerBound: number }
  | { kind: 'none' };

export type TagCloudModeBrowse = Extract<TagCloudModeF, { kind: 'top' | 'lowerBound' }>;
export type TagCloudModeRelated = Extract<TagCloudModeF, { kind: 'related' | 'relatedLowerBound' }>;

export function tagCloudModeToF(mode: TagCloudMode): TagCloudModeF {
  switch (mode.mode) {
    case 'top':
      return { kind: 'top', expanded: mode.expanded };
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
    case 'relatedLowerBound': {
      const s = typeof mode.value === 'string' ? mode.value : '';
      const tags = s ? s.split(' ') : [];
      const lowerBound =
        typeof mode.lowerBound === 'number' && mode.lowerBound > 0 ? mode.lowerBound : 1;
      return { kind: 'relatedLowerBound', expanded: mode.expanded, value: tags, lowerBound };
    }
    default:
      return { kind: 'none' };
  }
}

export function tagCloudModeFromF(mode: TagCloudModeF): TagCloudMode {
  switch (mode.kind) {
    case 'top':
      return { mode: 'top', value: null, expanded: mode.expanded };
    case 'lowerBound':
      return { mode: 'lowerBound', value: mode.value, expanded: mode.expanded };
    case 'related':
      return { mode: 'related', value: mode.value.join(' '), expanded: mode.expanded };
    case 'relatedLowerBound':
      return {
        mode: 'relatedLowerBound',
        value: mode.value.join(' '),
        expanded: mode.expanded,
        lowerBound: mode.lowerBound,
      };
    case 'none':
      return { mode: 'none', value: '', expanded: false };
  }
}

export function isExpanded(mode: TagCloudModeF): boolean {
  return mode.kind === 'none' ? false : mode.expanded;
}

export function setExpanded(mode: TagCloudModeF, expanded: boolean): TagCloudModeF {
  return mode.kind === 'none' ? mode : { ...mode, expanded };
}

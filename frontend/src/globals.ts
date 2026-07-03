import type { Bookmark, Filter, SharedP } from './types';

/** Server-rendered page data. */
export type AppData = {
  /** The list of bookmarks for the current page. */
  bmarks: Bookmark[];
  /** A single bookmark, used when viewing or editing one entry. */
  bmark: Bookmark;
  /** Whether the current user owns the displayed profile. */
  isowner: boolean;
  /** Whether tag suggestions are enabled. */
  suggestTags?: boolean;
  /** Whether note listings show a text preview, or just the title. */
  previewNotes?: boolean;
  /** Whether the archive backend is enabled. */
  archiveBackendEnabled?: boolean;
  /** The current filter applied to the bookmarks. */
  filter?: Filter;
  /** The current shared/private filter. */
  sharedp?: SharedP;
  /** The path tags currently filtering bookmarks. */
  tags?: string[];
  /** The current search query string, or null if none. */
  query?: string | null;
};

/** Server-rendered global data. */
export type App = {
  /** The HTTP header name used to send the CSRF token. */
  csrfHeaderName: string;
  /** The cookie name that holds the CSRF token. */
  csrfCookieName: string;
  /** The form parameter name used to submit the CSRF token. */
  csrfParamName: string;
  /** The current CSRF token value. */
  csrfToken: string;
  /** URL for the home route. */
  homeR: string;
  /** URL for the logout route. */
  authRlogoutR: string;
  /** URL for the current user's profile, or null if not logged in. */
  userR: string | null;
  /** Endpoint URL for fetching tag cloud data. */
  tagCloudR?: string;
  /** URL for the current note, or null if not viewing a note. */
  noteR: string | null;
  /** URL for the i18n backend */
  i18nR: string;
  /** The current language code (e.g. 'en', 'fr'). */
  lang: string;
  /** Server-rendered page data. */
  dat: AppData;
};

declare global {
  // Provided by the server in a <script> tag.

  var app: App;
}

export function app(): App {
  return globalThis.app;
}

export function fromNow(locale = navigator.language, date: Date | number | string): string {
  const target = date instanceof Date ? date.getTime() : new Date(date).getTime();

  const diff = target - Date.now();
  const abs = Math.abs(diff);

  const units: readonly [Intl.RelativeTimeFormatUnit, number][] = [
    ['year', 365 * 24 * 60 * 60 * 1000],
    ['month', 30 * 24 * 60 * 60 * 1000],
    ['day', 24 * 60 * 60 * 1000],
    ['hour', 60 * 60 * 1000],
    ['minute', 60 * 1000],
    ['second', 1000],
  ];

  const rtf = new Intl.RelativeTimeFormat(locale, { numeric: 'auto' });

  for (const [unit, ms] of units) {
    if (abs >= ms || unit === 'second') {
      return rtf.format(Math.round(diff / ms), unit);
    }
  }

  // Unreachable, but satisfies TypeScript.
  return '';
}

export function shdatetime(lang: string, dateString: string): string {
  return new Date(dateString).toLocaleString(lang, {
    dateStyle: 'full',
    timeStyle: 'long',
  });
}

export function toLocaleDateString(lang: string, dateString: string): string {
  return new Date(dateString).toLocaleDateString(lang, {
    year: 'numeric',
    month: 'short',
    day: 'numeric',
  });
}

export function closeWindow(win: Window): void {
  win.close();
}

export type RawHTML = string;

export function setFocus(elemId: string): void {
  document.getElementById(elemId)?.focus();
}

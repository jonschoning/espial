import moment from 'moment';

import type { Bookmark } from './types';

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
  /** URL for the current note, or null if not viewing a note. */
  noteR: string | null;
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

export function moment8601(s: string): [string, string] {
  const m = moment(s, moment.ISO_8601);
  const s1 = m.fromNow();
  const s2 = `${m.format('MMMM D YYYY, h:mm a')} (${m.format()}) `;
  return [s1, s2];
}

export function mmoment8601(s: string): [string, string] | null {
  try {
    return moment8601(s);
  } catch {
    return null;
  }
}

export function closeWindow(win: Window): void {
  win.close();
}

export type RawHTML = string;

export function setFocus(elemId: string): void {
  document.getElementById(elemId)?.focus();
}

export function toLocaleDateString(dateString: string): string {
  return new Date(dateString).toLocaleDateString(undefined, {
    year: 'numeric',
    month: 'short',
    day: 'numeric',
  });
}

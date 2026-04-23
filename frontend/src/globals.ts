import moment from "moment";
import type { Bookmark } from "./types";

export type AppData = {
  bmarks: Bookmark[];
  bmark: Bookmark;
  isowner: boolean;
  // notes, note, accountSettings, tagCloudMode are present on some pages, but not always.
  // We keep this permissive to avoid breaking server-provided shapes.
  [k: string]: unknown;
};

export type App = {
  csrfHeaderName: string;
  csrfCookieName: string;
  csrfParamName: string;
  csrfToken: string;
  homeR: string;
  authRlogoutR: string;
  userR: string | null;
  noteR: string | null;
  dat: AppData;
  [k: string]: unknown;
};

declare global {
  // Provided by the server in a <script> tag.
  // eslint-disable-next-line no-var
  var app: App;
}

export function app(): App {
  return globalThis.app;
}

export function closest(selector: string, node: Element): Element | null {
  return node.closest(selector);
}

export function createFormData(formElement: HTMLFormElement): FormData {
  return new FormData(formElement);
}

export function createFormString(formElement: HTMLFormElement): string {
  return new URLSearchParams(new FormData(formElement) as any).toString();
}

export function createFormArray(formElement: HTMLFormElement): Array<[string, FormDataEntryValue]> {
  return Array.from(new FormData(formElement).entries());
}

export function moment8601(s: string): [string, string] {
  const m = moment(s, moment.ISO_8601);
  const s1 = m.fromNow();
  const s2 = `${m.format("MMMM D YYYY, h:mm a")} (${m.format()}) `;
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
  // Keep the output human-readable and locale-aware (similar intent to PS version),
  // but avoid relying on `dateStyle` typings for older lib targets.
  return new Date(dateString).toLocaleDateString(undefined, { year: "numeric", month: "short", day: "numeric" });
}


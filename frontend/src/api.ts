import ky from 'ky';

import { app } from './globals';
import type {
  AccountSettings,
  Bookmark,
  BulkEditRequest,
  BulkEditResponse,
  Note,
  NoteBulkEditRequest,
  TagCloud,
  TagCloudMode,
  TagSuggestionRequest,
  TagSuggestionResponse,
} from './types';

export type StarAction = 'star' | 'unstar';

function withCsrf(headers?: HeadersInit): HeadersInit {
  const a = app();
  const h = new Headers(headers);
  h.set(a.csrfHeaderName, a.csrfToken);
  return h;
}

// Paths below are relative to the document's `<base href>` (set by the server to the
// approot), so they resolve correctly regardless of the current page's URL depth or
// whether the app is served from a subpath.
async function request(
  method: string,
  path: string,
  opts: { headers?: HeadersInit; body?: BodyInit | null; timeout?: number | false } = {},
) {
  const res = await ky(path, {
    method,
    headers: withCsrf(opts.headers),
    body: opts.body ?? undefined,
    throwHttpErrors: false,
    timeout: opts.timeout,
  });
  return res;
}

export async function toggleStar(
  bid: number,
  action: StarAction,
): Promise<{ ok: boolean; status: number; bodyText: string }> {
  const res = await request('PATCH', `api/bm/${bid.toString()}/${action}`, {
    headers: { 'content-type': 'application/x-www-form-urlencoded' },
  });
  return { ok: res.ok, status: res.status, bodyText: await res.text() };
}

export async function markRead(
  bid: number,
): Promise<{ ok: boolean; status: number; bodyText: string }> {
  const res = await request('PATCH', `api/bm/${bid.toString()}/read`, {
    headers: { 'content-type': 'application/x-www-form-urlencoded' },
  });
  return { ok: res.ok, status: res.status, bodyText: await res.text() };
}

export async function destroy(
  bid: number,
): Promise<{ ok: boolean; status: number; bodyText: string }> {
  const res = await request('DELETE', `api/bm/${bid.toString()}`, {
    headers: { 'content-type': 'application/x-www-form-urlencoded' },
  });
  return { ok: res.ok, status: res.status, bodyText: await res.text() };
}

export async function bulkEdit(req: BulkEditRequest): Promise<BulkEditResponse> {
  return bulkEditPost('api/bm/bulkEdit', req);
}

export async function noteBulkEdit(req: NoteBulkEditRequest): Promise<BulkEditResponse> {
  return bulkEditPost('api/note/bulkEdit', req);
}

async function bulkEditPost(
  path: string,
  req: BulkEditRequest | NoteBulkEditRequest,
): Promise<BulkEditResponse> {
  const res = await request('POST', path, {
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify(req),
    timeout: 300_000,
  });
  const contentType = res.headers.get('content-type') ?? '';
  let bodyText: string | undefined;
  let data: { editedCount: number } | undefined;
  if (contentType.includes('application/json')) {
    try {
      data = await res.json();
    } catch {
      data = undefined;
    }
  } else {
    bodyText = await res.text();
  }
  return { ok: res.ok, status: res.status, bodyText, data };
}

export async function archiveBookmark(
  bid: number,
): Promise<{ ok: boolean; status: number; bodyText: string }> {
  const res = await request('POST', `api/bm/${bid.toString()}/archive`, {
    headers: { 'content-type': 'application/x-www-form-urlencoded' },
  });
  return { ok: res.ok, status: res.status, bodyText: await res.text() };
}

export async function editBookmark(
  bm: Bookmark,
): Promise<{ ok: boolean; status: number; bodyText: string }> {
  const res = await request('POST', 'api/add', {
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify(bm),
  });
  return { ok: res.ok, status: res.status, bodyText: await res.text() };
}

export async function editNote(
  note: Note,
): Promise<{ ok: boolean; status: number; bodyText: string }> {
  const res = await request('POST', 'api/note/add', {
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify(note),
  });
  return { ok: res.ok, status: res.status, bodyText: await res.text() };
}

export async function lookupTitle(bm: Bookmark): Promise<string | null> {
  const res = await request('POST', 'api/lookuptitle', {
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify(bm),
  });
  if (res.status === 200) return await res.text();
  return null;
}

export async function getTagCloud(mode: TagCloudMode): Promise<TagCloud | null> {
  const tagCloudR = app().tagCloudR;
  if (!tagCloudR) return null;
  const res = await request('POST', tagCloudR, {
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify(mode),
  });
  if (!res.ok) return null;
  try {
    return await res.json();
  } catch {
    return null;
  }
}

export async function updateTagCloudMode(mode: TagCloudMode): Promise<void> {
  await request('POST', 'api/tagcloudmode', {
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify(mode),
  });
}

export async function fetchTagSuggestions(
  data: TagSuggestionRequest,
): Promise<TagSuggestionResponse | null> {
  const res = await request('POST', 'api/tagSuggestions', {
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify(data),
  });
  if (!res.ok) return null;
  try {
    return await res.json<TagSuggestionResponse>();
  } catch {
    return null;
  }
}

export async function destroyNote(
  nid: number,
): Promise<{ ok: boolean; status: number; bodyText: string }> {
  const res = await request('DELETE', `api/note/${nid.toString()}`, {
    headers: { 'content-type': 'application/x-www-form-urlencoded' },
  });
  return { ok: res.ok, status: res.status, bodyText: await res.text() };
}

export async function editAccountSettings(
  us: AccountSettings,
): Promise<{ ok: boolean; status: number; bodyText: string }> {
  const res = await request('POST', 'api/accountSettings', {
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify(us),
  });
  return { ok: res.ok, status: res.status, bodyText: await res.text() };
}

export async function resetApiKey(): Promise<{
  ok: boolean;
  status: number;
  apiKey?: string;
  bodyText?: string;
}> {
  const res = await request('POST', 'Settings/apikey');
  if (res.ok) {
    try {
      const data = await res.json<{ apiKey: string }>();
      return { ok: true, status: res.status, apiKey: data.apiKey };
    } catch {
      return { ok: true, status: res.status };
    }
  }
  return { ok: false, status: res.status, bodyText: await res.text() };
}

export async function revokeApiKey(): Promise<{ ok: boolean; status: number; bodyText: string }> {
  const res = await request('DELETE', 'Settings/apikey');
  return { ok: res.ok, status: res.status, bodyText: await res.text() };
}

// Paths relative to the document's `<base href>` (the approot). Exports are plain
// authenticated GET downloads; anchors point at these directly.
export const exportPaths = {
  bookmarksJson: 'Settings/export/bookmarks/json',
  bookmarksHtml: 'Settings/export/bookmarks/netscape',
  notesJson: 'Settings/export/notes/json',
} as const;

export type ImportResult = {
  ok: boolean;
  status: number;
  imported?: number;
  bodyText?: string;
};

async function importFile(path: string, contentType: string, body: string): Promise<ImportResult> {
  const res = await request('POST', path, {
    headers: { 'content-type': contentType },
    body,
    timeout: 300_000,
  });
  if (res.ok) {
    try {
      const data = await res.json<{ imported: number }>();
      return { ok: true, status: res.status, imported: data.imported };
    } catch {
      return { ok: true, status: res.status };
    }
  }
  return { ok: false, status: res.status, bodyText: await res.text() };
}

export const importBookmarks = (text: string): Promise<ImportResult> =>
  importFile('Settings/import/bookmarks/json', 'application/json', text);

export const importFirefox = (text: string): Promise<ImportResult> =>
  importFile('Settings/import/bookmarks/firefox', 'application/json', text);

export const importNetscape = (text: string): Promise<ImportResult> =>
  importFile('Settings/import/bookmarks/netscape', 'text/plain', text);

export const importNotes = (jsonArray: string): Promise<ImportResult> =>
  importFile('Settings/import/notes/json', 'application/json', jsonArray);

export async function logout(): Promise<void> {
  const a = app();
  await request('POST', a.authRlogoutR);
  window.location.reload();
}

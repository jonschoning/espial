import ky from 'ky';

import { app } from './globals';
import type {
  AccountSettings,
  Bookmark,
  BulkEditRequest,
  BulkEditResponse,
  Note,
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

function urlFor(path: string): string {
  // Preserve PureScript semantics: simple string concatenation.
  return `${app().homeR}${path}`;
}

function toRelativePath(urlOrPath: string): string {
  try {
    const u = new URL(urlOrPath, window.location.origin);
    const rel = `${u.pathname}${u.search}${u.hash}`;
    return rel.startsWith('/') ? rel.slice(1) : rel;
  } catch {
    return urlOrPath.replace(/^\/+/, '');
  }
}

async function request(
  method: string,
  path: string,
  opts: { headers?: HeadersInit; body?: BodyInit | null; timeout?: number | false } = {},
) {
  const res = await ky(urlFor(path), {
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
  const res = await request('POST', 'api/bm/bulk', {
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
  const res = await request('POST', 'api/tagcloud', {
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

export async function logout(): Promise<void> {
  const a = app();
  await request('POST', toRelativePath(a.authRlogoutR));
  window.location.reload();
}

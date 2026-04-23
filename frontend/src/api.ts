import ky from "ky";
import type { AccountSettings, Bookmark, Note, TagCloud, TagCloudMode } from "./types";
import { app } from "./globals";

export type StarAction = "star" | "unstar";

function withCsrf(headers?: HeadersInit): HeadersInit {
  const a = app();
  return {
    [a.csrfHeaderName]: a.csrfToken,
    ...(headers ?? {}),
  };
}

function urlFor(path: string): string {
  // Preserve PureScript semantics: simple string concatenation.
  return `${app().homeR}${path}`;
}

function toRelativePath(urlOrPath: string): string {
  try {
    const u = new URL(urlOrPath, window.location.origin);
    const rel = `${u.pathname}${u.search}${u.hash}`;
    return rel.startsWith("/") ? rel.slice(1) : rel;
  } catch {
    return urlOrPath.replace(/^\/+/, "");
  }
}

async function request(method: string, path: string, opts: { headers?: HeadersInit; body?: BodyInit | null } = {}) {
  const res = await ky(urlFor(path), {
    method,
    headers: withCsrf(opts.headers),
    body: opts.body ?? undefined,
    throwHttpErrors: false,
  });
  return res;
}

export async function toggleStar(bid: number, action: StarAction): Promise<void> {
  await request("POST", `bm/${bid}/${action}`, {
    headers: { "content-type": "application/x-www-form-urlencoded" },
  });
}

export async function destroy(bid: number): Promise<void> {
  await request("DELETE", `bm/${bid}`, {
    headers: { "content-type": "application/x-www-form-urlencoded" },
  });
}

export async function markRead(bid: number): Promise<void> {
  await request("POST", `bm/${bid}/read`, {
    headers: { "content-type": "application/x-www-form-urlencoded" },
  });
}

export async function editBookmark(bm: Bookmark): Promise<{ ok: boolean; status: number; bodyText: string }> {
  const res = await request("POST", "api/add", {
    headers: { "content-type": "application/json" },
    body: JSON.stringify(bm),
  });
  return { ok: res.ok, status: res.status, bodyText: await res.text() };
}

export async function editNote(note: Note): Promise<{ ok: boolean; status: number; bodyText: string }> {
  const res = await request("POST", "api/note/add", {
    headers: { "content-type": "application/json" },
    body: JSON.stringify(note),
  });
  return { ok: res.ok, status: res.status, bodyText: await res.text() };
}

export async function lookupTitle(bm: Bookmark): Promise<string | null> {
  const res = await request("POST", "api/lookuptitle", {
    headers: { "content-type": "application/json" },
    body: JSON.stringify(bm),
  });
  if (res.status === 200) return await res.text();
  return null;
}

export async function getTagCloud(mode: TagCloudMode): Promise<TagCloud | null> {
  const res = await request("POST", "api/tagcloud", {
    headers: { "content-type": "application/json" },
    body: JSON.stringify(mode),
  });
  if (!res.ok) return null;
  try {
    return (await res.json()) as TagCloud;
  } catch {
    return null;
  }
}

export async function updateTagCloudMode(mode: TagCloudMode): Promise<void> {
  await request("POST", "api/tagcloudmode", {
    headers: { "content-type": "application/json" },
    body: JSON.stringify(mode),
  });
}

export async function destroyNote(nid: number): Promise<void> {
  await request("DELETE", `api/note/${nid}`, {
    headers: { "content-type": "application/x-www-form-urlencoded" },
  });
}

export async function editAccountSettings(us: AccountSettings): Promise<void> {
  await request("POST", "api/accountSettings", {
    headers: { "content-type": "application/json" },
    body: JSON.stringify(us),
  });
}

export async function logout(): Promise<void> {
  const a = app();
  await request("POST", toRelativePath(a.authRlogoutR));
  window.location.reload();
}


import type { App } from './globals';
import { curQuerystring, encodeTag, fromNullableStr } from './util';

const PRESERVED_PARAMS = ['query', 'sort', 'order'];

/**
 * Builds a URL to the bookmark listing for the given tags, keeping the
 * current filter/shared axis (mirrors pageRouteFor in Handler/User.hs) and
 * the query/sort/order params from the current URL. Paging cursor params
 * are intentionally dropped, resetting paging.
 */
export function buildTagUrl(a: App, tags: string[]): string {
  const base = fromNullableStr(a.userR);
  const tagSegment = tags.length ? `/t:${tags.map(encodeTag).join('+')}` : '';
  return `${base}${axisSegment(a)}${tagSegment}${preservedQueryString()}`;
}

function axisSegment(a: App): string {
  if (a.dat.sharedp === 'private') return '/private';
  if (a.dat.sharedp === 'public') return '/public';
  switch (a.dat.filter?.tag) {
    case 'FilterUnread':
      return '/unread';
    case 'FilterUntagged':
      return '/untagged';
    case 'FilterStarred':
      return '/starred';
    default:
      return '';
  }
}

function preservedQueryString(): string {
  const qs = curQuerystring().filter(([k]) => PRESERVED_PARAMS.includes(k));
  if (qs.length === 0) return '';
  return (
    '?' + qs.map(([k, v]) => `${encodeURIComponent(k)}=${encodeURIComponent(v ?? '')}`).join('&')
  );
}

export type QueryStringArray = Array<[string, string | null]>;

function unsafeDecode(str: string): string {
  return decodeURIComponent(str);
}

export function parseQueryString(searchOrHash: string): QueryStringArray {
  const first = searchOrHash.slice(0, 1);
  const qs = first === "#" || first === "?" ? searchOrHash.slice(1) : searchOrHash;
  const parts = qs.split("&").filter((x) => x !== "");

  const decode = (s: string) => unsafeDecode(s.replace(/\+/g, " "));

  const out: QueryStringArray = [];
  for (const kv of parts) {
    const [k, ...rest] = kv.split("=");
    if (!k) continue;
    if (rest.length === 0) out.push([decode(k), null]);
    else if (rest.length === 1) out.push([decode(k), decode(rest[0] ?? "")]);
  }
  return out;
}

export function curQuerystring(): QueryStringArray {
  return parseQueryString(window.location.search);
}

export function lookupQueryStringValue(qs: QueryStringArray, k: string): string | null {
  for (const [key, val] of qs) {
    if (key === k) return val;
  }
  return null;
}

export function encodeTag(tag: string): string {
  return encodeURIComponent(tag.replace(/\+/g, "%2B")) ?? "";
}

export function fromNullableStr(s: string | null | undefined): string {
  return s ?? "";
}

export function whenA<T>(b: boolean, k: () => T[]): T[] {
  return b ? k() : [];
}


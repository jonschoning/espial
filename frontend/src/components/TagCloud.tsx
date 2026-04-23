import React from "react";
import { getTagCloud, updateTagCloudMode } from "../api";
import { app } from "../globals";
import type { TagCloud as TagCloudT, TagCloudModeF } from "../types";
import {
  isExpanded,
  isRelated,
  setExpanded,
  tagCloudModeFromF,
  type TagCloudMode,
} from "../types";
import { encodeTag, fromNullableStr } from "../util";
import { useTagCloudStore } from "../stores/tagCloudStore";

function rescale(f: (x: number) => number, v: number, n: number, m: number, l: number, h: number): number {
  const denom = f(m - n);
  const ratio = m - n < 0.01 || denom === 0 ? 1.0 : f(v - n) / denom;
  return ratio * (h - l) + l;
}

function toArray(curtags: string[], n: number, m: number, tagcloud: TagCloudT) {
  const a = app();
  const cur = curtags.map((t) => t.toLowerCase());
  const entries: Array<[string, number]> = Object.entries(tagcloud).sort((x, y) =>
    x[0].toLowerCase().localeCompare(y[0].toLowerCase()),
  ) as Array<[string, number]>;

  const linkToFilterTag = (rest: string) => `${fromNullableStr(a.userR)}${rest === "" ? "" : `/t:${rest}`}`;

  const out: Array<React.ReactNode> = [];
  for (const [k, v] of entries) {
    const kLower = k.toLowerCase();
    const fontsize = rescale((x) => x, v, n, m, 100.0, 150.0);
    const opacity = rescale((x) => Math.log(1.0 + x), v, n, m, 0.6, 1.0);
    const style: React.CSSProperties = { fontSize: `${fontsize}%`, opacity };

    const includeExclude =
      cur.length === 0 ? null : !cur.includes(kLower) ? (
        <a
          href={linkToFilterTag(cur.concat([kLower]).map(encodeTag).join("+"))}
          className="link mr2 tag-include"
        >
          ⊕
        </a>
      ) : (
        <a
          href={linkToFilterTag(cur.filter((t) => t !== kLower).map(encodeTag).join("+"))}
          className="link mr2 tag-exclude"
        >
          ⊖
        </a>
      );

    out.push(
      <a key={`${k}-tag`} href={linkToFilterTag(encodeTag(k))} className="link tag mr1" style={style}>
        {k}
      </a>,
    );
    if (includeExclude) out.push(<React.Fragment key={`${k}-ie`}>{includeExclude}</React.Fragment>);
  }
  return out;
}

export function TagCloud({ initialMode }: { initialMode: TagCloudModeF }) {
  const a = app();
  const mode = useTagCloudStore((s) => s.mode);
  const tagcloud = useTagCloudStore((s) => s.tagcloud);
  const setMode = useTagCloudStore((s) => s.setMode);
  const setTagCloud = useTagCloudStore((s) => s.setTagCloud);

  React.useEffect(() => {
    setMode(initialMode);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  React.useEffect(() => {
    async function fetchTagCloud(m: TagCloudModeF) {
      if (m.kind === "none") return;
      const tc = await getTagCloud(tagCloudModeFromF(m) as TagCloudMode);
      setTagCloud(tc ?? {});
    }
    void fetchTagCloud(mode);
  }, [mode, setTagCloud]);

  if (mode.kind === "none") return <div className="tag_cloud" />;

  const modetop: TagCloudModeF = { kind: "top", expanded: isExpanded(mode), value: 200 };
  const modelb1: TagCloudModeF = { kind: "lowerBound", expanded: isExpanded(mode), value: 1 };
  const modelb2: TagCloudModeF = { kind: "lowerBound", expanded: isExpanded(mode), value: 2 };
  const modelb5: TagCloudModeF = { kind: "lowerBound", expanded: isExpanded(mode), value: 5 };
  const modelb10: TagCloudModeF = { kind: "lowerBound", expanded: isExpanded(mode), value: 10 };
  const modelb20: TagCloudModeF = { kind: "lowerBound", expanded: isExpanded(mode), value: 20 };

  const values = Object.values(tagcloud);
  const n = values.length ? Math.min(...values) : 1;
  const m = values.length ? Math.max(...values) : 1;

  async function onExpanded(expanded: boolean) {
    const next = setExpanded(mode, expanded);
    setMode(next);
    await updateTagCloudMode(tagCloudModeFromF(next) as TagCloudMode);
  }

  async function onChangeMode(next: TagCloudModeF) {
    if (JSON.stringify(mode) === JSON.stringify(next)) {
      await onExpanded(!isExpanded(mode));
    } else {
      setMode(setExpanded(next, true));
    }
  }

  return (
    <div className="tag_cloud mv3">
      <div className="tag_cloud_header mb2">
        {isRelated(mode) ? (
          <button
            type="button"
            className="pa1 f7 link hover-blue mr1 b"
            onClick={() => void onExpanded(!isExpanded(mode))}
          >
            Related Tags
          </button>
        ) : (
          <>
            <button
              type="button"
              className={`pa1 f7 link hover-blue mr1${JSON.stringify(mode) === JSON.stringify(modetop) ? " b" : ""}`}
              title="show a cloud of your most-used tags"
              onClick={() => void onChangeMode(modetop)}
            >
              Top Tags
            </button>
            <button
              type="button"
              className={`pa1 f7 link hover-blue ml2 ${JSON.stringify(mode) === JSON.stringify(modelb1) ? " b" : ""}`}
              title="show all tags"
              onClick={() => void onChangeMode(modelb1)}
            >
              all
            </button>
            {"‧"}
            <button
              type="button"
              className={`pa1 f7 link hover-blue${JSON.stringify(mode) === JSON.stringify(modelb2) ? " b" : ""}`}
              title="show tags with at least 2 bookmarks"
              onClick={() => void onChangeMode(modelb2)}
            >
              2
            </button>
            {"‧"}
            <button
              type="button"
              className={`pa1 f7 link hover-blue${JSON.stringify(mode) === JSON.stringify(modelb5) ? " b" : ""}`}
              title="show tags with at least 5 bookmarks"
              onClick={() => void onChangeMode(modelb5)}
            >
              5
            </button>
            {"‧"}
            <button
              type="button"
              className={`pa1 f7 link hover-blue${JSON.stringify(mode) === JSON.stringify(modelb10) ? " b" : ""}`}
              title="show tags with at least 10 bookmarks"
              onClick={() => void onChangeMode(modelb10)}
            >
              10
            </button>
            {"‧"}
            <button
              type="button"
              className={`pa1 f7 link hover-blue${JSON.stringify(mode) === JSON.stringify(modelb20) ? " b" : ""}`}
              title="show tags with at least 20 bookmarks"
              onClick={() => void onChangeMode(modelb20)}
            >
              20
            </button>
          </>
        )}
        <button type="button" className="pa1 ml2 f7 link silver hover-blue" onClick={() => void onExpanded(!isExpanded(mode))}>
          {isExpanded(mode) ? "hide" : "show"}
        </button>
      </div>

      {isExpanded(mode) ? (
        <div className="tag_cloud_body">
          {mode.kind === "related" ? toArray(mode.value, n, m, tagcloud) : toArray([], n, m, tagcloud)}
        </div>
      ) : null}
    </div>
  );
}


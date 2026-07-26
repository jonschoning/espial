import React from 'react';

import { getTagCloud, updateTagCloudMode } from '../api';
import { app } from '../globals';
import { useTagCloudStore } from '../stores/tagCloudStore';
import {
  isExpanded,
  setExpanded,
  type TagCloud,
  type TagCloudModeBrowse,
  type TagCloudModeF,
  tagCloudModeFromF,
  type TagCloudModeRelated,
} from '../types';
import { buildTagUrl } from '../urlBuild';

/** Binds click handlers on the server-rendered tag cloud header (mode/filter controls) to the tag cloud store. */
export function bindTagCloudHeader(renderElSelector: string) {
  return () => {
    const container = document.querySelector<HTMLElement>(renderElSelector);
    if (!container) return;

    container.addEventListener('click', (e) => {
      const target = (e.target as HTMLElement).closest<HTMLElement>('[data-tcm-action]');
      if (!target || !container.contains(target)) return;
      const parsed = readTagCloudHeaderAction(target);
      if (!parsed) return;
      e.preventDefault();

      const mode = useTagCloudStore.getState().mode;

      if (parsed.action === 'toggleExpand') {
        void applyTagCloudMode(setExpanded(mode, !isExpanded(mode)));
        return;
      }

      if (parsed.action === 'top' || parsed.action === 'lowerBound') {
        const next: TagCloudModeBrowse =
          parsed.action === 'top'
            ? { kind: 'top', expanded: isExpanded(mode) }
            : { kind: 'lowerBound', expanded: isExpanded(mode), value: parsed.value };
        const isActive =
          (next.kind === 'top' && mode.kind === 'top') ||
          (next.kind === 'lowerBound' && mode.kind === 'lowerBound' && mode.value === next.value);
        void applyTagCloudMode(
          isActive ? setExpanded(mode, !isExpanded(mode)) : setExpanded(next, true),
        );
        return;
      }

      const relatedTags = currentRelatedTags(mode);
      const next: TagCloudModeRelated =
        parsed.action === 'related'
          ? { kind: 'related', expanded: isExpanded(mode), value: relatedTags }
          : {
              kind: 'relatedLowerBound',
              expanded: isExpanded(mode),
              value: relatedTags,
              lowerBound: parsed.lowerBound,
            };
      const isActive =
        (next.kind === 'related' && mode.kind === 'related') ||
        (next.kind === 'relatedLowerBound' &&
          mode.kind === 'relatedLowerBound' &&
          mode.lowerBound === next.lowerBound);
      void applyTagCloudMode(
        isActive ? setExpanded(mode, !isExpanded(mode)) : setExpanded(next, true),
      );
    });

    function updateHeaderUI(mode: TagCloudModeF) {
      if (!container) return;
      if (mode.kind === 'none') return;
      container.querySelectorAll<HTMLElement>('[data-tcm-action]').forEach((btn) => {
        const parsed = readTagCloudHeaderAction(btn);
        if (!parsed) return;
        switch (parsed.action) {
          case 'top':
            btn.classList.toggle('b', mode.kind === 'top');
            return;
          case 'lowerBound':
            btn.classList.toggle('b', mode.kind === 'lowerBound' && mode.value === parsed.value);
            return;
          case 'related':
            btn.classList.toggle('b', mode.kind === 'related');
            return;
          case 'relatedLowerBound':
            btn.classList.toggle(
              'b',
              mode.kind === 'relatedLowerBound' && mode.lowerBound === parsed.lowerBound,
            );
            return;
          case 'toggleExpand': {
            const label = isExpanded(mode) ? btn.dataset.hideLabel : btn.dataset.showLabel;
            if (label !== undefined) btn.textContent = label;
            return;
          }
        }
      });
    }

    useTagCloudStore.subscribe((state) => {
      updateHeaderUI(state.mode);
    });
  };
}

type TagCloudHeaderAction =
  | { action: 'top' }
  | { action: 'lowerBound'; value: number }
  | { action: 'related' }
  | { action: 'relatedLowerBound'; lowerBound: number }
  | { action: 'toggleExpand' };

function readTagCloudHeaderAction(el: HTMLElement): TagCloudHeaderAction | null {
  switch (el.dataset.tcmAction) {
    case 'top':
      return { action: 'top' };
    case 'lowerBound':
      return { action: 'lowerBound', value: Number(el.dataset.tcmValue) };
    case 'related':
      return { action: 'related' };
    case 'relatedLowerBound':
      return { action: 'relatedLowerBound', lowerBound: Number(el.dataset.tcmLb) };
    case 'toggleExpand':
      return { action: 'toggleExpand' };
    default:
      return null;
  }
}

async function applyTagCloudMode(next: TagCloudModeF) {
  useTagCloudStore.getState().setMode(next);
  if (app().dat.isowner) {
    await updateTagCloudMode(tagCloudModeFromF(next));
  }
}

function currentRelatedTags(mode: TagCloudModeF): string[] {
  return mode.kind === 'related' || mode.kind === 'relatedLowerBound' ? mode.value : [];
}

/** Displays the tag cloud body (the list of tag links). The header (mode/filter controls) is server-rendered; see bindTagCloudHeader. */
export function TagCloudBody({ initialMode }: { initialMode: TagCloudModeF }) {
  const mode = useTagCloudStore((s) => s.mode);
  const tagcloud = useTagCloudStore((s) => s.tagcloud);
  const setMode = useTagCloudStore((s) => s.setMode);
  const setTagCloud = useTagCloudStore((s) => s.setTagCloud);

  React.useEffect(() => {
    setMode(initialMode);
  }, [initialMode]);

  React.useEffect(() => {
    async function fetchTagCloud(m: TagCloudModeF) {
      if (m.kind === 'none') return;
      const tc = await getTagCloud(tagCloudModeFromF(m));
      setTagCloud(tc ?? {});
    }
    void fetchTagCloud(mode);
  }, [mode, setTagCloud]);

  if (mode.kind === 'none' || !isExpanded(mode)) return null;

  const curtags = mode.kind === 'related' || mode.kind === 'relatedLowerBound' ? mode.value : [];

  return <div className="tag_cloud_body">{renderLinks(curtags, tagcloud)}</div>;
}

function renderLinks(curtags: string[], tagcloud: TagCloud) {
  const a = app();
  const cur = curtags.map((tag) => tag.toLowerCase());

  const linkToFilterTag = (tags: string[]) => buildTagUrl(a, tags);

  const tagCounts = Object.values(tagcloud);
  const cMin = tagCounts.length ? Math.min(...tagCounts) : 1;
  const cMax = tagCounts.length ? Math.max(...tagCounts) : 1;

  return Object.entries(tagcloud)
    .sort((x, y) => x[0].toLowerCase().localeCompare(y[0].toLowerCase()))
    .map(([tag, c]) => {
      const kLower = tag.toLowerCase();
      const fontsize = rescale((x) => Math.log(1.0 + x), c, cMin, cMax, 100.0, 133.0);
      const opacity = rescale((x) => Math.log(1.0 + x), c, cMin, cMax, 0.6, 1.0);
      const style: React.CSSProperties = { fontSize: `${fontsize.toString()}%`, opacity };

      const includeExcludeLink =
        cur.length === 0 ? null : !cur.includes(kLower) ? (
          <a href={linkToFilterTag(cur.concat([kLower]))} className="link mr2 tag-include">
            ⊕
          </a>
        ) : (
          <a
            href={linkToFilterTag(cur.filter((t) => t !== kLower))}
            className="link mr2 tag-exclude"
          >
            ⊖
          </a>
        );

      return (
        <React.Fragment key={tag}>
          <a href={linkToFilterTag([tag])} className="link tag mr1" style={style}>
            {tag}
          </a>
          {includeExcludeLink}
        </React.Fragment>
      );
    });
}

/**  Rescales a value v in the range [n, m] to a value in the range [l, h] using a function f.
 *  @param f - The function to apply to the value.
 *  @param v - The value to rescale.
 *  @param n - The minimum of the input range.
 *  @param m - The maximum of the input range.
 *  @param l - The minimum of the output range.
 *  @param h - The maximum of the output range.
 *  @returns The rescaled value.
 */
function rescale(
  f: (x: number) => number,
  v: number,
  n: number,
  m: number,
  l: number,
  h: number,
): number {
  const denom = f(m - n);
  const ratio = m - n < 0.01 || denom === 0 ? 1.0 : f(v - n) / denom;
  return ratio * (h - l) + l;
}

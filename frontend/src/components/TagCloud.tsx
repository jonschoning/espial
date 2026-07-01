import React from 'react';
import { useTranslation } from 'react-i18next';

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
import { encodeTag, fromNullableStr } from '../util';

/** Displays the tag cloud, with controls to filter by top tags, frequency, or related tags. */
export function TagCloud({ initialMode }: { initialMode: TagCloudModeF }) {
  const { t } = useTranslation();
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

  if (mode.kind === 'none') return <div className="tag_cloud" />;

  const makeLbMode = (n: number): TagCloudModeBrowse => ({
    kind: 'lowerBound',
    expanded: isExpanded(mode),
    value: n,
  });

  const relatedTags =
    mode.kind === 'related' || mode.kind === 'relatedLowerBound' ? mode.value : [];
  const makeRelatedMode = (): TagCloudModeRelated => ({
    kind: 'related',
    expanded: isExpanded(mode),
    value: relatedTags,
  });
  const makeRelatedLbMode = (lb: number): TagCloudModeRelated => ({
    kind: 'relatedLowerBound',
    expanded: isExpanded(mode),
    value: relatedTags,
    lowerBound: lb,
  });

  async function onExpanded(expanded: boolean) {
    const next = setExpanded(mode, expanded);
    setMode(next);
    if (app().dat.isowner) {
      await updateTagCloudMode(tagCloudModeFromF(next));
    }
  }

  async function onChangeBrowse(next: TagCloudModeBrowse) {
    const isActive =
      (next.kind === 'top' && mode.kind === 'top') ||
      (next.kind === 'lowerBound' && mode.kind === 'lowerBound' && mode.value === next.value);
    if (isActive) {
      await onExpanded(!isExpanded(mode));
    } else {
      setMode(setExpanded(next, true));
    }
  }

  function onChangeRelated(next: TagCloudModeRelated) {
    const isActive =
      (next.kind === 'related' && mode.kind === 'related') ||
      (next.kind === 'relatedLowerBound' &&
        mode.kind === 'relatedLowerBound' &&
        mode.lowerBound === next.lowerBound);
    if (isActive) {
      void onExpanded(!isExpanded(mode));
    } else {
      setMode(setExpanded(next, true));
    }
  }

  const isTagCloudModeRelated = mode.kind === 'related' || mode.kind === 'relatedLowerBound';
  const activeLb = mode.kind === 'relatedLowerBound' ? mode.lowerBound : 0;

  return (
    <div className="tag_cloud mv3">
      <div className="tag_cloud_header mb2">
        {isTagCloudModeRelated ? (
          <>
            <button
              type="button"
              className={`pa1 f7 link thm-hover-link-color mr1${mode.kind === 'related' ? ' b' : ''}`}
              title={t('tagCloud.topTagsTitle')}
              onClick={() => {
                onChangeRelated(makeRelatedMode());
              }}
            >
              {t('tagCloud.relatedTags')}
            </button>
            {(
              [
                [1, 'tagCloud.allTitle'],
                [2, 'tagCloud.min2Title'],
                [5, 'tagCloud.min5Title'],
                [10, 'tagCloud.min10Title'],
                [20, 'tagCloud.min20Title'],
              ] as const
            ).map(([lb, titleKey]) => (
              <React.Fragment key={lb}>
                {lb !== 1 && '‧'}
                <button
                  type="button"
                  className={`pa1 f7 link thm-hover-link-color${lb === 1 ? ' ml2' : ''}${activeLb === lb ? ' b' : ''}`}
                  title={t(titleKey)}
                  onClick={() => {
                    onChangeRelated(makeRelatedLbMode(lb));
                  }}
                >
                  {lb === 1 ? t('filter.all') : lb}
                </button>
              </React.Fragment>
            ))}
          </>
        ) : (
          <>
            <button
              type="button"
              className={`pa1 f7 link thm-hover-link-color mr1${mode.kind === 'top' ? ' b' : ''}`}
              title={t('tagCloud.topTagsTitle')}
              onClick={() => void onChangeBrowse({ kind: 'top', expanded: isExpanded(mode) })}
            >
              {t('tagCloud.topTags')}
            </button>
            {(
              [
                [1, 'tagCloud.allTitle'],
                [2, 'tagCloud.min2Title'],
                [5, 'tagCloud.min5Title'],
                [10, 'tagCloud.min10Title'],
                [20, 'tagCloud.min20Title'],
              ] as const
            ).map(([n, titleKey]) => (
              <React.Fragment key={n}>
                {n !== 1 && '‧'}
                <button
                  type="button"
                  className={`pa1 f7 link thm-hover-link-color${n === 1 ? ' ml2' : ''}${mode.kind === 'lowerBound' && mode.value === n ? ' b' : ''}`}
                  title={t(titleKey)}
                  onClick={() => void onChangeBrowse(makeLbMode(n))}
                >
                  {n === 1 ? t('filter.all') : n}
                </button>
              </React.Fragment>
            ))}
          </>
        )}
        <button
          type="button"
          className="pa1 ml2 f7 link thm-text-faded thm-hover-link-color"
          onClick={() => void onExpanded(!isExpanded(mode))}
        >
          {isExpanded(mode) ? t('tagCloud.hide') : t('tagCloud.show')}
        </button>
      </div>

      {isExpanded(mode) ? (
        <div className="tag_cloud_body">
          {renderLinks(isTagCloudModeRelated ? mode.value : [], tagcloud)}
        </div>
      ) : null}
    </div>
  );
}

function renderLinks(curtags: string[], tagcloud: TagCloud) {
  const a = app();
  const cur = curtags.map((tag) => tag.toLowerCase());

  const linkToFilterTag = (rest: string) =>
    `${fromNullableStr(a.userR)}${rest === '' ? '' : `/t:${rest}`}`;

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
          <a
            href={linkToFilterTag(cur.concat([kLower]).map(encodeTag).join('+'))}
            className="link mr2 tag-include"
          >
            ⊕
          </a>
        ) : (
          <a
            href={linkToFilterTag(
              cur
                .filter((t) => t !== kLower)
                .map(encodeTag)
                .join('+'),
            )}
            className="link mr2 tag-exclude"
          >
            ⊖
          </a>
        );

      return (
        <React.Fragment key={tag}>
          <a href={linkToFilterTag(encodeTag(tag))} className="link tag mr1" style={style}>
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

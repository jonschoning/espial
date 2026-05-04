import React from 'react';

import type { TSuggestion } from '../types';

const MAX_SUGGESTION_TERM_RENDER_LENGTH = 80;

export type TagTokenRange = {
  token: string;
  start: number;
  end: number;
};

export type SuggestionState = {
  items: TSuggestion[];
  selectedIndex: number;
  tokenRange: TagTokenRange;
  anchorLeft: number;
  anchorTop: number;
};

export function getActiveTagToken(value: string, caret: number): TagTokenRange | null {
  const boundedCaret = Math.max(0, Math.min(caret, value.length));

  let start = boundedCaret;
  while (start > 0 && value[start - 1] !== ' ') {
    start -= 1;
  }

  let end = boundedCaret;
  while (end < value.length && value[end] !== ' ') {
    end += 1;
  }

  const token = value.slice(start, end).trim();
  return token === '' ? null : { token, start, end };
}

function renderHighlightedSuggestion(term: string, query: string): React.ReactNode {
  const lowerTerm = term.toLocaleLowerCase();
  const lowerQuery = query.toLocaleLowerCase();
  const idx = lowerTerm.indexOf(lowerQuery);
  if (idx < 0) return term;

  const before = term.slice(0, idx);
  const match = term.slice(idx, idx + query.length);
  const after = term.slice(idx + query.length);

  return (
    <>
      {before}
      <strong>{match}</strong>
      {after}
    </>
  );
}

function truncateSuggestionTerm(term: string): string {
  if (term.length <= MAX_SUGGESTION_TERM_RENDER_LENGTH) return term;
  return `${term.slice(0, MAX_SUGGESTION_TERM_RENDER_LENGTH)}...`;
}

export function measureCaretPosition(
  input: HTMLInputElement,
  caret: number,
): { left: number; top: number } {
  const style = window.getComputedStyle(input);
  const mirror = document.createElement('div');
  mirror.style.position = 'absolute';
  mirror.style.visibility = 'hidden';
  mirror.style.whiteSpace = 'pre';
  mirror.style.top = '0';
  mirror.style.left = '0';
  mirror.style.font = style.font;
  mirror.style.fontKerning = style.fontKerning;
  mirror.style.fontFeatureSettings = style.fontFeatureSettings;
  mirror.style.fontVariantLigatures = style.fontVariantLigatures;
  mirror.style.letterSpacing = style.letterSpacing;
  mirror.style.textTransform = style.textTransform;
  mirror.style.textIndent = style.textIndent;
  mirror.style.boxSizing = style.boxSizing;
  mirror.style.padding = style.padding;
  mirror.style.border = style.border;
  mirror.style.width = `${String(input.clientWidth)}px`;

  const prefix = document.createElement('span');
  prefix.textContent = input.value.slice(0, caret);
  mirror.appendChild(prefix);

  const marker = document.createElement('span');
  marker.textContent = '\u200b';
  mirror.appendChild(marker);

  document.body.appendChild(mirror);
  const left = marker.offsetLeft - input.scrollLeft + 1;
  const top = marker.offsetTop - input.scrollTop + input.offsetHeight + 4;
  document.body.removeChild(mirror);

  return { left, top };
}

export function replaceTokenAtRange(
  value: string,
  range: TagTokenRange,
  replacement: string,
): { value: string; caret: number } {
  const prefix = value.slice(0, range.start);
  const suffix = value.slice(range.end);
  const needsTrailingSpace = suffix === '' || !suffix.startsWith(' ');
  const inserted = `${replacement}${needsTrailingSpace ? ' ' : ''}`;
  return {
    value: `${prefix}${inserted}${suffix}`,
    caret: prefix.length + inserted.length,
  };
}

export function TagSuggestionsDropdown({
  suggestionState,
  onHover,
  onPick,
}: {
  suggestionState: SuggestionState;
  onHover: (index: number) => void;
  onPick: (term: string) => void;
}) {
  return (
    <div
      className="absolute z-5 bg-white ba b--black-10 br2 shadow-4 overflow-hidden"
      style={{
        left: suggestionState.anchorLeft,
        top: suggestionState.anchorTop,
        width: 'max-content',
        maxWidth: '24rem',
      }}
    >
      {suggestionState.items.map((item, index) => {
        const selected = index === suggestionState.selectedIndex;
        const displayTerm = truncateSuggestionTerm(item.term);
        return (
          <button
            key={`${item.term}-${item.count.toString()}`}
            type="button"
            className={`flex items-center justify-between w-100 bn bg-transparent tl ph2 pv2 pointer nowrap ${selected ? 'bg-light-blue' : ''}`}
            onMouseEnter={() => {
              onHover(index);
            }}
            onMouseDown={(e) => {
              e.preventDefault();
            }}
            onClick={() => {
              onPick(item.term);
            }}
            title={item.term}
          >
            <span className="mr2">
              {renderHighlightedSuggestion(displayTerm, suggestionState.tokenRange.token)}
            </span>
            <span className="gray f7 tr" style={{ minWidth: '3ch' }}>
              {item.count}
            </span>
          </button>
        );
      })}
    </div>
  );
}

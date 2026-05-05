import React from 'react';

import { fetchTagSuggestions } from '../api';
import type { TagSuggestionRequest, TSuggestion } from '../types';

export type SuggestionState = {
  items: TSuggestion[];
  selectedIndex: number;
  tokenRange: TagTokenRange;
  anchorLeft: number;
  anchorTop: number;
};

type TagTokenRange = {
  token: string;
  start: number;
  end: number;
};

const TAG_SUGGESTION_DEBOUNCE_MS = 180;

export function useTagSuggestions({
  enabled,
  tags,
  onTagsUpdate,
  maxSuggestions = 10,
}: {
  enabled: boolean;
  tags: string;
  onTagsUpdate: (next: string) => void;
  maxSuggestions?: number;
}) {
  const [suggestionState, setSuggestionState] = React.useState<SuggestionState | null>(null);
  const tagSuggestionRequestId = React.useRef(0);
  const tagSuggestionDebounceRef = React.useRef<number | null>(null);
  const tagInputRef = React.useRef<HTMLInputElement | null>(null);

  const cancelPendingSuggestions = React.useCallback(() => {
    if (tagSuggestionDebounceRef.current != null) {
      window.clearTimeout(tagSuggestionDebounceRef.current);
      tagSuggestionDebounceRef.current = null;
    }
    tagSuggestionRequestId.current += 1;
  }, []);

  const closeSuggestions = React.useCallback(() => {
    cancelPendingSuggestions();
    setSuggestionState(null);
  }, [cancelPendingSuggestions]);

  React.useEffect(() => {
    return () => {
      cancelPendingSuggestions();
    };
  }, [cancelPendingSuggestions]);

  React.useEffect(() => {
    if (!enabled) closeSuggestions();
  }, [enabled, closeSuggestions]);

  const onTagsChange = React.useCallback(
    (e: React.ChangeEvent<HTMLInputElement>) => {
      const { value, selectionStart, selectionEnd } = e.target;

      onTagsUpdate(value);

      if (!enabled) {
        closeSuggestions();
        return;
      }

      if (selectionStart == null || selectionEnd == null || selectionStart !== selectionEnd) {
        closeSuggestions();
        return;
      }

      if (isWhitespaceOnlyEdit(tags, value)) {
        closeSuggestions();
        return;
      }

      const activeToken = getActiveTagToken(value, selectionStart);
      if (activeToken == null || activeToken.token.length < 2) {
        closeSuggestions();
        return;
      }

      const normalizedTagTerms = getNormalizedTagTerms(value);
      const activeTokenNormalized = activeToken.token.toLowerCase();
      const activeTokenOccurrences = normalizedTagTerms.filter(
        (tag) => tag === activeTokenNormalized,
      ).length;
      const shouldExcludeActiveToken = activeTokenOccurrences > 1;
      const payload: TagSuggestionRequest = {
        query: activeToken.token,
        currentTags: Array.from(
          new Set(
            normalizedTagTerms.filter(
              (tag) => shouldExcludeActiveToken || tag !== activeTokenNormalized,
            ),
          ),
        ),
      };

      cancelPendingSuggestions();
      tagSuggestionDebounceRef.current = window.setTimeout(() => {
        const requestId = tagSuggestionRequestId.current + 1;
        tagSuggestionRequestId.current = requestId;

        void (async () => {
          const response = await fetchTagSuggestions(payload);

          if (
            requestId !== tagSuggestionRequestId.current ||
            response == null ||
            tagInputRef.current == null
          ) {
            return;
          }

          const items = response.suggestions.slice(0, maxSuggestions);
          if (items.length === 0) {
            closeSuggestions();
            return;
          }

          const anchor = measureCaretPosition(tagInputRef.current, selectionStart);

          setSuggestionState({
            items,
            selectedIndex: 0,
            tokenRange: activeToken,
            anchorLeft: anchor.left,
            anchorTop: anchor.top,
          });
        })();
      }, TAG_SUGGESTION_DEBOUNCE_MS);
    },
    [cancelPendingSuggestions, closeSuggestions, enabled, maxSuggestions, onTagsUpdate],
  );

  const applySuggestion = React.useCallback(
    (term: string) => {
      const input = tagInputRef.current;
      if (input == null || suggestionState == null) return;

      const next = replaceTokenAtRange(tags, suggestionState.tokenRange, term);
      onTagsUpdate(next.value);
      closeSuggestions();

      requestAnimationFrame(() => {
        input.focus();
        input.setSelectionRange(next.caret, next.caret);
      });
    },
    [closeSuggestions, onTagsUpdate, suggestionState, tags],
  );

  const onTagsKeyDown = React.useCallback(
    (e: React.KeyboardEvent<HTMLInputElement>) => {
      if (e.key === 'Escape') {
        if (suggestionState != null) {
          e.preventDefault();
          closeSuggestions();
        }
        return;
      }

      if (e.key === ' ') {
        closeSuggestions();
        return;
      }

      if (suggestionState == null) return;

      if (e.key === 'ArrowDown') {
        e.preventDefault();
        setSuggestionState((current) => {
          if (current == null) return current;
          return {
            ...current,
            selectedIndex: (current.selectedIndex + 1) % current.items.length,
          };
        });
        return;
      }

      if (e.key === 'ArrowUp') {
        e.preventDefault();
        setSuggestionState((current) => {
          if (current == null) return current;
          return {
            ...current,
            selectedIndex:
              (current.selectedIndex - 1 + current.items.length) % current.items.length,
          };
        });
        return;
      }

      if (e.key === 'Enter' || e.key === 'Tab') {
        e.preventDefault();
        const selected = suggestionState.items[suggestionState.selectedIndex];
        applySuggestion(selected.term);
      }
    },
    [applySuggestion, closeSuggestions, suggestionState],
  );

  const onTagsSelect = React.useCallback(
    (e: React.SyntheticEvent<HTMLInputElement>) => {
      if (suggestionState == null) return;

      const input = e.currentTarget;
      const { selectionStart, selectionEnd, value } = input;
      if (selectionStart == null || selectionEnd == null || selectionStart !== selectionEnd) {
        closeSuggestions();
        return;
      }

      const activeToken = getActiveTagToken(value, selectionStart);
      if (activeToken == null) {
        closeSuggestions();
        return;
      }

      if (activeToken.start !== suggestionState.tokenRange.start) {
        closeSuggestions();
      }
    },
    [closeSuggestions, suggestionState],
  );

  const onSuggestionHover = React.useCallback((index: number) => {
    setSuggestionState((current) => {
      if (current == null) return current;
      return { ...current, selectedIndex: index };
    });
  }, []);

  return {
    tagInputRef,
    suggestionState,
    closeSuggestions,
    onTagsChange,
    onTagsKeyDown,
    onTagsSelect,
    onSuggestionHover,
    onSuggestionPick: applySuggestion,
  };
}

function getActiveTagToken(value: string, caret: number): TagTokenRange | null {
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

function measureCaretPosition(
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

function replaceTokenAtRange(
  value: string,
  range: TagTokenRange,
  replacement: string,
): { value: string; caret: number } {
  const prefix = value.slice(0, range.start);
  // Consume any existing leading space from the suffix so we always own the
  // separator. This lets us unconditionally append a trailing space and place
  // the caret after it, whether the accepted tag is in the middle or at the end.
  const suffix = value.slice(range.end).replace(/^ /, '');
  const inserted = `${replacement} `;
  return {
    value: `${prefix}${inserted}${suffix}`,
    caret: prefix.length + inserted.length,
  };
}

function isWhitespaceOnlyEdit(previous: string, next: string): boolean {
  if (previous === next) return false;

  let start = 0;
  while (start < previous.length && start < next.length && previous[start] === next[start]) {
    start += 1;
  }

  let prevEnd = previous.length;
  let nextEnd = next.length;
  while (prevEnd > start && nextEnd > start && previous[prevEnd - 1] === next[nextEnd - 1]) {
    prevEnd -= 1;
    nextEnd -= 1;
  }

  const removed = previous.slice(start, prevEnd);
  const added = next.slice(start, nextEnd);
  if (removed === '' && added === '') return false;
  return isSpacesOnly(removed) && isSpacesOnly(added);
}

function isSpacesOnly(value: string): boolean {
  return value === '' || /^ *$/.test(value);
}

function getNormalizedTagTerms(value: string): string[] {
  return value
    .split(' ')
    .map((tag) => tag.trim().toLowerCase())
    .filter((tag) => tag !== '');
}

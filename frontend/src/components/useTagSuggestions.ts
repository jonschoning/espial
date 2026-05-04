import React from 'react';

import { fetchTagSuggestions } from '../api';
import type { TagSuggestions } from '../types';
import {
  getActiveTagToken,
  measureCaretPosition,
  replaceTokenAtRange,
  type SuggestionState,
} from './TagSuggestions';

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

      const activeToken = getActiveTagToken(value, selectionStart);
      if (activeToken == null || activeToken.token.length < 2) {
        closeSuggestions();
        return;
      }

      const payload: TagSuggestions = {
        query: activeToken.token,
        suggestions: [],
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

          const items = [...response.suggestions].slice(0, maxSuggestions);
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

      if (
        activeToken.start !== suggestionState.tokenRange.start ||
        activeToken.end !== suggestionState.tokenRange.end
      ) {
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

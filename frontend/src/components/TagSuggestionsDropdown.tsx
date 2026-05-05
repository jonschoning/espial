import React from 'react';

import type { SuggestionState } from '../hooks/useTagSuggestions';

const MAX_SUGGESTION_TERM_RENDER_LENGTH = 80;

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
      className="absolute z-5 bg-near-white dark-gray ba b--moon-gray br2 shadow-4 overflow-hidden f6"
      style={{
        left: suggestionState.anchorLeft,
        top: suggestionState.anchorTop,
        width: 'max-content',
        maxWidth: '22rem',
      }}
    >
      {suggestionState.items.map((item, index) => {
        const selected = index === suggestionState.selectedIndex;
        const displayTerm = truncateSuggestionTerm(item.term);
        return (
          <button
            key={`${item.term}-${item.count.toString()}`}
            type="button"
            className={`flex items-center justify-between w-100 bn bg-transparent tl ph2 pv1 pointer nowrap dark-gray hover-blue ${selected ? 'bg-light-blue dark-blue' : ''}`}
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
            <span className="gray f6 tr" style={{ minWidth: '3ch' }}>
              {item.count}
            </span>
          </button>
        );
      })}
    </div>
  );
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

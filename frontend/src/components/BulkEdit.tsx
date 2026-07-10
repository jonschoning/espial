import { TimeoutError } from 'ky';
import React, { useEffect, useRef, useState } from 'react';
import { useTranslation } from 'react-i18next';

import { apiErrorMsg, normalizeTags } from '@/util';

import * as api from '../api';
import { app } from '../globals';
import { useTagSuggestions } from '../hooks/useTagSuggestions';
import { useBookmarksStore } from '../stores/bookmarksStore';
import { TagSuggestionsDropdown } from './TagSuggestionsDropdown';

type BulkAction = 'read' | 'unread' | 'star' | 'unstar' | 'delete' | 'private' | 'public';
type BulkSelection = 'page' | 'all';

interface Props {
  bcount: number;
}

type BulkValidationKey =
  | 'bulkEdit.pleaseAddSelection'
  | 'bulkEdit.pleaseChooseAction'
  | 'bulkEdit.tooManyTags';

function disabledReason(
  selection: BulkSelection | null,
  action: BulkAction | null,
  addTags: string,
  removeTags: string,
  bcount: number,
): BulkValidationKey | null {
  if (selection === null) return 'bulkEdit.pleaseAddSelection';
  if (!action && !addTags.trim() && !removeTags.trim()) return 'bulkEdit.pleaseChooseAction';
  if (bcount > 20000 && addTags.trim().split(/\s+/).filter(Boolean).length > 10)
    return 'bulkEdit.tooManyTags';
  return null;
}

function navLink<T>(current: T | null, value: T, label: string, onClick: (v: T) => void) {
  return (
    <a
      href="#"
      className={`dib mt1 link${current === value ? ' nav-active' : ' silver hover-alt'}`}
      onClick={(e) => {
        e.preventDefault();
        onClick(value);
      }}
    >
      {label}
    </a>
  );
}

export function BulkEdit({ bcount }: Props) {
  const { t } = useTranslation();
  const [visible, setVisible] = useState(false);
  const [selection, setSelection] = useState<BulkSelection | null>(null);
  const [action, setAction] = useState<BulkAction | null>(null);
  const [addTags, setAddTags] = useState('');
  const [removeTags, setRemoveTags] = useState('');
  const [submitting, setSubmitting] = useState(false);
  const [confirmDelete, setConfirmDelete] = useState(false);
  const [confirmPublic, setConfirmPublic] = useState(false);
  const [validationMsg, setValidationMsg] = useState<string | null>(null);
  const [errorFadingOut, setErrorFadingOut] = useState(false);
  const validationTimer = useRef<number | null>(null);
  const fadeTimer = useRef<number | null>(null);

  const a = app();
  const suggestEnabled = a.dat.suggestTags === true;
  const suggestUseReturnKey = a.dat.suggestTagsUseReturnKey !== false;
  const bulkFilter = a.dat.filter ?? { tag: 'FilterAll' as const };
  const bulkSharedp = a.dat.sharedp ?? 'all';
  const bulkTags = a.dat.tags ?? [];
  const bulkQuery = a.dat.query ?? null;

  const bmarks = useBookmarksStore((s) => s.bmarks);
  const bids = bmarks.map((b) => b.bid);
  const pageCount = bids.length;
  const selectionCount = selection === null ? 0 : selection === 'page' ? pageCount : bcount;

  const reason = disabledReason(selection, action, addTags, removeTags, bcount);
  const isVisuallyDisabled = submitting || reason !== null || confirmDelete || confirmPublic;

  const {
    tagInputRef: removeTagsRef,
    suggestionState: removeSuggestionState,
    closeSuggestions: closeRemoveSuggestions,
    onTagsChange: onRemoveTagsChange,
    onTagsKeyDown: onRemoveTagsKeyDown,
    onTagsSelect: onRemoveTagsSelect,
    onSuggestionHover: onRemoveSuggestionHover,
    onSuggestionPick: onRemoveSuggestionPick,
  } = useTagSuggestions({
    enabled: suggestEnabled && action !== 'delete',
    useReturnKey: suggestUseReturnKey,
    tags: removeTags,
    onTagsUpdate: setRemoveTags,
  });

  const {
    tagInputRef: addTagsRef,
    suggestionState: addSuggestionState,
    closeSuggestions: closeAddSuggestions,
    onTagsChange: onAddTagsChange,
    onTagsKeyDown: onAddTagsKeyDown,
    onTagsSelect: onAddTagsSelect,
    onSuggestionHover: onAddSuggestionHover,
    onSuggestionPick: onAddSuggestionPick,
  } = useTagSuggestions({
    enabled: suggestEnabled && action !== 'delete',
    useReturnKey: suggestUseReturnKey,
    tags: addTags,
    onTagsUpdate: setAddTags,
  });

  const clearValidationTimers = () => {
    if (validationTimer.current !== null) {
      window.clearTimeout(validationTimer.current);
      validationTimer.current = null;
    }
    if (fadeTimer.current !== null) {
      window.clearTimeout(fadeTimer.current);
      fadeTimer.current = null;
    }
  };

  const resetForm = () => {
    setSelection(null);
    setAction(null);
    setAddTags('');
    setRemoveTags('');
    setConfirmDelete(false);
    setConfirmPublic(false);
    setValidationMsg(null);
    setErrorFadingOut(false);
    clearValidationTimers();
  };

  useEffect(() => {
    const el = document.getElementById('bulk-edit-toggle');
    if (!el) return;
    const handler = (e: Event) => {
      e.preventDefault();
      resetForm();
      setVisible((v) => !v);
    };
    el.addEventListener('click', handler);
    return () => {
      el.removeEventListener('click', handler);
    };
  }, []);

  useEffect(() => {
    const el = document.getElementById('bulk-edit-toggle');
    if (!el) return;
    el.classList.toggle('nav-active', visible);
    el.classList.toggle('silver', !visible);
  }, [visible]);

  useEffect(() => {
    setConfirmDelete(false);
    setConfirmPublic(false);
    setValidationMsg(null);
    setErrorFadingOut(false);
  }, [action, selection]);

  useEffect(() => {
    return () => {
      clearValidationTimers();
    };
  }, []);

  const showValidationMsg = (msg: string) => {
    clearValidationTimers();
    setErrorFadingOut(false);
    setValidationMsg(msg);
    validationTimer.current = window.setTimeout(() => {
      validationTimer.current = null;
      setErrorFadingOut(true);
      fadeTimer.current = window.setTimeout(() => {
        setValidationMsg(null);
        setErrorFadingOut(false);
        fadeTimer.current = null;
      }, 5000);
    }, 3000);
  };

  const handleSubmit = async (
    e: React.SyntheticEvent<HTMLFormElement, SubmitEvent>,
  ): Promise<void> => {
    e.preventDefault();
    setValidationMsg(null);
    setErrorFadingOut(false);

    const addTagsTrimmed = normalizeTags(addTags);
    const removeTagsTrimmed = normalizeTags(removeTags);
    const r = disabledReason(selection, action, addTagsTrimmed, removeTagsTrimmed, bcount);
    if (r !== null) {
      showValidationMsg(t(r));
      return;
    }
    if (action === 'delete' && !confirmDelete) {
      setConfirmDelete(true);
      return;
    }
    if (action === 'public' && !confirmPublic) {
      setConfirmPublic(true);
      return;
    }
    setConfirmDelete(false);
    setConfirmPublic(false);
    setSubmitting(true);
    try {
      if (selection === null) return;
      const res = await api.bulkEdit(
        selection === 'page'
          ? {
              selection: 'page',
              bids,
              action,
              addTags: addTagsTrimmed,
              removeTags: removeTagsTrimmed,
              selectionCount,
            }
          : {
              selection: 'all',
              filter: bulkFilter,
              sharedp: bulkSharedp,
              tags: bulkTags,
              query: bulkQuery,
              action,
              addTags: addTagsTrimmed,
              removeTags: removeTagsTrimmed,
              selectionCount,
            },
      );
      if (res.ok) {
        window.location.reload();
      } else {
        showValidationMsg(apiErrorMsg(t, res.status, res.bodyText ?? ''));
      }
    } catch (err) {
      if (err instanceof TimeoutError) {
        showValidationMsg(t('error.requestTimedOut'));
      } else {
        showValidationMsg(t('error.networkError'));
      }
    } finally {
      setSubmitting(false);
    }
  };

  if (!visible) return null;

  const toggleAction = (v: BulkAction) => {
    setAction((cur) => (cur === v ? null : v));
    if (v === 'delete') {
      setAddTags('');
      setRemoveTags('');
    }
  };

  return (
    <form
      onSubmit={(e) => {
        void handleSubmit(e);
      }}
      className="w-100 mw7 pa2 bulk_bookmark_form mt1"
    >
      <div className="mb2">
        <div>
          <span className="fw7">{t('bulkEdit.select')}</span>
          {selection !== null && (
            <span className="ml2">
              {selectionCount === 1
                ? t('bulkEdit.oneBookmarkSelected')
                : t('bulkEdit.bookmarksSelected', { count: selectionCount })}
            </span>
          )}
        </div>
        <div>
          {navLink(selection, 'page', t('bulkEdit.thisPage'), setSelection)}
          {' ‧ '}
          {navLink(selection, 'all', t('bulkEdit.allPages'), setSelection)}
        </div>
      </div>

      <div className="mb2">
        <div>
          <span className="fw7">{t('bulkEdit.action')}</span>
        </div>
        <div>
          {navLink(action, 'private', t('bulkEdit.makePrivate'), toggleAction)}
          {' ‧ '}
          {navLink(action, 'public', t('bulkEdit.makePublic'), toggleAction)}
          {' ‧ '}
          {navLink(action, 'read', t('bulkEdit.markAsRead'), toggleAction)}
          {' ‧ '}
          {navLink(action, 'unread', t('bulkEdit.unread'), toggleAction)}
          {' ‧ '}
          {navLink(action, 'star', t('bulkEdit.addStars'), toggleAction)}
          {' ‧ '}
          {navLink(action, 'unstar', t('bulkEdit.removeStars'), toggleAction)}
          {' ‧ '}
          {navLink(action, 'delete', t('delete'), toggleAction)}
        </div>
      </div>

      <div className="mb2">
        <div>
          <span className={`fw7${normalizeTags(removeTags) ? ' nav-active' : ''}`}>
            {t('bulkEdit.removeTags')}
          </span>
        </div>
        <div className="relative">
          <input
            ref={removeTagsRef}
            type="text"
            className={`tags w-100 mb1 pt1 edit_form_input${action === 'delete' ? ' o-50 not-allowed' : ''}`}
            disabled={action === 'delete'}
            autoComplete="off"
            autoCapitalize="off"
            value={removeTags}
            onChange={onRemoveTagsChange}
            onKeyDown={onRemoveTagsKeyDown}
            onClick={onRemoveTagsSelect}
            onSelect={onRemoveTagsSelect}
            onBlur={() => {
              window.setTimeout(() => {
                closeRemoveSuggestions();
              }, 0);
            }}
          />
          {removeSuggestionState != null ? (
            <TagSuggestionsDropdown
              suggestionState={removeSuggestionState}
              onHover={onRemoveSuggestionHover}
              onPick={onRemoveSuggestionPick}
            />
          ) : null}
        </div>
      </div>

      <div className="mb2">
        <div>
          <span className={`fw7${normalizeTags(addTags) ? ' nav-active' : ''}`}>
            {t('bulkEdit.addTags')}
          </span>
        </div>
        <div className="relative">
          <input
            ref={addTagsRef}
            type="text"
            className={`tags w-100 mb1 pt1 edit_form_input${action === 'delete' ? ' o-50 not-allowed' : ''}`}
            disabled={action === 'delete'}
            autoComplete="off"
            autoCapitalize="off"
            value={addTags}
            onChange={onAddTagsChange}
            onKeyDown={onAddTagsKeyDown}
            onClick={onAddTagsSelect}
            onSelect={onAddTagsSelect}
            onBlur={() => {
              window.setTimeout(() => {
                closeAddSuggestions();
              }, 0);
            }}
          />
          {addSuggestionState != null ? (
            <TagSuggestionsDropdown
              suggestionState={addSuggestionState}
              onHover={onAddSuggestionHover}
              onPick={onAddSuggestionPick}
            />
          ) : null}
        </div>
      </div>

      <div className="flex items-center">
        <button
          type="submit"
          disabled={submitting || confirmDelete || confirmPublic}
          className={`mr2 pv1 ph2 thm-text-primary ba thm-border-default rdim${isVisuallyDisabled ? ' thm-bg-disabled not-allowed' : ' thm-bg-secondary pointer'}`}
          onClick={() => {
            if (reason !== null) showValidationMsg(t(reason));
          }}
        >
          {submitting ? t('bulkEdit.submitting') : t('bulkEdit.submit')}
        </button>{' '}
        <button
          type="button"
          className="pv1 ph2 thm-text-primary ba thm-border-default thm-bg-secondary pointer rdim"
          onClick={() => {
            resetForm();
            setVisible(false);
          }}
        >
          {t('cancel')}
        </button>
        {validationMsg != null && (
          <span
            className="thm-text-error f6 ml2"
            style={{
              opacity: errorFadingOut ? 0 : 1,
              transition: errorFadingOut ? 'opacity 5s' : 'none',
            }}
          >
            {validationMsg}
          </span>
        )}
        {confirmDelete && (
          <span className="f6 ml2">
            <span className="mr2">
              {selectionCount === 1
                ? t('bulkEdit.destroyOne')
                : t('bulkEdit.destroyMany', { count: selectionCount })}
            </span>
            <span className="nowrap">
              <button
                type="button"
                onClick={() => {
                  setConfirmDelete(false);
                }}
              >
                {t('cancel')}
              </button>
              {' / '}
              <button type="submit" className="thm-text-error">
                {t('destroy')}
              </button>
            </span>
          </span>
        )}
        {confirmPublic && (
          <span className="f6 ml2">
            <span className="mr2">
              {selectionCount === 1
                ? t('bulkEdit.publicOne')
                : t('bulkEdit.publicMany', { count: selectionCount })}
            </span>
            <span className="nowrap">
              <button
                type="button"
                onClick={() => {
                  setConfirmPublic(false);
                }}
              >
                {t('cancel')}
              </button>
              {' / '}
              <button type="submit" className="thm-text-error">
                {t('bulkEdit.makePublic')}
              </button>
            </span>
          </span>
        )}
      </div>
    </form>
  );
}

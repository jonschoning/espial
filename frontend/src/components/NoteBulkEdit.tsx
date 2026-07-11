import { TimeoutError } from 'ky';
import React, { useEffect, useRef, useState } from 'react';
import { useTranslation } from 'react-i18next';

import { apiErrorMsg } from '@/util';

import * as api from '../api';
import { app } from '../globals';
import type { NoteBulkAction } from '../types';

type BulkSelection = 'page' | 'all';

interface Props {
  ncount: number;
}

type BulkValidationKey = 'bulkEditNotes.pleaseAddSelection' | 'bulkEditNotes.pleaseChooseAction';

function disabledReason(
  selection: BulkSelection | null,
  action: NoteBulkAction | null,
): BulkValidationKey | null {
  if (selection === null) return 'bulkEditNotes.pleaseAddSelection';
  if (!action) return 'bulkEditNotes.pleaseChooseAction';
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

export function NoteBulkEdit({ ncount }: Props) {
  const { t } = useTranslation();
  const [visible, setVisible] = useState(false);
  const [selection, setSelection] = useState<BulkSelection | null>(null);
  const [action, setAction] = useState<NoteBulkAction | null>(null);
  const [submitting, setSubmitting] = useState(false);
  const [confirmDelete, setConfirmDelete] = useState(false);
  const [confirmPublic, setConfirmPublic] = useState(false);
  const [validationMsg, setValidationMsg] = useState<string | null>(null);
  const [errorFadingOut, setErrorFadingOut] = useState(false);
  const validationTimer = useRef<number | null>(null);
  const fadeTimer = useRef<number | null>(null);

  const a = app();
  const bulkQuery = a.dat.query ?? null;

  const nids = (a.dat.notes ?? []).map((n) => n.id);
  const pageCount = nids.length;
  const selectionCount = selection === null ? 0 : selection === 'page' ? pageCount : ncount;

  const reason = disabledReason(selection, action);
  const isVisuallyDisabled = submitting || reason !== null || confirmDelete || confirmPublic;

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

    const r = disabledReason(selection, action);
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
      if (selection === null || action === null) return;
      const res = await api.noteBulkEdit(
        selection === 'page'
          ? {
              selection: 'page',
              nids,
              action,
              selectionCount,
            }
          : {
              selection: 'all',
              query: bulkQuery,
              action,
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

  const toggleAction = (v: NoteBulkAction) => {
    setAction((cur) => (cur === v ? null : v));
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
                ? t('bulkEditNotes.oneNoteSelected')
                : t('bulkEditNotes.notesSelected', { count: selectionCount })}
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
          {navLink(action, 'markdown', t('bulkEditNotes.useMarkdown'), toggleAction)}
          {' ‧ '}
          {navLink(action, 'plaintext', t('bulkEditNotes.usePlaintext'), toggleAction)}
          {' ‧ '}
          {navLink(action, 'delete', t('delete'), toggleAction)}
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
                ? t('bulkEditNotes.destroyOne')
                : t('bulkEditNotes.destroyMany', { count: selectionCount })}
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
                ? t('bulkEditNotes.publicOne')
                : t('bulkEditNotes.publicMany', { count: selectionCount })}
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

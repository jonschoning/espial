import React from 'react';
import { useTranslation } from 'react-i18next';

import { destroyNote, editNote } from '../api';
import { app, closeWindow, fromNow, setFocus, shdatetime } from '../globals';
import type { Note } from '../types';
import { curQuerystring, fromNullableStr, lookupQueryStringValue } from '../util';
import { Markdown } from './Markdown';

function toTextarea(input: string) {
  const lines = input.split('\n');
  return (
    <>
      {lines[0]}
      {lines.slice(1).map((x, idx) => (
        <React.Fragment key={idx}>
          <br />
          {x}
        </React.Fragment>
      ))}
    </>
  );
}

/** Displays a single note with inline editing and deletion, used in the popup window. */
export function NNote({ initial }: { initial: Note }) {
  const { t } = useTranslation();
  const a = app();

  const [note, setNote] = React.useState<Note>(initial);
  const [editNoteState, setEditNoteState] = React.useState<Note>(initial);
  const [deleteAsk, setDeleteAsk] = React.useState(false);
  const [edit, setEdit] = React.useState(initial.id <= 0);
  const [destroyed, setDestroyed] = React.useState(false);
  const [apiError, setApiError] = React.useState<string | null>(null);
  const fromNowVal = fromNow(a.lang, note.created);

  const notetextid = `${note.id.toString()}_text`;

  function startEdit(next: boolean) {
    setEditNoteState(note);
    setEdit(next);
    const qs = curQuerystring();
    const q = lookupQueryStringValue(qs, 'next');
    if (!next && q === 'closeWindow') closeWindow(window);
    else if (next) setFocus(notetextid);
  }

  async function onDestroy() {
    await destroyNote(note.id);
    setDestroyed(true);
  }

  const onSubmit = async (e: React.SyntheticEvent<HTMLFormElement, SubmitEvent>): Promise<void> => {
    e.preventDefault();
    setApiError(null);

    const res = await editNote(editNoteState);
    if (res.ok && res.status >= 200 && res.status < 300) {
      const qs = curQuerystring();
      const next = lookupQueryStringValue(qs, 'next');
      const ref = document.referrer;
      const org = window.location.origin;

      if (next === 'closeWindow') closeWindow(window);
      else if (next === 'back') {
        if (ref.startsWith(org)) window.location.href = ref;
        else window.location.href = org;
      } else if (editNoteState.id === 0) {
        window.location.href = fromNullableStr(a.noteR);
      } else {
        setNote(editNoteState);
        setEdit(false);
      }
    } else {
      setApiError(res.bodyText);
      // eslint-disable-next-line no-console
      console.log(res.bodyText);
    }
  };

  if (destroyed) return <p className="thm-text-error">{t('note.killed')}</p>;

  return (
    <div id={String(note.id)} className="note w-100 mw7 pa1 mb2">
      {!edit ? (
        <>
          <div className="display">
            <div className="link f5 lh-title">{note.title === '' ? t('noTitle') : note.title}</div>
            <br />
            {note.isMarkdown ? (
              <div className="description mt1">
                <Markdown text={note.text} />
              </div>
            ) : (
              <div className="description mt1 thm-text-secondary">{toTextarea(note.text)}</div>
            )}
            <div className="link f7 mt2 dib thm-text-tertiary w4">
              <span data-created={note.created} title={shdatetime(a.lang, note.created)}>
                {fromNowVal}
              </span>
              {' - '}
              <span className="thm-text-tertiary">
                {note.shared ? t('filter.public') : t('filter.private')}
              </span>
            </div>
          </div>

          {a.dat.isowner ? (
            <div className="edit_links db mt3">
              <button
                type="button"
                onClick={() => {
                  startEdit(true);
                }}
                className="edit thm-text-muted thm-hover-link-color"
              >
                {t('edit')}&nbsp;&nbsp;
              </button>
              <div className="delete_link di">
                <button
                  type="button"
                  onClick={() => {
                    setDeleteAsk(true);
                  }}
                  className={`delete thm-text-muted thm-hover-link-color${deleteAsk ? ' dn' : ''}`}
                >
                  {t('delete')}
                </button>
                <span className={`confirm thm-text-error${!deleteAsk ? ' dn' : ''}`}>
                  <button
                    type="button"
                    onClick={() => {
                      setDeleteAsk(false);
                    }}
                  >
                    {t('cancel')}&nbsp;/&nbsp;
                  </button>
                  <button type="button" onClick={() => void onDestroy()} className="thm-text-error">
                    {t('destroy')}
                  </button>
                </span>
              </div>
            </div>
          ) : null}
        </>
      ) : (
        <form
          onSubmit={(e) => {
            void onSubmit(e);
          }}
        >
          {apiError ? <div className="alert alert-err">{apiError}</div> : null}
          <p className="mt2 mb1">{t('note.titleLabel')}</p>
          <input
            type="text"
            className="title w-100 mb1 pt1 edit_form_input"
            name="title"
            value={editNoteState.title}
            onChange={(e) => {
              setEditNoteState((x) => ({ ...x, title: e.target.value }));
            }}
            autoFocus={editNoteState.title === ''}
          />
          <br />
          <p className="mt2 mb1">{t('note.descriptionLabel')}</p>
          <textarea
            id={notetextid}
            className="description w-100 mb1 pt1 edit_form_input"
            name="text"
            rows={25}
            value={editNoteState.text}
            onChange={(e) => {
              setEditNoteState((x) => ({ ...x, text: e.target.value }));
            }}
          />
          <div className="edit_form_checkboxes mb3">
            <input
              type="checkbox"
              className="is-markdown pointer"
              id="edit_ismarkdown"
              name="ismarkdown"
              checked={editNoteState.isMarkdown}
              onChange={(e) => {
                setEditNoteState((x) => ({ ...x, isMarkdown: e.target.checked }));
              }}
            />{' '}
            <label htmlFor="edit_ismarkdown" className="mr2">
              {t('note.useMarkdown')}
            </label>
            <br />
          </div>
          <div className="edit_form_checkboxes mb3">
            <input
              type="checkbox"
              className="is-markdown pointer"
              id="edit_shared"
              name="shared"
              checked={editNoteState.shared}
              onChange={(e) => {
                setEditNoteState((x) => ({ ...x, shared: e.target.checked }));
              }}
            />{' '}
            <label htmlFor="edit_shared" className="mr2">
              {t('note.publicLabel')}
            </label>
            <br />
          </div>
          <input
            type="submit"
            className="mr1 pv1 ph2 thm-text-primary ba thm-border-default thm-bg-secondary pointer rdim"
            value={t('save')}
          />{' '}
          <input
            type="reset"
            className="pv1 ph2 thm-text-primary ba thm-border-default thm-bg-secondary pointer rdim"
            value={t('cancel')}
            onClick={() => {
              startEdit(false);
            }}
          />
        </form>
      )}
    </div>
  );
}

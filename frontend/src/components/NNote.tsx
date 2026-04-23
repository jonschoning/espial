import React from 'react';

import { destroyNote, editNote } from '../api';
import { app, closeWindow, mmoment8601, setFocus } from '../globals';
import type { Note } from '../types';
import { curQuerystring, fromNullableStr, lookupQueryStringValue } from '../util';
import { Markdown } from './Markdown';

function toTextarea(input: string) {
  const lines = input.split('\n');
  return (
    <>
      {lines.slice(1).map((x, idx) => (
        <React.Fragment key={idx}>
          <br />
          {x}
        </React.Fragment>
      ))}
    </>
  );
}

export function NNote({ initial }: { initial: Note }) {
  const a = app();

  const [note, setNote] = React.useState<Note>(initial);
  const [editNoteState, setEditNoteState] = React.useState<Note>(initial);
  const [deleteAsk, setDeleteAsk] = React.useState(false);
  const [edit, setEdit] = React.useState(initial.id <= 0);
  const [destroyed, setDestroyed] = React.useState(false);
  const [apiError, setApiError] = React.useState<string | null>(null);

  const notetextid = `${note.id.toString()}_text`;
  const mm = mmoment8601(note.created);

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

  if (destroyed) return <p className="red">you killed this note</p>;

  return (
    <div id={String(note.id)} className="note w-100 mw7 pa1 mb2">
      {!edit ? (
        <>
          <div className="display">
            <div className="link f5 lh-title">{note.title === '' ? '[no title]' : note.title}</div>
            <br />
            {note.isMarkdown ? (
              <div className="description mt1">
                <Markdown text={note.text} />
              </div>
            ) : (
              <div className="description mt1 mid-gray">{toTextarea(note.text)}</div>
            )}
            <div className="link f7 dib gray w4">
              <span title={mm?.[1] ?? note.created}>{mm?.[0] ?? '\u00a0'}</span>
              {' - '}
              <span className="gray">{note.shared ? 'public' : 'private'}</span>
            </div>
          </div>

          {a.dat.isowner ? (
            <div className="edit_links db mt3">
              <button
                type="button"
                onClick={() => {
                  startEdit(true);
                }}
                className="edit light-silver hover-blue"
              >
                edit&nbsp;&nbsp;
              </button>
              <div className="delete_link di">
                <button
                  type="button"
                  onClick={() => {
                    setDeleteAsk(true);
                  }}
                  className={`delete light-silver hover-blue${deleteAsk ? ' dn' : ''}`}
                >
                  delete
                </button>
                <span className={`confirm red${!deleteAsk ? ' dn' : ''}`}>
                  <button
                    type="button"
                    onClick={() => {
                      setDeleteAsk(false);
                    }}
                  >
                    cancel&nbsp;/&nbsp;
                  </button>
                  <button type="button" onClick={() => void onDestroy()} className="red">
                    destroy
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
          <p className="mt2 mb1">title:</p>
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
          <p className="mt2 mb1">description:</p>
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
              use markdown?
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
              public?
            </label>
            <br />
          </div>
          <input
            type="submit"
            className="mr1 pv1 ph2 dark-gray ba b--moon-gray bg-near-white pointer rdim"
            value="save"
          />{' '}
          <input
            type="reset"
            className="pv1 ph2 dark-gray ba b--moon-gray bg-near-white pointer rdim"
            value="cancel"
            onClick={() => {
              startEdit(false);
            }}
          />
        </form>
      )}
    </div>
  );
}

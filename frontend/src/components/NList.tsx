import React from 'react';
import { useTranslation } from 'react-i18next';

import { app, fromNow, shdatetime } from '../globals';
import type { Note } from '../types';
import { fromNullableStr } from '../util';

function toTextareaPreview(input: string) {
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

/** Renders a list of note summaries with links to individual notes. */
export function NList({ initial }: { initial: Note[] }) {
  const { t } = useTranslation();
  const a = app();
  const linkToFilterSingle = (slug: string) => `${fromNullableStr(a.userR)}/notes/${slug}`;

  return (
    <div>
      {initial.map((note) => {
        const fromNowVal = React.useMemo(
          () => fromNow(a.lang, note.created),
          [a.lang, note.created],
        );
        return (
          <div
            key={note.id}
            id={String(note.id)}
            className={`note w-100 mw7 pa1 mb2${note.shared ? '' : ' private'}`}
          >
            <div className="display">
              <a href={linkToFilterSingle(note.slug)} className="link f5 lh-title">
                {note.title === '' ? t('noTitle') : note.title}
              </a>
              <br />
              <div className="description mt1 thm-text-secondary">
                {toTextareaPreview(note.text.slice(0, 200))}
              </div>
              <a
                className="link f7 dib thm-text-tertiary w4"
                data-created={note.created}
                title={shdatetime(a.lang, note.created)}
                href={linkToFilterSingle(note.slug)}
              >
                {fromNowVal}
              </a>
            </div>
          </div>
        );
      })}
    </div>
  );
}

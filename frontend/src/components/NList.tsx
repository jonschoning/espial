import React from 'react';

import { app, mmoment8601 } from '../globals';
import type { Note } from '../types';
import { fromNullableStr } from '../util';

function toTextareaPreview(input: string) {
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

export function NList({ initial }: { initial: Note[] }) {
  const a = app();
  const linkToFilterSingle = (slug: string) => `${fromNullableStr(a.userR)}/notes/${slug}`;

  return (
    <div>
      {initial.map((note) => {
        const mm = mmoment8601(note.created);
        return (
          <div
            key={note.id}
            id={String(note.id)}
            className={`note w-100 mw7 pa1 mb2${note.shared ? '' : ' private'}`}
          >
            <div className="display">
              <a href={linkToFilterSingle(note.slug)} className="link f5 lh-title">
                {note.title === '' ? '[no title]' : note.title}
              </a>
              <br />
              <div className="description mt1 mid-gray">
                {toTextareaPreview(note.text.slice(0, 200))}
              </div>
              <a
                className="link f7 dib gray w4"
                title={mm?.[1] ?? note.created}
                href={linkToFilterSingle(note.slug)}
              >
                {mm?.[0] ?? '\u00a0'}
              </a>
            </div>
          </div>
        );
      })}
    </div>
  );
}

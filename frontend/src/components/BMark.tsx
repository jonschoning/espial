import React from 'react';

import { destroy, editBookmark, lookupTitle, markRead, toggleStar } from '../api';
import { app, setFocus, toLocaleDateString } from '../globals';
import type { Bookmark } from '../types';
import { encodeTag, fromNullableStr } from '../util';
import { Markdown } from './Markdown';

export function BMark({
  initial,
  onNotifyRemove,
  onUpdated,
}: {
  initial: Bookmark;
  onNotifyRemove: () => void;
  onUpdated: (bm: Bookmark) => void;
}) {
  const a = app();
  const [bm, setBm] = React.useState<Bookmark>(initial);
  const [editBm, setEditBm] = React.useState<Bookmark>(initial);
  const [deleteAsk, setDeleteAsk] = React.useState(false);
  const [edit, setEdit] = React.useState(false);
  const [loading, setLoading] = React.useState(false);
  const [apiError, setApiError] = React.useState<string | null>(null);

  const tagInputId = `${bm.bid.toString()}_tags`;

  const linkToFilterSingle = (slug: string) => `${fromNullableStr(a.userR)}/b:${slug}`;
  const linkToFilterTag = (tag: string) => `${fromNullableStr(a.userR)}/t:${encodeTag(tag)}`;

  const shdate = toLocaleDateString(bm.time);
  const shdatetime = `${bm.time.slice(0, 16)}Z`;

  async function onStar(next: boolean) {
    await toggleStar(bm.bid, next ? 'star' : 'unstar');
    const updated = { ...bm, selected: next };
    setBm(updated);
    setEditBm((x) => ({ ...x, selected: next }));
    onUpdated(updated);
  }

  async function onDestroy() {
    await destroy(bm.bid);
    onNotifyRemove();
  }

  async function onMarkRead() {
    await markRead(bm.bid);
    const updated = { ...bm, toread: false };
    setBm(updated);
    onUpdated(updated);
  }

  function startEdit(next: boolean) {
    setEditBm(bm);
    setEdit(next);
    setApiError(null);
    if (next) setFocus(tagInputId);
  }

  async function onFetchTitle() {
    setLoading(true);
    try {
      const title = await lookupTitle(editBm);
      if (title != null) setEditBm((x) => ({ ...x, title }));
    } finally {
      setLoading(false);
    }
  }

  const onSubmit = async (e: React.SyntheticEvent<HTMLFormElement, SubmitEvent>): Promise<void> => {
    e.preventDefault();
    setApiError(null);
    const editBm2 = { ...editBm, tags: editBm.tags.replace(/,/g, ' ') };

    const res = await editBookmark(editBm2);
    if (res.ok && res.status >= 200 && res.status < 300) {
      setBm(editBm2);
      setEdit(false);
      onUpdated(editBm2);
    } else {
      setApiError(res.bodyText);
      // match PS behavior: log error
      // eslint-disable-next-line no-console
      console.log(res.bodyText);
    }
  };

  return (
    <div
      id={String(bm.bid)}
      className={`bookmark w-100 mw7 pa1 mb3${bm.private ? ' private' : ''}`}
    >
      {a.dat.isowner ? (
        <div className={`star fl pointer${bm.selected ? ' selected' : ''}`}>
          <button className="moon-gray" onClick={() => void onStar(!bm.selected)} type="button">
            ✭
          </button>
        </div>
      ) : null}

      {!edit ? (
        <div className="display">
          <a
            href={bm.url}
            target="_blank"
            rel="noreferrer"
            className={`link f5 lh-title${bm.toread ? ' unread' : ''}`}
          >
            {bm.title === '' ? '[no title]' : bm.title}
          </a>
          <br />
          <a href={bm.url} className="link f7 gray hover-blue">
            {bm.url}
          </a>
          <a
            href={bm.archiveUrl ?? `http://archive.is/${bm.url}`}
            className={`link f7 gray hover-blue ml2${bm.archiveUrl ? ' green' : ''}`}
            target="_blank"
            rel="noreferrer"
            title="archive link"
          >
            {bm.archiveUrl ? '☑' : '☐'}
          </a>
          <br />
          <div className="description mt1 mid-gray">
            <Markdown text={bm.description} />
          </div>
          <div className="tags">
            {bm.tags !== ''
              ? bm.tags.split(' ').map((tag) => (
                  <a
                    key={tag}
                    className={`link tag mr1${tag.slice(0, 1) === '.' ? ' private' : ''}`}
                    href={linkToFilterTag(tag)}
                  >
                    {tag}
                  </a>
                ))
              : null}
          </div>

          <a className="link f7 dib gray w4" href={linkToFilterSingle(bm.slug)} title={shdatetime}>
            {shdate}
          </a>

          {a.dat.isowner ? (
            <div className="edit_links di">
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

          {a.dat.isowner && bm.toread ? (
            <div className="read di">
              &nbsp;&nbsp;
              <button onClick={() => void onMarkRead()} className="mark_read" type="button">
                mark as read
              </button>
            </div>
          ) : null}
        </div>
      ) : (
        <div className="edit_bookmark_form pa2 pt0 bg-white">
          {apiError ? <div className="alert alert-err">{apiError}</div> : null}
          <form onSubmit={(e) => void onSubmit(e)}>
            <div>url</div>
            <input
              type="url"
              className="url w-100 mb2 pt1 edit_form_input"
              required
              name="url"
              value={editBm.url}
              onChange={(e) => {
                setEditBm((x) => ({ ...x, url: e.target.value }));
              }}
            />
            <div>title</div>
            <div className="flex">
              <input
                type="text"
                className="title w-100 mb2 pt1 edit_form_input"
                name="title"
                value={editBm.title}
                onChange={(e) => {
                  setEditBm((x) => ({ ...x, title: e.target.value }));
                }}
              />
              <button
                disabled={loading}
                type="button"
                onClick={() => void onFetchTitle()}
                className={`ml1 pa1 mb2 dark-gray ba b--moon-gray bg-near-white pointer rdim f7${
                  loading ? 'bg-light-silver' : ''
                }`}
              >
                fetch
              </button>
            </div>
            <div>description</div>
            <textarea
              className="description w-100 mb1 pt1 edit_form_input"
              name="description"
              rows={5}
              value={editBm.description}
              onChange={(e) => {
                setEditBm((x) => ({ ...x, description: e.target.value }));
              }}
            />
            <div id="tags_input_box">
              <div>tags</div>
              <input
                id={tagInputId}
                type="text"
                className="tags w-100 mb1 pt1 edit_form_input"
                name="tags"
                autoComplete="off"
                autoCapitalize="off"
                value={editBm.tags}
                onChange={(e) => {
                  setEditBm((x) => ({ ...x, tags: e.target.value }));
                }}
              />
            </div>
            <div className="edit_form_checkboxes mv3">
              <input
                type="checkbox"
                className="private pointer"
                id="edit_private"
                name="private"
                checked={editBm.private}
                onChange={(e) => {
                  setEditBm((x) => ({ ...x, private: e.target.checked }));
                }}
              />{' '}
              <label htmlFor="edit_private" className="mr2">
                private
              </label>{' '}
              <input
                type="checkbox"
                className="toread pointer"
                id="edit_toread"
                name="toread"
                checked={editBm.toread}
                onChange={(e) => {
                  setEditBm((x) => ({ ...x, toread: e.target.checked }));
                }}
              />{' '}
              <label htmlFor="edit_toread">to-read</label>
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
        </div>
      )}
    </div>
  );
}

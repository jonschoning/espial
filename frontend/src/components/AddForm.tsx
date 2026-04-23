import React from 'react';

import { destroy, editBookmark, lookupTitle } from '../api';
import { closeWindow, mmoment8601 } from '../globals';
import type { Bookmark } from '../types';
import { curQuerystring, lookupQueryStringValue } from '../util';

export function AddForm({ initial }: { initial: Bookmark }) {
  const [bm, setBm] = React.useState<Bookmark>(initial);
  const [editBm, setEditBm] = React.useState<Bookmark>(initial);
  const [deleteAsk, setDeleteAsk] = React.useState(false);
  const [loading, setLoading] = React.useState(false);
  const [destroyed, setDestroyed] = React.useState(false);
  const [apiError, setApiError] = React.useState<string | null>(null);

  const mm = mmoment8601(bm.time);

  async function onFetchTitle() {
    setLoading(true);
    try {
      const title = await lookupTitle(editBm);
      if (title != null) setEditBm((x) => ({ ...x, title }));
    } finally {
      setLoading(false);
    }
  }

  async function onDestroy() {
    await destroy(bm.bid);
    setDestroyed(true);
  }

  const onSubmit = async (e: React.SyntheticEvent<HTMLFormElement, SubmitEvent>): Promise<void> => {
    e.preventDefault();
    setApiError(null);
    const res = await editBookmark(editBm);
    if (res.ok && res.status >= 200 && res.status < 300) {
      setBm(editBm);

      const qs = curQuerystring();
      const next = lookupQueryStringValue(qs, 'next');
      const ref = document.referrer;
      const org = window.location.origin;

      if (next === 'closeWindow') closeWindow(window);
      else if (next === 'back') {
        if (ref.startsWith(org)) window.location.href = ref;
        else window.location.href = org;
      } else {
        closeWindow(window);
      }
    } else {
      setApiError(res.bodyText);
    }
  };

  if (destroyed) return <p className="red">you killed this bookmark</p>;

  return (
    <form
      onSubmit={(e) => {
        void onSubmit(e);
      }}
    >
      <table className="w-100">
        <tbody>
          <tr>
            <td className="w1" />
            <td>
              {bm.bid > 0 ? (
                <div className="alert">
                  previously saved&nbsp;
                  <span className="link f7 dib gray pr3" title={mm?.[1] ?? bm.time}>
                    {mm?.[0] ?? '\u00a0'}
                  </span>
                  <div className="edit_links dib ml1">
                    <div className="delete_link di">
                      <button
                        type="button"
                        onClick={() => {
                          setDeleteAsk(true);
                        }}
                        className="delete"
                        hidden={deleteAsk}
                      >
                        delete
                      </button>
                      <span className="confirm red" hidden={!deleteAsk}>
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
                </div>
              ) : null}
              {apiError ? <div className="alert alert-err">{apiError}</div> : null}
            </td>
          </tr>

          <tr>
            <td>
              <label htmlFor="url">URL</label>
            </td>
            <td>
              <input
                type="url"
                id="url"
                className="w-100 mv1"
                required
                name="url"
                autoFocus={bm.url === ''}
                value={editBm.url}
                onChange={(e) => {
                  setEditBm((x) => ({ ...x, url: e.target.value }));
                }}
              />
            </td>
          </tr>

          <tr>
            <td>
              <label htmlFor="title">title</label>
            </td>
            <td className="flex">
              <input
                type="text"
                id="title"
                className="w-100 mv1 flex-auto"
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
                className={`ml2 input-reset ba b--navy pointer f6 di dim pa1 ma1 mr0${loading ? 'bg-light-silver' : ''}`}
              >
                fetch
              </button>
            </td>
          </tr>

          <tr>
            <td>
              <label htmlFor="description">description</label>
            </td>
            <td>
              <textarea
                className="w-100 mt1 mid-gray"
                id="description"
                name="description"
                rows={4}
                value={editBm.description}
                onChange={(e) => {
                  setEditBm((x) => ({ ...x, description: e.target.value }));
                }}
              />
            </td>
          </tr>

          <tr>
            <td>
              <label htmlFor="tags">tags</label>
            </td>
            <td>
              <input
                type="text"
                id="tags"
                className="w-100 mv1"
                name="tags"
                autoComplete="off"
                autoCapitalize="off"
                autoFocus={bm.url !== ''}
                value={editBm.tags}
                onChange={(e) => {
                  setEditBm((x) => ({ ...x, tags: e.target.value }));
                }}
              />
            </td>
          </tr>

          <tr>
            <td>
              <label htmlFor="private">private</label>
            </td>
            <td>
              <input
                type="checkbox"
                id="private"
                className="private pointer"
                name="private"
                checked={editBm.private}
                onChange={(e) => {
                  setEditBm((x) => ({ ...x, private: e.target.checked }));
                }}
              />
            </td>
          </tr>

          <tr>
            <td>
              <label htmlFor="toread">read later</label>
            </td>
            <td>
              <input
                type="checkbox"
                id="toread"
                className="toread pointer"
                name="toread"
                checked={editBm.toread}
                onChange={(e) => {
                  setEditBm((x) => ({ ...x, toread: e.target.checked }));
                }}
              />
            </td>
          </tr>

          <tr>
            <td />
            <td>
              <input
                type="submit"
                className="ph3 pv2 input-reset black ba b--navy bg-transparent pointer f6 dib mt1 dim"
                value={bm.bid > 0 ? 'update bookmark' : 'add bookmark'}
              />
            </td>
          </tr>
        </tbody>
      </table>
    </form>
  );
}

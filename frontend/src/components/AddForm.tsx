import React from 'react';
import { useTranslation } from 'react-i18next';

import { destroy, editBookmark, lookupTitle } from '../api';
import { app, closeWindow, fromNow, shdatetime } from '../globals';
import { useTagSuggestions } from '../hooks/useTagSuggestions';
import type { Bookmark } from '../types';
import { curQuerystring, lookupQueryStringValue, normalizeTags } from '../util';
import { TagSuggestionsDropdown } from './TagSuggestionsDropdown';

/** Form for adding or editing a single bookmark, used in the popup window. */
export function AddForm({ initial }: { initial: Bookmark }) {
  const { t } = useTranslation();
  const a = app();
  const [bm, setBm] = React.useState<Bookmark>(initial);
  const [editBm, setEditBm] = React.useState<Bookmark>(initial);
  const [deleteAsk, setDeleteAsk] = React.useState(false);
  const [loading, setLoading] = React.useState(false);
  const [destroyed, setDestroyed] = React.useState(false);
  const [apiError, setApiError] = React.useState<string | null>(null);
  const suggestEnabled = a.dat.suggestTags === true;
  const fromNowVal = React.useMemo(() => fromNow(a.lang, bm.time), [a.lang, bm.time]);

  const {
    tagInputRef,
    suggestionState,
    closeSuggestions,
    onTagsChange,
    onTagsKeyDown,
    onTagsSelect,
    onSuggestionHover,
    onSuggestionPick,
  } = useTagSuggestions({
    enabled: suggestEnabled,
    tags: editBm.tags,
    maxSuggestions: 4,
    onTagsUpdate: (tags) => {
      setEditBm((x) => ({ ...x, tags }));
    },
  });

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
    const res = await destroy(bm.bid);
    if (!res.ok) {
      setApiError(res.bodyText);
      return;
    }
    setDestroyed(true);
  }

  const onSubmit = async (e: React.SyntheticEvent<HTMLFormElement, SubmitEvent>): Promise<void> => {
    e.preventDefault();
    setApiError(null);
    const editBm2 = { ...editBm, tags: normalizeTags(editBm.tags) };

    const res = await editBookmark(editBm2);
    if (res.ok && res.status >= 200 && res.status < 300) {
      setBm(editBm2);

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

  if (destroyed) return <p className="thm-text-killed">{t('addForm.killed')}</p>;

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
                  {t('addForm.previouslySaved')}&nbsp;
                  <span
                    className="link f7 dib thm-text-tertiary thm-hover-link-color pr3"
                    data-time={bm.time}
                    title={shdatetime(a.lang, bm.time)}
                  >
                    {fromNowVal}
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
                        {t('delete')}
                      </button>
                      <span className="confirm thm-text-error" hidden={!deleteAsk}>
                        <button
                          type="button"
                          onClick={() => {
                            setDeleteAsk(false);
                          }}
                        >
                          {t('cancel')}&nbsp;/&nbsp;
                        </button>
                        <button
                          type="button"
                          onClick={() => void onDestroy()}
                          className="thm-text-error"
                        >
                          {t('destroy')}
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
              <label htmlFor="url">{t('addForm.url')}</label>
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
              <label htmlFor="title">{t('bmark.title')}</label>
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
                className={`ml2 input-reset ba thm-border-primary pointer f6 di dim pa1 ma1 mr0${loading ? ' thm-bg-disabled' : ''}`}
              >
                {t('fetch')}
              </button>
            </td>
          </tr>

          <tr>
            <td>
              <label htmlFor="description">{t('bmark.description')}</label>
            </td>
            <td>
              <textarea
                className="w-100 mt1 thm-text-secondary"
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
              <label htmlFor="tags">{t('bmark.tags')}</label>
            </td>
            <td>
              <div className="relative">
                <input
                  ref={tagInputRef}
                  type="text"
                  id="tags"
                  className="w-100 mv1"
                  name="tags"
                  autoComplete="off"
                  autoCapitalize="off"
                  autoFocus={bm.url !== ''}
                  value={editBm.tags}
                  onChange={onTagsChange}
                  onKeyDown={onTagsKeyDown}
                  onClick={onTagsSelect}
                  onSelect={onTagsSelect}
                  onBlur={() => {
                    window.setTimeout(() => {
                      closeSuggestions();
                    }, 0);
                  }}
                />
                {suggestionState != null ? (
                  <TagSuggestionsDropdown
                    suggestionState={suggestionState}
                    onHover={onSuggestionHover}
                    onPick={onSuggestionPick}
                  />
                ) : null}
              </div>
            </td>
          </tr>

          <tr>
            <td>
              <label htmlFor="private">{t('bmark.private')}</label>
            </td>
            <td>
              <div className="mr4 dib">
                <input
                  type="checkbox"
                  id="private"
                  className="private pointer"
                  name="private"
                  checked={editBm.private}
                  onChange={(e) => {
                    setEditBm((x) => ({
                      ...x,
                      private: e.target.checked,
                      ...(a.dat.archiveBackendEnabled && editBm.archiveRequested && e.target.checked
                        ? { archiveRequested: false }
                        : {}),
                    }));
                  }}
                />
              </div>
              {!(bm.bid > 0) && a.dat.archiveBackendEnabled ? (
                <div
                  className={`dib ${editBm.private ? 'o-50' : ''}`}
                  title={editBm.private ? t('addForm.archiveUnavailable') : undefined}
                >
                  <label className="mr2 di" htmlFor="archiveRequested">
                    {t('archive')}
                  </label>
                  <input
                    type="checkbox"
                    id="archiveRequested"
                    className="archive pointer ml2"
                    name="archiveRequested"
                    disabled={editBm.private}
                    checked={editBm.archiveRequested ?? false}
                    onChange={(e) => {
                      setEditBm((x) => ({ ...x, archiveRequested: e.target.checked }));
                    }}
                  />
                </div>
              ) : null}
            </td>
          </tr>

          <tr>
            <td>
              <label htmlFor="toread">{t('addForm.readLater')}</label>
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
                className="ph3 pv2 input-reset thm-text-strong ba thm-border-primary bg-transparent pointer f6 dib mt1 dim"
                value={bm.bid > 0 ? t('addForm.update') : t('addForm.add')}
              />
            </td>
          </tr>
        </tbody>
      </table>
    </form>
  );
}

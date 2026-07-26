import { TimeoutError } from 'ky';
import React from 'react';
import { useTranslation } from 'react-i18next';

import { archiveBookmark, destroy, editBookmark, lookupTitle, markRead, toggleStar } from '../api';
import { app, setFocus, shdatetime, toLocaleDateString } from '../globals';
import { useTagSuggestions } from '../hooks/useTagSuggestions';
import type { Bookmark } from '../types';
import { buildTagUrl } from '../urlBuild';
import { apiErrorMsg, fromNullableStr, normalizeTags } from '../util';
import { Markdown } from './Markdown';
import { TagSuggestionsDropdown } from './TagSuggestionsDropdown';

/** Displays a single bookmark with inline editing, starring, and deletion. */
export function BMark({
  initial,
  onNotifyRemove,
  onUpdated,
}: {
  initial: Bookmark;
  onNotifyRemove: () => void;
  onUpdated: (bm: Bookmark) => void;
}) {
  const { t } = useTranslation();
  const a = app();
  const [bm, setBm] = React.useState<Bookmark>(initial);
  const [editBm, setEditBm] = React.useState<Bookmark>(initial);
  const [deleteAsk, setDeleteAsk] = React.useState(false);
  const [edit, setEdit] = React.useState(false);
  const [loading, setLoading] = React.useState(false);
  const [archiving, setArchiving] = React.useState(false);
  const [archived, setArchived] = React.useState(false);
  const [actionError, setActionError] = React.useState<string | null>(null);
  const [errorFadingOut, setErrorFadingOut] = React.useState(false);
  const errorTimer = React.useRef<number | null>(null);
  const fadeTimer = React.useRef<number | null>(null);

  React.useEffect(() => {
    return () => {
      if (errorTimer.current !== null) window.clearTimeout(errorTimer.current);
      if (fadeTimer.current !== null) window.clearTimeout(fadeTimer.current);
    };
  }, []);

  function clearErrorTimers() {
    if (errorTimer.current !== null) {
      window.clearTimeout(errorTimer.current);
      errorTimer.current = null;
    }
    if (fadeTimer.current !== null) {
      window.clearTimeout(fadeTimer.current);
      fadeTimer.current = null;
    }
  }

  function showActionError(msg: string) {
    clearErrorTimers();
    setErrorFadingOut(false);
    setActionError(msg);
    errorTimer.current = window.setTimeout(() => {
      errorTimer.current = null;
      setErrorFadingOut(true);
      fadeTimer.current = window.setTimeout(() => {
        setActionError(null);
        setErrorFadingOut(false);
        fadeTimer.current = null;
      }, 5000);
    }, 3000);
  }

  const tagInputId = `${bm.bid.toString()}_tags`;

  const linkToFilterSingle = `${fromNullableStr(a.userR)}/b:${bm.slug}`;
  const linkToFilterTag = (tag: string) => buildTagUrl(a, [tag]);
  const viewInContextTime = (time: Date) => {
    const t = new Date(time);
    const t2 = new Date(t.getTime() + 2);
    return t2.toISOString();
  };
  const linkToViewInContext = `${fromNullableStr(a.userR)}?before=${encodeURIComponent(viewInContextTime(new Date(bm.time)))}`;

  const suggestEnabled = a.dat.suggestTags === true;
  const suggestUseReturnKey = a.dat.suggestTagsUseReturnKey !== false;

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
    useReturnKey: suggestUseReturnKey,
    tags: editBm.tags,
    onTagsUpdate: (tags) => {
      setEditBm((x) => ({ ...x, tags }));
    },
  });

  async function onStar(next: boolean) {
    try {
      const res = await toggleStar(bm.bid, next ? 'star' : 'unstar');
      if (!res.ok) {
        showActionError(apiErrorMsg(t, res.status, res.bodyText));
        return;
      }
      clearErrorTimers();
      setActionError(null);
      const updated = { ...bm, selected: next };
      setBm(updated);
      setEditBm((x) => ({ ...x, selected: next }));
      onUpdated(updated);
    } catch (err) {
      showActionError(
        err instanceof TimeoutError ? t('error.requestTimedOut') : t('error.networkError'),
      );
    }
  }

  async function onDestroy() {
    try {
      const res = await destroy(bm.bid);
      if (!res.ok) {
        showActionError(apiErrorMsg(t, res.status, res.bodyText));
        return;
      }
      onNotifyRemove();
    } catch (err) {
      showActionError(
        err instanceof TimeoutError ? t('error.requestTimedOut') : t('error.networkError'),
      );
    }
  }

  async function onMarkRead() {
    try {
      const res = await markRead(bm.bid);
      if (!res.ok) {
        showActionError(apiErrorMsg(t, res.status, res.bodyText));
        return;
      }
      clearErrorTimers();
      setActionError(null);
      const updated = { ...bm, toread: false };
      setBm(updated);
      onUpdated(updated);
    } catch (err) {
      showActionError(
        err instanceof TimeoutError ? t('error.requestTimedOut') : t('error.networkError'),
      );
    }
  }

  function startEdit(next: boolean) {
    setEditBm(bm);
    setEdit(next);
    clearErrorTimers();
    setActionError(null);
    setErrorFadingOut(false);
    setArchiving(false);
    setArchived(false);
    closeSuggestions();
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

  async function onArchive() {
    setArchiving(true);
    setArchived(true);
    try {
      const res = await archiveBookmark(bm.bid);
      if (!res.ok) {
        showActionError(apiErrorMsg(t, res.status, res.bodyText));
        return;
      }
    } finally {
      setArchiving(false);
    }
  }

  const onSubmit = async (e: React.SyntheticEvent<HTMLFormElement, SubmitEvent>): Promise<void> => {
    e.preventDefault();
    clearErrorTimers();
    setActionError(null);
    setErrorFadingOut(false);
    const editBm2 = { ...editBm, tags: normalizeTags(editBm.tags) };
    try {
      const res = await editBookmark(editBm2);
      if (res.ok && res.status >= 200 && res.status < 300) {
        setBm(editBm2);
        setEdit(false);
        onUpdated(editBm2);
      } else {
        showActionError(apiErrorMsg(t, res.status, res.bodyText));
      }
    } catch (err) {
      showActionError(
        err instanceof TimeoutError ? t('error.requestTimedOut') : t('error.networkError'),
      );
    }
  };

  const archivingDisabled = archiving || archived || editBm.private || bm.url !== editBm.url;

  return (
    <div
      id={String(bm.bid)}
      className={`bookmark w-100 mw7 pa1 mb3${bm.private ? ' private' : ''}`}
    >
      {a.dat.isowner ? (
        <div className={`star fl pointer${bm.selected ? ' selected' : ''}`}>
          <button
            className="thm-text-border"
            onClick={() => void onStar(!bm.selected)}
            type="button"
          >
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
            className={`link bmark-link f5 lh-title${bm.toread ? ' unread' : ''}`}
          >
            {bm.title === '' ? t('noTitle') : bm.title}
          </a>
          <br />
          <a href={bm.url} className="link bmark-link f7 thm-text-tertiary thm-hover-link-color">
            {bm.url}
          </a>
          {bm.archiveUrl ? (
            <a
              href={bm.archiveUrl}
              className="link f7 thm-text-tertiary thm-hover-link-color ml2 thm-text-success"
              target="_blank"
              rel="noreferrer"
              title={t('bmark.archiveLinkTitle')}
            >
              ☑
            </a>
          ) : null}
          <br />
          <div className="description mt1 thm-text-secondary">
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

          <div>
            <a
              className="link f7 di mr5 thm-text-tertiary"
              href={linkToFilterSingle}
              data-time={bm.time}
              title={shdatetime(a.lang, bm.time)}
            >
              {toLocaleDateString(a.lang, bm.time)}
            </a>

            {a.dat.isowner ? (
              <div className="edit_links di">
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
            ) : null}

            {a.dat.isowner && bm.toread ? (
              <div className="read di">
                &nbsp;&nbsp;
                <button onClick={() => void onMarkRead()} className="mark_read" type="button">
                  {t('bmark.markAsRead')}
                </button>
              </div>
            ) : null}
          </div>

          {a.dat.filter?.tag == 'FilterSingle' ? (
            <div className="mt2">
              <a className="link f7 di mr5" href={linkToViewInContext}>
                {t('bmark.viewInContext')}
              </a>
            </div>
          ) : null}
        </div>
      ) : null}
      {edit ? (
        <div className="edit_bookmark_form pa2 pt0 thm-bg-surface">
          <form onSubmit={(e) => void onSubmit(e)}>
            <div>{t('bmark.url')}</div>
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
            <div>{t('bmark.title')}</div>
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
                className={`ml1 pa1 mb2 thm-text-primary ba thm-border-default thm-bg-secondary pointer rdim f7${
                  loading ? ' thm-bg-disabled' : ''
                }`}
              >
                {t('fetch')}
              </button>
            </div>
            <div>{t('bmark.description')}</div>
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
              <div>{t('bmark.tags')}</div>
              <div className="relative">
                <input
                  id={tagInputId}
                  ref={tagInputRef}
                  type="text"
                  className="tags w-100 mb1 pt1 edit_form_input"
                  name="tags"
                  autoComplete="off"
                  autoCapitalize="off"
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
                {t('bmark.private')}
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
              <label htmlFor="edit_toread">{t('bmark.toread')}</label>
            </div>
            <div className="flex justify-between items-center">
              <div>
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
              </div>
              {a.dat.archiveBackendEnabled ? (
                <button
                  type="button"
                  disabled={archivingDisabled}
                  onClick={() => void onArchive()}
                  className={`pv1 ph2 thm-text-primary ba thm-border-default thm-bg-secondary${archiving ? ' thm-bg-disabled' : ''}${archivingDisabled ? ' not-allowed o-20' : ''}`}
                >
                  {archiving ? t('archiving') : t('archive')}
                </button>
              ) : null}
            </div>
          </form>
        </div>
      ) : null}

      {actionError != null ? (
        <span
          className="thm-text-error f7 db mt1"
          style={{
            opacity: errorFadingOut ? 0 : 1,
            transition: errorFadingOut ? 'opacity 5s' : 'none',
          }}
        >
          {actionError}
        </span>
      ) : null}
    </div>
  );
}

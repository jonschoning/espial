import { TimeoutError } from 'ky';
import React from 'react';
import { useTranslation } from 'react-i18next';

import * as api from '../api';
import { editAccountSettings } from '../api';
import type { AccountSettings } from '../types';
import { apiErrorMsg } from '../util';

type SettingsTab = 'account' | 'importexport';

/** Displays and edits the current user's account settings, with a sub-nav to import/export. */
export function AccountSettingsView({ initial }: { initial: AccountSettings }) {
  const { t } = useTranslation();
  const [tab, setTab] = React.useState<SettingsTab>('account');

  function tabLink(value: SettingsTab, label: string) {
    return (
      <a
        href="#"
        className={`dib link${tab === value ? ' nav-active' : ' silver hover-alt'}`}
        onClick={(e) => {
          e.preventDefault();
          setTab(value);
        }}
      >
        {label}
      </a>
    );
  }

  return (
    <div>
      <div className="mb3">
        {tabLink('account', t('settings.tabAccount'))}
        {' ‧ '}
        {tabLink('importexport', t('settings.tabImportExport'))}
      </div>

      <div className={tab === 'account' ? '' : 'dn'}>
        <SettingsView initial={initial} />
      </div>
      <div className={tab === 'importexport' ? '' : 'dn'}>
        <ImportExportView />
      </div>
    </div>
  );
}

function SettingsView({ initial }: { initial: AccountSettings }) {
  const { t } = useTranslation();
  const [us, setUs] = React.useState<AccountSettings>(initial);
  const [apiError, setApiError] = React.useState<string | null>(null);
  const archiveDisabled = !us.archiveBackendEnabled;
  const archiveDisabledTitle = archiveDisabled ? t('settings.archivingDisabled') : undefined;

  async function update(next: AccountSettings): Promise<boolean> {
    try {
      const res = await editAccountSettings(next);
      if (!res.ok) {
        setApiError(apiErrorMsg(t, res.status, res.bodyText));
        return false;
      }
      setApiError(null);
      setUs(next);
      const lockIcon = document.getElementById('privacy-lock-icon');
      if (lockIcon) lockIcon.style.display = next.privacyLock ? '' : 'none';
      return true;
    } catch (err) {
      setApiError(
        err instanceof TimeoutError ? t('error.requestTimedOut') : t('error.networkError'),
      );
      return false;
    }
  }

  return (
    <div className="settings-form">
      <div className="fw7 mb3">{t('settings.accountTitle')}</div>

      <div className="f7 fw7 ttu tracked thm-text-tertiary mb2">{t('settings.groupLanguage')}</div>
      <div className="flex items-center mb3">
        <select
          id="language"
          name="language"
          className="pointer"
          aria-label={t('settings.language')}
          value={us.language ?? ''}
          onChange={(e) => {
            const next = { ...us, language: e.target.value === '' ? null : e.target.value };
            void update(next).then((ok) => {
              if (ok) window.location.reload();
            });
          }}
        >
          <option value="">{t('settings.serverDefault')}</option>
          <option value="en">English</option>
          <option value="de">Deutsch</option>
          <option value="es">Español</option>
          <option value="fr">Français</option>
          <option value="it">Italiano</option>
          <option value="ja">日本語</option>
          <option value="ko">한국어</option>
          <option value="pl">Polski</option>
          <option value="pt-BR">Português (Brasil)</option>
          <option value="ru">Русский</option>
          <option value="tr">Türkçe</option>
          <option value="uk">Українська</option>
          <option value="zh-Hans">简体中文</option>
          <option value="zh-Hant">繁體中文</option>
        </select>
      </div>

      <div className="f7 fw7 ttu tracked thm-text-tertiary mb2">{t('settings.groupGeneral')}</div>
      <div className="flex items-center mb2">
        <input
          type="checkbox"
          className="pointer mr2"
          id="suggestTags"
          name="suggestTags"
          checked={us.suggestTags}
          onChange={(e) => void update({ ...us, suggestTags: e.target.checked })}
        />
        <label htmlFor="suggestTags" className="lh-copy">
          {t('settings.suggestTags')}
        </label>
      </div>

      <div className="flex items-center mb3">
        <input
          type="checkbox"
          className="pointer mr2"
          id="previewNotes"
          name="previewNotes"
          checked={us.previewNotes}
          onChange={(e) => void update({ ...us, previewNotes: e.target.checked })}
        />
        <label htmlFor="previewNotes" className="lh-copy">
          {t('settings.previewNotes')}
        </label>
      </div>

      <div className="f7 fw7 ttu tracked thm-text-tertiary mb2">{t('settings.groupArchive')}</div>
      <div
        className={`flex items-center mb3${archiveDisabled ? ' o-50' : ''}`}
        title={archiveDisabledTitle}
      >
        <input
          type="checkbox"
          className={`mr2${archiveDisabled ? '' : ' pointer'}`}
          id="archiveDefault"
          name="archiveDefault"
          checked={us.archiveDefault}
          disabled={archiveDisabled}
          title={archiveDisabledTitle}
          onChange={(e) => void update({ ...us, archiveDefault: e.target.checked })}
        />
        <label
          htmlFor="archiveDefault"
          className={`lh-copy${archiveDisabled ? ' thm-text-tertiary' : ''}`}
          title={archiveDisabledTitle}
        >
          {t('settings.archiveDefault')}
        </label>
      </div>

      <div className="f7 fw7 ttu tracked thm-text-tertiary mb2">{t('settings.groupPrivacy')}</div>
      <div className="flex items-center mb2">
        <input
          type="checkbox"
          className="pointer mr2"
          id="privateDefault"
          name="privateDefault"
          checked={us.privateDefault}
          onChange={(e) => void update({ ...us, privateDefault: e.target.checked })}
        />
        <label htmlFor="privateDefault" className="lh-copy">
          {t('settings.privateDefault')}
        </label>
      </div>

      <div className="flex items-center mb2">
        <input
          type="checkbox"
          className="pointer mr2"
          id="publicTagCloud"
          name="publicTagCloud"
          checked={us.publicTagCloud}
          onChange={(e) => void update({ ...us, publicTagCloud: e.target.checked })}
        />
        <label htmlFor="publicTagCloud" className="lh-copy">
          {t('settings.publicTagCloud')}
        </label>
      </div>

      <div className="flex items-center mb2">
        <input
          type="checkbox"
          className="pointer mr2"
          id="privacyLock"
          name="privacyLock"
          checked={us.privacyLock}
          onChange={(e) => void update({ ...us, privacyLock: e.target.checked })}
        />
        <label htmlFor="privacyLock" className="lh-copy">
          {t('settings.privacyLock')}
        </label>
      </div>

      {apiError ? <div className="thm-text-error f7 mt2">{apiError}</div> : null}
    </div>
  );
}

function ImportExportView() {
  const { t } = useTranslation();
  const [status, setStatus] = React.useState<string | null>(null);
  const [error, setError] = React.useState<string | null>(null);
  const [busy, setBusy] = React.useState(false);

  async function runImport(
    files: FileList | null,
    prepare: (texts: string[]) => string,
    importFn: (body: string) => Promise<api.ImportResult>,
  ): Promise<void> {
    if (!files || files.length === 0) return;
    setError(null);
    setStatus(t('settings.importing'));
    setBusy(true);
    try {
      const texts = await Promise.all(Array.from(files).map((f) => f.text()));
      const res = await importFn(prepare(texts));
      if (res.ok) {
        setStatus(t('settings.imported', { count: res.imported ?? 0 }));
      } else {
        setStatus(null);
        setError(apiErrorMsg(t, res.status, res.bodyText ?? ''));
      }
    } catch (err) {
      setStatus(null);
      setError(
        err instanceof TimeoutError ? t('error.requestTimedOut') : t('settings.importParseError'),
      );
    } finally {
      setBusy(false);
    }
  }

  const first = (texts: string[]) => texts[0] ?? '';
  const combineNotes = (texts: string[]) => {
    const notes: unknown[] = [];
    for (const txt of texts) {
      const parsed: unknown = JSON.parse(txt);
      if (Array.isArray(parsed)) notes.push(...(parsed as unknown[]));
      else notes.push(parsed);
    }
    return JSON.stringify(notes);
  };

  function importRow(
    label: string,
    accept: string,
    prepare: (texts: string[]) => string,
    importFn: (body: string) => Promise<api.ImportResult>,
    multiple = false,
  ) {
    return (
      <div className="mb2">
        <div className="mb1">{label}</div>
        <input
          type="file"
          accept={accept}
          multiple={multiple}
          disabled={busy}
          className="f7 pointer"
          onChange={(e) => {
            void runImport(e.target.files, prepare, importFn);
            e.target.value = '';
          }}
        />
      </div>
    );
  }

  return (
    <div className="settings-form">
      <div className="f7 fw7 ttu tracked thm-text-tertiary mb2">{t('settings.export')}</div>
      <div className="mb1">
        <a className="link" href={api.exportPaths.bookmarksJson} download>
          {t('settings.exportBookmarksJson')}
        </a>
      </div>
      <div className="mb1">
        <a className="link" href={api.exportPaths.bookmarksHtml} download>
          {t('settings.exportBookmarksHtml')}
        </a>
      </div>
      <div className="mb3">
        <a className="link" href={api.exportPaths.notesJson} download>
          {t('settings.exportNotesJson')}
        </a>
      </div>

      <div className="f7 fw7 ttu tracked thm-text-tertiary mb2">{t('settings.import')}</div>
      {importRow(t('settings.importBookmarksJson'), '.json', first, api.importBookmarks)}
      {importRow(t('settings.importFirefoxJson'), '.json', first, api.importFirefox)}
      {importRow(t('settings.importNetscapeHtml'), '.html,.htm', first, api.importNetscape)}
      {importRow(t('settings.importNotesJson'), '.json', combineNotes, api.importNotes, true)}

      {status ? <div className="f7 mt2">{status}</div> : null}
      {error ? <div className="thm-text-error f7 mt2">{error}</div> : null}
    </div>
  );
}

import { TimeoutError } from 'ky';
import React from 'react';
import { useTranslation } from 'react-i18next';

import { editAccountSettings } from '../api';
import type { AccountSettings } from '../types';
import { apiErrorMsg } from '../util';

/** Displays and edits the current user's account settings. */
export function AccountSettingsView({ initial }: { initial: AccountSettings }) {
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

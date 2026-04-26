import React from 'react';

import { editAccountSettings } from '../api';
import type { AccountSettings } from '../types';

export function AccountSettingsView({ initial }: { initial: AccountSettings }) {
  const [us, setUs] = React.useState<AccountSettings>(initial);
  const archiveDisabled = !us.archiveBackendEnabled;
  const archiveDisabledTitle = archiveDisabled ? 'Archiving Disabled' : undefined;

  async function update(next: AccountSettings) {
    setUs(next);
    await editAccountSettings(next);
  }

  return (
    <div className="settings-form">
      <div className="fw7 mb2">Account Settings</div>

      <div
        className={`flex items-center mb2${archiveDisabled ? ' o-50' : ''}`}
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
          className={`lh-copy${archiveDisabled ? ' gray' : ''}`}
          title={archiveDisabledTitle}
        >
          Archive Non-Private Bookmarks
        </label>
      </div>

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
          Default new bookmarks to Private
        </label>
      </div>

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
          Enable tag suggestions
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
          Privacy Lock (Private Account)
        </label>
      </div>
    </div>
  );
}

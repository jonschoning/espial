import React from 'react';

import { editAccountSettings } from '../api';
import type { AccountSettings } from '../types';

export function AccountSettingsView({ initial }: { initial: AccountSettings }) {
  const [us, setUs] = React.useState<AccountSettings>(initial);

  async function update(next: AccountSettings) {
    setUs(next);
    await editAccountSettings(next);
  }

  return (
    <div className="settings-form">
      <div className="fw7 mb2">Account Settings</div>

      <div className="flex items-center mb2">
        <input
          type="checkbox"
          className="pointer mr2"
          id="archiveDefault"
          name="archiveDefault"
          checked={us.archiveDefault}
          onChange={(e) => void update({ ...us, archiveDefault: e.target.checked })}
        />
        <label htmlFor="archiveDefault" className="lh-copy">
          Archive Non-Private Bookmarks (archive.li)
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

import i18next from 'i18next';
import ChainedBackend from 'i18next-chained-backend';
import HttpBackend from 'i18next-http-backend';
import LocalStorageBackend from 'i18next-localstorage-backend';
import { initReactI18next } from 'react-i18next';

import translationEn from '../../static/locales/en/translation.json';
import { app } from './globals';
import { getShortHash } from './util';

export const defaultNS = 'translation';
export const fallbackNS = 'translation';

const a = app();

void i18next
  .use(ChainedBackend)
  .use(initReactI18next)
  .init({
    lng: a.lang,
    fallbackLng: 'en',
    defaultNS: defaultNS,
    ns: [defaultNS],
    partialBundledLanguages: true,
    resources: {
      en: {
        translation: translationEn,
      },
    },
    backend: {
      backends: [LocalStorageBackend, HttpBackend],
      backendOptions: [
        {
          prefix: `i18next_res_${getShortHash(a.i18nR)}_`,
          expirationTime: 7 * 24 * 60 * 60 * 1000, // 7 days
        },
        {
          loadPath: `${a.i18nR}/{{lng}}/{{ns}}.json`,
        },
      ],
    },
    interpolation: {
      escapeValue: false,
    },
    load: 'currentOnly',
  });

export default i18next;

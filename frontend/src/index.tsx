import { createRoot } from 'react-dom/client';

import { logout } from './api';
import { AccountSettingsView } from './components/AccountSettings';
import { AddForm } from './components/AddForm';
import { BList } from './components/BList';
import { NList } from './components/NList';
import { NNote } from './components/NNote';
import { TagCloud } from './components/TagCloud';
import type { AccountSettings, Bookmark, ColorSchemePreference, Note, TagCloudMode } from './types';
import { tagCloudModeToF } from './types';

function selectEl(selector: string): Element | null {
  return document.querySelector(selector);
}

function viewRendered(): void {
  const el = document.querySelector('#content');
  if (el) el.setAttribute('view-rendered', '');
}

export const logoutE = (e: Event) => () => {
  e.preventDefault();
  void logout();
};

export const renderBookmarks = (renderElSelector: string) => (bmarks: Bookmark[]) => () => {
  const el = selectEl(renderElSelector);
  if (!el) return;
  createRoot(el).render(<BList initial={bmarks} />);
  viewRendered();
};

export const renderTagCloud = (renderElSelector: string) => (tagCloudMode: TagCloudMode) => () => {
  const el = selectEl(renderElSelector);
  if (!el) return;
  createRoot(el).render(<TagCloud initialMode={tagCloudModeToF(tagCloudMode)} />);
};

export const renderAddForm = (renderElSelector: string) => (bmark: Bookmark) => () => {
  const el = selectEl(renderElSelector);
  if (!el) return;
  createRoot(el).render(<AddForm initial={bmark} />);
  viewRendered();
};

export const renderNotes = (renderElSelector: string) => (notes: Note[]) => () => {
  const el = selectEl(renderElSelector);
  if (!el) return;
  createRoot(el).render(<NList initial={notes} />);
  viewRendered();
};

export const renderNote = (renderElSelector: string) => (note: Note) => () => {
  const el = selectEl(renderElSelector);
  if (!el) return;
  createRoot(el).render(<NNote initial={note} />);
  viewRendered();
};

export const renderAccountSettings =
  (renderElSelector: string) => (accountSettings: AccountSettings) => () => {
    const el = selectEl(renderElSelector);
    if (!el) return;
    createRoot(el).render(<AccountSettingsView initial={accountSettings} />);
    viewRendered();
  };

export function initColorSchemeToggle(): void {
  function isColorSchemePreference(value: string | null): value is ColorSchemePreference {
    return value === 'light' || value === 'dark';
  }
  function getEffectiveColorScheme(): ColorSchemePreference {
    const explicit = document.documentElement.dataset.colorScheme;
    if (explicit === 'light' || explicit === 'dark') return explicit;
    return window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';
  }

  function applyColorSchemePreference(preference: ColorSchemePreference): void {
    document.documentElement.dataset.colorScheme = preference;
  }
  function setColorSchemeToggleText(toggleEl: HTMLAnchorElement): void {
    const next = getEffectiveColorScheme() === 'dark' ? 'light' : 'dark';
    toggleEl.textContent = `${next} mode\u00A0\u00A0`;
  }

  const colorSchemeStorageKey = 'espial-color-scheme';
  const stored = window.localStorage.getItem(colorSchemeStorageKey);
  if (isColorSchemePreference(stored)) {
    applyColorSchemePreference(stored);
  }

  const toggleEl = document.querySelector<HTMLAnchorElement>('#color-scheme-toggle');
  if (!toggleEl) return;

  setColorSchemeToggleText(toggleEl);
  toggleEl.addEventListener('click', (e) => {
    e.preventDefault();
    const next: ColorSchemePreference = getEffectiveColorScheme() === 'dark' ? 'light' : 'dark';
    applyColorSchemePreference(next);
    window.localStorage.setItem(colorSchemeStorageKey, next);
    setColorSchemeToggleText(toggleEl);
  });
}

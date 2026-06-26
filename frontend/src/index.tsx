import './i18n';

import { type ReactNode, useLayoutEffect } from 'react';
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

function RenderReady({ children }: { children: ReactNode }) {
  useLayoutEffect(() => {
    viewRendered();
  }, []);

  return children;
}

function renderView(renderElSelector: string, node: ReactNode): void {
  const el = selectEl(renderElSelector);
  if (!el) return;
  createRoot(el).render(<RenderReady>{node}</RenderReady>);
}

export const logoutE = (e: Event) => () => {
  e.preventDefault();
  void logout();
};

export const renderBookmarks = (renderElSelector: string) => (bmarks: Bookmark[]) => () => {
  renderView(renderElSelector, <BList initial={bmarks} />);
};

export const renderTagCloud = (renderElSelector: string) => (tagCloudMode: TagCloudMode) => () => {
  const el = selectEl(renderElSelector);
  if (!el) return;
  createRoot(el).render(<TagCloud initialMode={tagCloudModeToF(tagCloudMode)} />);
};

export const renderAddForm = (renderElSelector: string) => (bmark: Bookmark) => () => {
  renderView(renderElSelector, <AddForm initial={bmark} />);
};

export const renderNotes = (renderElSelector: string) => (notes: Note[]) => () => {
  renderView(renderElSelector, <NList initial={notes} />);
};

export const renderNote = (renderElSelector: string) => (note: Note) => () => {
  renderView(renderElSelector, <NNote initial={note} />);
};

export const renderAccountSettings =
  (renderElSelector: string) => (accountSettings: AccountSettings) => () => {
    renderView(renderElSelector, <AccountSettingsView initial={accountSettings} />);
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

  const colorSchemeStorageKey = 'espial-color-scheme';
  const stored = window.localStorage.getItem(colorSchemeStorageKey);
  if (isColorSchemePreference(stored)) {
    applyColorSchemePreference(stored);
  }

  const toggleEl = document.querySelector<HTMLAnchorElement>('#color-scheme-toggle');
  if (!toggleEl) return;

  toggleEl.addEventListener('click', (e) => {
    e.preventDefault();
    const next: ColorSchemePreference = getEffectiveColorScheme() === 'dark' ? 'light' : 'dark';
    applyColorSchemePreference(next);
    window.localStorage.setItem(colorSchemeStorageKey, next);
  });
}

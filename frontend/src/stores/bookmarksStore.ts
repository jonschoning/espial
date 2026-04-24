import { create } from 'zustand';

import type { Bookmark } from '../types';

export type BookmarksState = {
  bmarks: Bookmark[];
  setAll: (bmarks: Bookmark[]) => void;
  removeById: (bid: number) => void;
  upsert: (bm: Bookmark) => void;
};

export const useBookmarksStore = create<BookmarksState>((set) => ({
  bmarks: [],
  setAll: (bmarks) => {
    set({ bmarks });
  },
  removeById: (bid) => {
    set((s) => ({ bmarks: s.bmarks.filter((b) => b.bid !== bid) }));
  },
  upsert: (bm) => {
    set((s) => ({ bmarks: s.bmarks.map((b) => (b.bid === bm.bid ? bm : b)) }));
  },
}));

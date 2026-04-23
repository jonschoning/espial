import { create } from "zustand";
import type { TagCloud, TagCloudModeF } from "../types";

export type TagCloudState = {
  mode: TagCloudModeF;
  tagcloud: TagCloud;
  setMode: (mode: TagCloudModeF) => void;
  setTagCloud: (tagcloud: TagCloud) => void;
};

export const useTagCloudStore = create<TagCloudState>((set) => ({
  mode: { kind: "none" },
  tagcloud: {},
  setMode: (mode) => set({ mode }),
  setTagCloud: (tagcloud) => set({ tagcloud }),
}));


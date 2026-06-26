import React from 'react';

import { useBookmarksStore } from '../stores/bookmarksStore';
import type { Bookmark } from '../types';
import { BMark } from './BMark';

/** Renders the full list of bookmarks, delegating each item to BMark. */
export function BList({ initial }: { initial: Bookmark[] }) {
  const [hasHydrated, setHasHydrated] = React.useState(false);
  const bmarks = useBookmarksStore((s) => s.bmarks);
  const setAll = useBookmarksStore((s) => s.setAll);
  const removeById = useBookmarksStore((s) => s.removeById);
  const upsert = useBookmarksStore((s) => s.upsert);

  React.useLayoutEffect(() => {
    setAll(initial);
    setHasHydrated(true);
  }, [initial, setAll]);

  const visibleBmarks = hasHydrated ? bmarks : initial;

  return (
    <div>
      {visibleBmarks.map((b) => (
        <BMark
          key={b.bid}
          initial={b}
          onNotifyRemove={() => {
            removeById(b.bid);
          }}
          onUpdated={(bm) => {
            upsert(bm);
          }}
        />
      ))}
    </div>
  );
}

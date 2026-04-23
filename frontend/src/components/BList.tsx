import React from "react";
import type { Bookmark } from "../types";
import { useBookmarksStore } from "../stores/bookmarksStore";
import { BMark } from "./BMark";

export function BList({ initial }: { initial: Bookmark[] }) {
  const bmarks = useBookmarksStore((s) => s.bmarks);
  const setAll = useBookmarksStore((s) => s.setAll);
  const removeById = useBookmarksStore((s) => s.removeById);
  const upsert = useBookmarksStore((s) => s.upsert);

  React.useEffect(() => {
    setAll(initial);
  }, [initial, setAll]);

  return (
    <div>
      {bmarks.map((b) => (
        <BMark
          key={b.bid}
          initial={b}
          onNotifyRemove={() => removeById(b.bid)}
          onUpdated={(bm) => upsert(bm)}
        />
      ))}
    </div>
  );
}


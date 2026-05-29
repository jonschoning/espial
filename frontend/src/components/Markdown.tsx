import DOMPurify from 'dompurify';
import { marked } from 'marked';
import React from 'react';

marked.setOptions({
  pedantic: false,
  gfm: true,
});

/** Renders sanitized Markdown text as HTML. */
export function Markdown({ text, className }: { text: string; className?: string }) {
  const html = React.useMemo(() => {
    if (!text) return '';
    const parsed = marked.parse(text, { async: false });
    return DOMPurify.sanitize(typeof parsed === 'string' ? parsed : '');
  }, [text]);

  return <div className={className} dangerouslySetInnerHTML={{ __html: html }} />;
}

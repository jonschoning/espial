import DOMPurify from 'dompurify';
import hljs from 'highlight.js/lib/core';
import bash from 'highlight.js/lib/languages/bash';
import csharp from 'highlight.js/lib/languages/csharp';
import css from 'highlight.js/lib/languages/css';
import diff from 'highlight.js/lib/languages/diff';
import go from 'highlight.js/lib/languages/go';
import haskell from 'highlight.js/lib/languages/haskell';
import javascript from 'highlight.js/lib/languages/javascript';
import json from 'highlight.js/lib/languages/json';
import plaintext from 'highlight.js/lib/languages/plaintext';
import powershell from 'highlight.js/lib/languages/powershell';
import python from 'highlight.js/lib/languages/python';
import rust from 'highlight.js/lib/languages/rust';
import sql from 'highlight.js/lib/languages/sql';
import typescript from 'highlight.js/lib/languages/typescript';
import xml from 'highlight.js/lib/languages/xml';
import yaml from 'highlight.js/lib/languages/yaml';
import { Marked } from 'marked';
import { markedHighlight } from 'marked-highlight';
import React from 'react';

import { app } from '../globals';

const markedOptions = { pedantic: false, gfm: true };

const plain = new Marked(markedOptions);

const highlighted = new Marked(
  markedOptions,
  markedHighlight({
    emptyLangClass: 'hljs',
    langPrefix: 'hljs language-',
    // returning the code unchanged leaves the token unescaped, so marked escapes it as usual
    highlight: (code, lang) =>
      hljs.getLanguage(lang)
        ? hljs.highlight(code, { language: lang, ignoreIllegals: true }).value
        : code,
  }),
);

/** Renders sanitized Markdown text as HTML. */
export function Markdown({ text, className }: { text: string; className?: string }) {
  const highlight = app().dat.markdownSyntaxHighlight !== false;
  const html = React.useMemo(() => {
    if (!text) return '';
    const parsed = (highlight ? highlighted : plain).parse(text, { async: false });
    return DOMPurify.sanitize(typeof parsed === 'string' ? parsed : '');
  }, [text, highlight]);

  return <div className={className} dangerouslySetInnerHTML={{ __html: html }} />;
}

for (const [name, language] of Object.entries({
  bash,
  csharp,
  css,
  diff,
  go,
  haskell,
  javascript,
  json,
  plaintext,
  powershell,
  python,
  rust,
  sql,
  typescript,
  xml,
  yaml,
})) {
  hljs.registerLanguage(name, language);
}

hljs.registerAliases(['curl'], { languageName: 'bash' });

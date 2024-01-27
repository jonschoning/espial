import { marked } from 'marked';
import DOMPurify from "dompurify"

marked.setOptions({
  pedantic: false,
  gfm: true
});

export const markedImpl = function(str) {
  if (!str) return "";
  return DOMPurify.sanitize(marked.parse(str));
};

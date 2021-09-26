var marked = require("marked");
var DOMPurify = require("dompurify");

marked.setOptions({
  pedantic: false,
  gfm: true
});

exports.markedImpl = function(str) {
  if (!str) return "";
  return DOMPurify.sanitize(marked(str));
};

var marked = require("marked");

marked.setOptions({
  pedantic: false,
  gfm: true
});

exports.markedImpl = function(str) {
  if (!str) return "";
  return marked(str);
};

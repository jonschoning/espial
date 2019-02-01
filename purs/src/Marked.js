var marked = require("marked");

marked.setOptions({
  pedantic: false,
  gfm: true
});

exports.markedImpl = function(str) {
  return marked(str);
};

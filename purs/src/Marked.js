exports.markedImpl = function(str) {
  marked.setOptions({
    pedantic: false,
    gfm: true
  });
  return marked(str);
};

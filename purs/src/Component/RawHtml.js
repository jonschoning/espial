// use at your own risk!
exports.unsafeSetInnerHTML = function(element) {
  return function(html) {
    return function() {
      element.innerHTML = html;
    };
  };
};

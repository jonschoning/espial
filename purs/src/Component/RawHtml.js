// use at your own risk!
export const unsafeSetInnerHTML = function(element) {
  return function(html) {
    return function() {
      element.innerHTML = html;
    };
  };
};

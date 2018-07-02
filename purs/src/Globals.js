"use strict";

exports._app = function() {
  return app;
}

exports._closest = function(just, nothing, selector, el) {
  var node = el.closest(selector);
  if(node) {
    return just(node);
  } else {
    return nothing;
  }
}

exports._innerHtml = function(el, content) {
  el.innerHTML = content
}

exports._replace_iso_timestamps = function() {
  $('.js-moment').each(function(i, e) {
    var $e = $(e);
    var m = moment($e.data('iso8601'), moment.ISO_8601);
    $e.text(m.fromNow());
    $e.attr('title', m.format('MMMM D YYYY, h:mm a') + " (" + m.format() + ") ");
    $e.css('visibility','visible');
  });
}

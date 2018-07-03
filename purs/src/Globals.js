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

exports._innerHtml = function(el) {
  return el.innerHTML;
}

exports._setInnerHtml = function(content, el) {
  el.innerHTML = content;
  return el;
}

exports._createFormData = function(formElement) {
  return new FormData(formElement);
}

exports._createFormString = function(formElement) {
  return new URLSearchParams(new FormData(formElement)).toString()
}

exports._createFormArray = function(formElement) {
  return Array.from(new FormData(formElement));
}

exports._getDataAttribute = function(name, el) {
  return el.dataset[name];
}

exports._setDataAttribute = function(name, value, el) {
  return el.dataset[name] = value;
}

exports._moment8601 = function(tuple, s) {
  var m = moment(s, moment.ISO_8601);
  var s1 = m.fromNow();
  var s2 = m.format('MMMM D YYYY, h:mm a') + " (" + m.format() + ") ";
  return tuple(s1)(s2);
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

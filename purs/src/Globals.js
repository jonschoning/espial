"use strict";

var moment = require("moment");

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

exports._moment8601 = function(tuple, s) {
  var m = moment(s, moment.ISO_8601);
  var s1 = m.fromNow();
  var s2 = m.format('MMMM D YYYY, h:mm a') + " (" + m.format() + ") ";
  return tuple(s1)(s2);
}

exports._mmoment8601 = function(just, nothing, tuple, s) {
  try {
    var m = moment(s, moment.ISO_8601);
    var s1 = m.fromNow();
    var s2 = m.format('MMMM D YYYY, h:mm a') + " (" + m.format() + ") ";
    return just(tuple(s1)(s2));
  } catch (error) {
    return nothing
  }
}

exports._closeWindow = function (window) {
  window.close();
};

exports._setFocus = function(elemId) {
    document.getElementById(elemId).focus();
};


exports._toLocaleDateString = function(dateString) {
  return new Date(dateString).toLocaleDateString(undefined, {dateStyle: 'medium'}) 
}

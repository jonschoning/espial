"use strict";

import moment from 'moment'

export const _app = function() {
  return app;
}

export const _closest = function(just, nothing, selector, el) {
  var node = el.closest(selector);
  if(node) {
    return just(node);
  } else {
    return nothing;
  }
}

export const _createFormData = function(formElement) {
  return new FormData(formElement);
}

export const _createFormString = function(formElement) {
  return new URLSearchParams(new FormData(formElement)).toString()
}

export const _createFormArray = function(formElement) {
  return Array.from(new FormData(formElement));
}

export const _moment8601 = function(tuple, s) {
  var m = moment(s, moment.ISO_8601);
  var s1 = m.fromNow();
  var s2 = m.format('MMMM D YYYY, h:mm a') + " (" + m.format() + ") ";
  return tuple(s1)(s2);
}

export const _mmoment8601 = function(just, nothing, tuple, s) {
  try {
    var m = moment(s, moment.ISO_8601);
    var s1 = m.fromNow();
    var s2 = m.format('MMMM D YYYY, h:mm a') + " (" + m.format() + ") ";
    return just(tuple(s1)(s2));
  } catch (error) {
    return nothing
  }
}

export const _closeWindow = function (window) {
  window.close();
};

export const _setFocus = function(elemId) {
    document.getElementById(elemId).focus();
};


export const _toLocaleDateString = function(dateString) {
  return new Date(dateString).toLocaleDateString(undefined, {dateStyle: 'medium'}) 
}

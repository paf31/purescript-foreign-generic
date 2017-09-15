"use strict";

var dateFormat = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}.\d{3}Z$/;

function jsDateReviver(key, value) {
  if (typeof value === "string" && dateFormat.test(value)) {
      return new Date(value);
  }

  return value;
}

exports.parseJSONImpl = function (str) {
  return JSON.parse(str, jsDateReviver);
};

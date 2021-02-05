"use strict";

exports.parseJSONImpl = function (str) {
  return JSON.parse(str);
};

exports.unsafeStringify = function (str) {
  return JSON.stringify(str);
};

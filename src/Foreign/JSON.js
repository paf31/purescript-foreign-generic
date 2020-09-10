/*global exports, require*/
"use strict";

var JSONbig = require('json-bigint');

exports.parseJSONImpl = function (str) {
  return JSONbig.parse(str);
};

exports.unsafeStringify = function (value) {
    return JSONbig.stringify(value);
};

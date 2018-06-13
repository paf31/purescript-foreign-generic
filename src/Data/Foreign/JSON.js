"use strict";

exports.parseJSONImpl = function (str) {
  return JSON.parse(str);
};

exports.addToObjImpl = function (key, value, obj) {
  var clone = Object.assign({}, obj);
  clone[key] = value;
  return clone;
}

/*global exports, require*/
'use strict';

var BigNumber = require('bignumber.js');

exports.eq_ = function (a) {
    return function (b) {
        return a.isEqualTo(b);
    };
};

exports.show_ = function (n) {
    return n.toString(10);
};

exports.toNumber = function (n) {
    return n.toNumber();
};

exports.fromInt = function (int) {
    return BigNumber(int);
};

exports.fromString_ = function (nothing) {
    return function (just) {
        return function (str) {
            var n = BigNumber(str);
            if (n && n.isInteger()) {
                return just(n);
            } else {
                return nothing;
            }
        };
    };
};

exports.add_ = function (x) {
    return function (y) {
        return x.plus(y);
    };
};

exports.mul_ = function (x) {
    return function (y) {
        return x.multipliedBy(y);
    };
};

exports.sub_ = function (x) {
    return function (y) {
        return x.minus(y);
    };
};

exports.div_ = function (x) {
    return function (y) {
        return x.dividedToIntegerBy(y);
    };
};

exports.mod_ = function (x) {
    return function (y) {
        return x.modulo(y);
    };
};

exports.degree_ = function (x) {
    return x.absoluteValue().toNumber();
};

exports.comparedTo_ = function (x) {
    return function (y) {
        return x.comparedTo(y);
    };
};

exports.readBigInteger_ = function (nothing) {
    return function (just) {
        return function (value) {
            var tag = Object.prototype.toString.call(value).slice(8,-1);
            if (value !== undefined && (BigNumber.isBigNumber(value) || tag === "Number" || tag === "String")) {
                var n = BigNumber(value);
                if (n && n.isInteger()) {
                    return just(n);
                } else {
                    return nothing;
                }
            } else {
                return nothing;
            }
        };
    };
};

exports.format = function (value) {
    return value.toFormat();
};

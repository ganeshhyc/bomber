// Generated by purs version 0.11.7
"use strict";
var Neon_Class = require("../Neon.Class");
var Neon_Class_HasAdd = require("../Neon.Class.HasAdd");
var Neon_Class_HasAnd = require("../Neon.Class.HasAnd");
var Neon_Class_HasDivide = require("../Neon.Class.HasDivide");
var Neon_Class_HasEqual = require("../Neon.Class.HasEqual");
var Neon_Class_HasGreater = require("../Neon.Class.HasGreater");
var Neon_Class_HasLess = require("../Neon.Class.HasLess");
var Neon_Class_HasMultiply = require("../Neon.Class.HasMultiply");
var Neon_Class_HasOr = require("../Neon.Class.HasOr");
var Neon_Class_HasPower = require("../Neon.Class.HasPower");
var Neon_Class_HasRemainder = require("../Neon.Class.HasRemainder");
var Neon_Class_HasSubtract = require("../Neon.Class.HasSubtract");
var Neon_Helper = require("../Neon.Helper");
var _subtract = function (dictHasSubtract) {
    return function (x) {
        return function (y) {
            return Neon_Class_HasSubtract.subtract(dictHasSubtract)(y)(x);
        };
    };
};
var _remainder = function (dictHasRemainder) {
    return function (x) {
        return function (y) {
            return Neon_Class_HasRemainder.remainder(dictHasRemainder)(y)(x);
        };
    };
};
var _power = function (dictHasPower) {
    return function (x) {
        return function (y) {
            return Neon_Class_HasPower.power(dictHasPower)(y)(x);
        };
    };
};
var _or = function (dictHasOr) {
    return function (x) {
        return function (y) {
            return Neon_Class_HasOr.or(dictHasOr)(y)(x);
        };
    };
};
var _notEqual = function (dictHasEqual) {
    return function (x) {
        return function (y) {
            return Neon_Helper.notEqual(dictHasEqual)(y)(x);
        };
    };
};
var _multiply = function (dictHasMultiply) {
    return function (x) {
        return function (y) {
            return Neon_Class_HasMultiply.multiply(dictHasMultiply)(y)(x);
        };
    };
};
var _lessOrEqual = function (dictHasEqual) {
    return function (dictHasLess) {
        return function (x) {
            return function (y) {
                return Neon_Helper.lessOrEqual(dictHasEqual)(dictHasLess)(y)(x);
            };
        };
    };
};
var _less = function (dictHasLess) {
    return function (x) {
        return function (y) {
            return Neon_Class_HasLess.less(dictHasLess)(y)(x);
        };
    };
};
var _greaterOrEqual = function (dictHasEqual) {
    return function (dictHasGreater) {
        return function (x) {
            return function (y) {
                return Neon_Helper.greaterOrEqual(dictHasEqual)(dictHasGreater)(y)(x);
            };
        };
    };
};
var _greater = function (dictHasGreater) {
    return function (x) {
        return function (y) {
            return Neon_Class_HasGreater.greater(dictHasGreater)(y)(x);
        };
    };
};
var _equal = function (dictHasEqual) {
    return function (x) {
        return function (y) {
            return Neon_Class_HasEqual.equal(dictHasEqual)(y)(x);
        };
    };
};
var _divide = function (dictHasDivide) {
    return function (x) {
        return function (y) {
            return Neon_Class_HasDivide.divide(dictHasDivide)(y)(x);
        };
    };
};
var _call = function (x) {
    return function (f) {
        return f(x);
    };
};
var _and = function (dictHasAnd) {
    return function (x) {
        return function (y) {
            return Neon_Class_HasAnd.and(dictHasAnd)(y)(x);
        };
    };
};
var _add = function (dictHasAdd) {
    return function (x) {
        return function (y) {
            return Neon_Class_HasAdd.add(dictHasAdd)(y)(x);
        };
    };
};
module.exports = {
    _call: _call,
    _power: _power,
    _multiply: _multiply,
    _divide: _divide,
    _remainder: _remainder,
    _add: _add,
    _subtract: _subtract,
    _equal: _equal,
    _notEqual: _notEqual,
    _greater: _greater,
    _greaterOrEqual: _greaterOrEqual,
    _less: _less,
    _lessOrEqual: _lessOrEqual,
    _and: _and,
    _or: _or
};
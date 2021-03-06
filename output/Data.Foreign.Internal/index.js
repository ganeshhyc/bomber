// Generated by purs version 0.11.7
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Monad_Except_Trans = require("../Control.Monad.Except.Trans");
var Data_Boolean = require("../Data.Boolean");
var Data_Eq = require("../Data.Eq");
var Data_Foreign = require("../Data.Foreign");
var Data_Function = require("../Data.Function");
var Data_Identity = require("../Data.Identity");
var Data_StrMap = require("../Data.StrMap");
var Prelude = require("../Prelude");
var isStrMap = function (v) {
    return Data_Foreign.tagOf(v) === "Object";
};
var readStrMap = function (value) {
    if (isStrMap(value)) {
        return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))(Data_Foreign.unsafeFromForeign(value));
    };
    if (Data_Boolean.otherwise) {
        return Data_Foreign.fail(new Data_Foreign.TypeMismatch("StrMap", Data_Foreign.tagOf(value)));
    };
    throw new Error("Failed pattern match at Data.Foreign.Internal line 13, column 1 - line 13, column 44: " + [ value.constructor.name ]);
};
module.exports = {
    isStrMap: isStrMap,
    readStrMap: readStrMap
};

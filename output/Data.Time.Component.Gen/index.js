// Generated by purs version 0.11.7
"use strict";
var Control_Monad_Gen = require("../Control.Monad.Gen");
var Data_Enum_Gen = require("../Data.Enum.Gen");
var Data_Time_Component = require("../Data.Time.Component");
var genSecond = function (dictMonadGen) {
    return Data_Enum_Gen.genBoundedEnum(dictMonadGen)(Data_Time_Component.boundedEnumSecond);
};
var genMinute = function (dictMonadGen) {
    return Data_Enum_Gen.genBoundedEnum(dictMonadGen)(Data_Time_Component.boundedEnumMinute);
};
var genMillisecond = function (dictMonadGen) {
    return Data_Enum_Gen.genBoundedEnum(dictMonadGen)(Data_Time_Component.boundedEnumMillisecond);
};
var genHour = function (dictMonadGen) {
    return Data_Enum_Gen.genBoundedEnum(dictMonadGen)(Data_Time_Component.boundedEnumHour);
};
module.exports = {
    genHour: genHour,
    genMinute: genMinute,
    genSecond: genSecond,
    genMillisecond: genMillisecond
};

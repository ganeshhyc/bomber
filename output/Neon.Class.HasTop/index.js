// Generated by purs version 0.11.7
"use strict";
var Data_Bounded = require("../Data.Bounded");
var Neon_Data = require("../Neon.Data");
var Neon_Primitive = require("../Neon.Primitive");
var Neon_Primitive_Number = require("../Neon.Primitive.Number");
var Prelude = require("../Prelude");
var HasTop = function (top) {
    this.top = top;
};
var top = function (dict) {
    return dict.top;
};
var orderingHasTop = new HasTop(Data_Bounded.top(Data_Bounded.boundedOrdering));
var numberHasTop = new HasTop(Neon_Primitive_Number.infinity);
var intHasTop = new HasTop(Data_Bounded.top(Data_Bounded.boundedInt));
var charHasTop = new HasTop(Data_Bounded.top(Data_Bounded.boundedChar));
var booleanHasTop = new HasTop(true);
module.exports = {
    top: top,
    HasTop: HasTop,
    booleanHasTop: booleanHasTop,
    charHasTop: charHasTop,
    intHasTop: intHasTop,
    numberHasTop: numberHasTop,
    orderingHasTop: orderingHasTop
};
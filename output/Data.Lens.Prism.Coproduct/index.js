// Generated by purs version 0.11.7
"use strict";
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Functor_Coproduct = require("../Data.Functor.Coproduct");
var Data_Lens_Iso_Newtype = require("../Data.Lens.Iso.Newtype");
var Data_Lens_Prism = require("../Data.Lens.Prism");
var Data_Lens_Prism_Either = require("../Data.Lens.Prism.Either");
var Prelude = require("../Prelude");
var _Right = function (dictChoice) {
    return function ($2) {
        return Data_Lens_Iso_Newtype._Newtype(Data_Functor_Coproduct.newtypeCoproduct)(Data_Functor_Coproduct.newtypeCoproduct)(dictChoice.Profunctor0())(Data_Lens_Prism_Either._Right(dictChoice)($2));
    };
};
var _Left = function (dictChoice) {
    return function ($3) {
        return Data_Lens_Iso_Newtype._Newtype(Data_Functor_Coproduct.newtypeCoproduct)(Data_Functor_Coproduct.newtypeCoproduct)(dictChoice.Profunctor0())(Data_Lens_Prism_Either._Left(dictChoice)($3));
    };
};
module.exports = {
    _Left: _Left,
    _Right: _Right
};

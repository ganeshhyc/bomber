// Generated by purs version 0.11.7
"use strict";
var Control_Category = require("../Control.Category");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Function = require("../Data.Function");
var Data_Lens_Internal_Forget = require("../Data.Lens.Internal.Forget");
var Data_Lens_Internal_Indexed = require("../Data.Lens.Internal.Indexed");
var Data_Lens_Types = require("../Data.Lens.Types");
var Data_Newtype = require("../Data.Newtype");
var Data_Profunctor_Strong = require("../Data.Profunctor.Strong");
var Data_Tuple = require("../Data.Tuple");
var Prelude = require("../Prelude");
var view = function (l) {
    return Data_Newtype.unwrap(Data_Lens_Internal_Forget.newtypeForget)(l(Control_Category.id(Control_Category.categoryFn)));
};
var viewOn = function (s) {
    return function (l) {
        return view(l)(s);
    };
};
var use = function (dictMonadState) {
    return function (p) {
        return Control_Monad_State_Class.gets(dictMonadState)(function (v) {
            return viewOn(v)(p);
        });
    };
};
var to = function (f) {
    return function (p) {
        return function ($7) {
            return Data_Newtype.unwrap(Data_Lens_Internal_Forget.newtypeForget)(p)(f($7));
        };
    };
};
var takeBoth = function (l) {
    return function (r) {
        return function (a) {
            var cmps = function (v) {
                return function (v1) {
                    return Data_Profunctor_Strong.fanout(Control_Category.categoryFn)(Data_Profunctor_Strong.strongFn)(v)(v1);
                };
            };
            return cmps(l(Control_Category.id(Control_Category.categoryFn)))(r(Control_Category.id(Control_Category.categoryFn)));
        };
    };
};
var iview = function (l) {
    return Data_Newtype.unwrap(Data_Lens_Internal_Forget.newtypeForget)(l(Data_Lens_Internal_Indexed.Indexed(Control_Category.id(Control_Category.categoryFn))));
};
var iuse = function (dictMonadState) {
    return function (p) {
        return Control_Monad_State_Class.gets(dictMonadState)(iview(p));
    };
};
module.exports = {
    viewOn: viewOn,
    view: view,
    to: to,
    takeBoth: takeBoth,
    use: use,
    iview: iview,
    iuse: iuse
};
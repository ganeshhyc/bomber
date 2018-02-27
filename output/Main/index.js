"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Console = require("../Control.Monad.Eff.Console");
var Control_Plus = require("../Control.Plus");
var DOM = require("../DOM");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Lens = require("../Data.Lens");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_String = require("../Data.String");
var Data_Unit = require("../Data.Unit");
var FRP = require("../FRP");
var FRP_Behavior = require("../FRP.Behavior");
var FRP_Behavior_Keyboard = require("../FRP.Behavior.Keyboard");
var FRP_Event = require("../FRP.Event");
var FRP_Event_Keyboard = require("../FRP.Event.Keyboard");
var Prelude = require("../Prelude");
var PrestoDOM_Core = require("../PrestoDOM.Core");
var PrestoDOM_Elements = require("../PrestoDOM.Elements");
var PrestoDOM_Events = require("../PrestoDOM.Events");
var PrestoDOM_Properties = require("../PrestoDOM.Properties");
var PrestoDOM_Types = require("../PrestoDOM.Types");
var PrestoDOM_Types_DomAttributes = require("../PrestoDOM.Types.DomAttributes");
var PrestoDOM_Util = require("../PrestoDOM.Util");
var view = function (action) {
    return function (state) {
        return PrestoDOM_Elements.relativeLayout([ PrestoDOM_Properties.height(PrestoDOM_Types_DomAttributes.Match_Parent.value), PrestoDOM_Properties.width(PrestoDOM_Types_DomAttributes.Match_Parent.value), PrestoDOM_Properties.background("#323232"), PrestoDOM_Properties.gravity("center"), PrestoDOM_Properties.name("rootNode") ])([ PrestoDOM_Elements.relativeLayout([ PrestoDOM_Properties.height(new PrestoDOM_Types_DomAttributes.V(600)), PrestoDOM_Properties.width(new PrestoDOM_Types_DomAttributes.V(600)), PrestoDOM_Properties.background("gray"), PrestoDOM_Properties.orientation("vertical"), PrestoDOM_Properties.margin("320,20,20,20"), PrestoDOM_Properties.gravity("center") ])([ PrestoDOM_Elements.imageView([ PrestoDOM_Properties.id_("loon1"), PrestoDOM_Properties.height(new PrestoDOM_Types_DomAttributes.V(50)), PrestoDOM_Properties.width(new PrestoDOM_Types_DomAttributes.V(50)), PrestoDOM_Properties.margin("100,20,20,20"), PrestoDOM_Properties.imageUrl("parachute") ]), PrestoDOM_Elements.imageView([ PrestoDOM_Properties.id_("loon2"), PrestoDOM_Properties.height(new PrestoDOM_Types_DomAttributes.V(50)), PrestoDOM_Properties.width(new PrestoDOM_Types_DomAttributes.V(50)), PrestoDOM_Properties.margin("200,0,20,20"), PrestoDOM_Properties.imageUrl("parachute") ]), PrestoDOM_Elements.imageView([ PrestoDOM_Properties.id_("loon3"), PrestoDOM_Properties.height(new PrestoDOM_Types_DomAttributes.V(50)), PrestoDOM_Properties.width(new PrestoDOM_Types_DomAttributes.V(50)), PrestoDOM_Properties.margin("300,100,20,20"), PrestoDOM_Properties.imageUrl("parachute") ]), PrestoDOM_Elements.imageView([ PrestoDOM_Properties.id_("loon4"), PrestoDOM_Properties.height(new PrestoDOM_Types_DomAttributes.V(50)), PrestoDOM_Properties.width(new PrestoDOM_Types_DomAttributes.V(50)), PrestoDOM_Properties.margin("0,200,20,20"), PrestoDOM_Properties.imageUrl("parachute") ]), PrestoDOM_Elements.imageView([ PrestoDOM_Properties.id_("loon5"), PrestoDOM_Properties.height(new PrestoDOM_Types_DomAttributes.V(50)), PrestoDOM_Properties.width(new PrestoDOM_Types_DomAttributes.V(50)), PrestoDOM_Properties.margin("100,300,20,20"), PrestoDOM_Properties.imageUrl("parachute") ]) ]), PrestoDOM_Elements.linearLayout([ PrestoDOM_Properties.height(new PrestoDOM_Types_DomAttributes.V(150)), PrestoDOM_Properties.width(PrestoDOM_Types_DomAttributes.Match_Parent.value), PrestoDOM_Properties.orientation("vertical"), PrestoDOM_Properties.margin("20,20,20,20"), PrestoDOM_Properties.gravity("center") ])([ PrestoDOM_Elements.imageView([ PrestoDOM_Properties.id_("destroyer"), PrestoDOM_Properties.height(new PrestoDOM_Types_DomAttributes.V(50)), PrestoDOM_Properties.width(new PrestoDOM_Types_DomAttributes.V(20)), PrestoDOM_Properties.margin(Data_Show.show(Data_Show.showInt)(state.x) + ",550,100,20"), PrestoDOM_Properties.imageUrl("bomb") ]) ]) ]);
    };
};
var main = (function () {
    var movement = function (left) {
        return function (right) {
            return function (boom) {
                return function (oldState) {
                    var $6 = left && oldState.x > (-300 | 0);
                    if ($6) {
                        var $7 = {};
                        for (var $8 in oldState) {
                            if ({}.hasOwnProperty.call(oldState, $8)) {
                                $7[$8] = oldState[$8];
                            };
                        };
                        $7.x = oldState.x - 10 | 0;
                        return $7;
                    };
                    var $10 = right && oldState.x < 300;
                    if ($10) {
                        var $11 = {};
                        for (var $12 in oldState) {
                            if ({}.hasOwnProperty.call(oldState, $12)) {
                                $11[$12] = oldState[$12];
                            };
                        };
                        $11.x = oldState.x + 10 | 0;
                        return $11;
                    };
                    return oldState;
                };
            };
        };
    };
    var initialState = {
        x: 0
    };
    return function __do() {
        var v = PrestoDOM_Core.mkDyn(PrestoDOM_Core.boolDyn)(false)();
        var v1 = PrestoDOM_Core.mkDyn(PrestoDOM_Core.boolDyn)(false)();
        var v2 = PrestoDOM_Core.mkDyn(PrestoDOM_Core.boolDyn)(false)();
        var v3 = PrestoDOM_Util.render(view({
            left: v,
            right: v1,
            boom: v2
        }))(initialState)();
        return Control_Apply.applySecond(Control_Monad_Eff.applyEff)(v3.updateState(Control_Apply.apply(FRP_Behavior.applyABehavior(FRP_Event.functorEvent))(Control_Apply.apply(FRP_Behavior.applyABehavior(FRP_Event.functorEvent))(Control_Apply.apply(FRP_Behavior.applyABehavior(FRP_Event.functorEvent))(Data_Functor.map(FRP_Behavior.functorABehavior(FRP_Event.functorEvent))(movement)(FRP_Behavior_Keyboard.key(37)))(FRP_Behavior_Keyboard.key(39)))(FRP_Behavior_Keyboard.key(32)))(v3.stateBeh))(FRP_Event_Keyboard.down))(Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(Data_Unit.unit))();
    };
})();
module.exports = {
    main: main,
    view: view
};

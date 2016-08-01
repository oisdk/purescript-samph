// Generated by psc version 0.9.1
"use strict";
var Data_Const = require("../Data.Const");
var Data_Functor_Contravariant = require("../Data.Functor.Contravariant");
var Data_Profunctor = require("../Data.Profunctor");
var Optic_Types = require("../Optic.Types");
var Prelude = require("../Prelude");
var Data_Function = require("../Data.Function");
var view = function (asa) {
    return function (s) {
        return Data_Const.getConst(asa(Data_Const.Const)(s));
    };
};
var weiv = Data_Function.flip(view);
var to = function (dictContravariant) {
    return function (dictFunctor) {
        return function (dictProfunctor) {
            return function (s2a) {
                return Data_Profunctor.dimap(dictProfunctor)(s2a)(Data_Functor_Contravariant.coerce(dictContravariant)(dictFunctor));
            };
        };
    };
};
module.exports = {
    to: to, 
    view: view, 
    weiv: weiv
};
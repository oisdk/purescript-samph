// Generated by psc version 0.9.1
"use strict";
var Prelude = require("../Prelude");
var Control_Monad_Cont_Class = require("../Control.Monad.Cont.Class");
var Control_Monad_Cont_Trans = require("../Control.Monad.Cont.Trans");
var Data_Identity = require("../Data.Identity");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var withCont = function (f) {
    return Control_Monad_Cont_Trans.withContT(function ($0) {
        return function ($1) {
            return Data_Identity.Identity(f(function ($2) {
                return Data_Identity.runIdentity($0($2));
            })($1));
        };
    });
};
var runCont = function (cc) {
    return function (k) {
        return Data_Identity.runIdentity(Control_Monad_Cont_Trans.runContT(cc)(function ($3) {
            return Data_Identity.Identity(k($3));
        }));
    };
};
var mapCont = function (f) {
    return Control_Monad_Cont_Trans.mapContT(function ($4) {
        return Data_Identity.Identity(f(Data_Identity.runIdentity($4)));
    });
};
var cont = function (f) {
    return function (c) {
        return f(function ($5) {
            return Data_Identity.runIdentity(c($5));
        });
    };
};
module.exports = {
    cont: cont, 
    runCont: runCont
};
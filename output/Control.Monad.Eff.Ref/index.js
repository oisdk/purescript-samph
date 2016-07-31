// Generated by psc version 0.9.1
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Data_Unit = require("../Data.Unit");
var modifyRef = function (ref) {
    return function (f) {
        return $foreign["modifyRef'"](ref)(function (s) {
            return {
                state: f(s), 
                value: Data_Unit.unit
            };
        });
    };
};
module.exports = {
    modifyRef: modifyRef, 
    "modifyRef'": $foreign["modifyRef'"], 
    newRef: $foreign.newRef, 
    readRef: $foreign.readRef, 
    writeRef: $foreign.writeRef
};

// Generated by psc version 0.9.1
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Array = require("../Data.Array");
var Data_Ring = require("../Data.Ring");
var unsafeIndex = function (dictPartial) {
    return $foreign.unsafeIndexImpl;
};
var tail = function (dictPartial) {
    return function (xs) {
        return Data_Array.slice(1)(Data_Array.length(xs))(xs);
    };
};
var last = function (dictPartial) {
    return function (xs) {
        return unsafeIndex(dictPartial)(xs)(Data_Array.length(xs) - 1);
    };
};
var init = function (dictPartial) {
    return function (xs) {
        return Data_Array.slice(0)(Data_Array.length(xs) - 1)(xs);
    };
};
var head = function (dictPartial) {
    return function (xs) {
        return unsafeIndex(dictPartial)(xs)(0);
    };
};
module.exports = {
    head: head, 
    init: init, 
    last: last, 
    tail: tail, 
    unsafeIndex: unsafeIndex
};

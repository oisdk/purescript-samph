// Generated by psc version 0.9.1
"use strict";
var Prelude = require("../Prelude");
var Data_String = require("../Data.String");
var Data_Foldable = require("../Data.Foldable");
var Data_Show = require("../Data.Show");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Eq = require("../Data.Eq");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Semiring = require("../Data.Semiring");
var Data_Ring = require("../Data.Ring");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Position = (function () {
    function Position(value0) {
        this.value0 = value0;
    };
    Position.create = function (value0) {
        return new Position(value0);
    };
    return Position;
})();
var updatePosString = function (pos) {
    return function (str) {
        var updatePosChar = function (v) {
            return function (c) {
                if (c === "\n") {
                    return new Position({
                        line: v.value0.line + 1 | 0, 
                        column: 1
                    });
                };
                if (c === "\r") {
                    return new Position({
                        line: v.value0.line + 1 | 0, 
                        column: 1
                    });
                };
                if (c === "\t") {
                    return new Position({
                        line: v.value0.line, 
                        column: (v.value0.column + 8 | 0) - (v.value0.column - 1) % 8
                    });
                };
                return new Position({
                    line: v.value0.line, 
                    column: v.value0.column + 1 | 0
                });
            };
        };
        return Data_Foldable.foldl(Data_Foldable.foldableArray)(updatePosChar)(pos)(Data_String.split("")(str));
    };
};
var showPosition = new Data_Show.Show(function (v) {
    return "Position { line: " + (Data_Show.show(Data_Show.showInt)(v.value0.line) + (", column: " + (Data_Show.show(Data_Show.showInt)(v.value0.column) + " }")));
});
var initialPos = new Position({
    line: 1, 
    column: 1
});
var eqPosition = new Data_Eq.Eq(function (v) {
    return function (v1) {
        return v.value0.line === v1.value0.line && v.value0.column === v1.value0.column;
    };
});
module.exports = {
    Position: Position, 
    initialPos: initialPos, 
    updatePosString: updatePosString, 
    showPosition: showPosition, 
    eqPosition: eqPosition
};

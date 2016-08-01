// Generated by psc version 0.9.1
"use strict";
var Data_Profunctor = require("../Data.Profunctor");
var Data_Profunctor_Choice = require("../Data.Profunctor.Choice");
var Data_Either = require("../Data.Either");
var Prelude = require("../Prelude");
var Data_Functor = require("../Data.Functor");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Function = require("../Data.Function");
var Market = (function () {
    function Market(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Market.create = function (value0) {
        return function (value1) {
            return new Market(value0, value1);
        };
    };
    return Market;
})();
var profunctorMarket = new Data_Profunctor.Profunctor(function (s2r) {
    return function (t2u) {
        return function (v) {
            return new Market(function ($25) {
                return t2u(v.value0($25));
            }, function ($26) {
                return Data_Either.either(function ($27) {
                    return Data_Either.Left.create(t2u($27));
                })(Data_Either.Right.create)(v.value1(s2r($26)));
            });
        };
    };
});
var functorMarket = new Data_Functor.Functor(function (t2u) {
    return function (v) {
        return new Market(function ($28) {
            return t2u(v.value0($28));
        }, function ($29) {
            return Data_Either.either(function ($30) {
                return Data_Either.Left.create(t2u($30));
            })(Data_Either.Right.create)(v.value1($29));
        });
    };
});
var choiceMarket = new Data_Profunctor_Choice.Choice(function () {
    return profunctorMarket;
}, function (v) {
    return new Market(function ($31) {
        return Data_Either.Left.create(v.value0($31));
    }, function (thing) {
        if (thing instanceof Data_Either.Left) {
            return Data_Either.either(function ($32) {
                return Data_Either.Left.create(Data_Either.Left.create($32));
            })(Data_Either.Right.create)(v.value1(thing.value0));
        };
        if (thing instanceof Data_Either.Right) {
            return Data_Function.apply(Data_Either.Left.create)(new Data_Either.Right(thing.value0));
        };
        throw new Error("Failed pattern match at Optic.Internal.Prism line 24, column 63 - line 26, column 32: " + [ thing.constructor.name ]);
    });
}, function (v) {
    return new Market(function ($33) {
        return Data_Either.Right.create(v.value0($33));
    }, function (thing) {
        if (thing instanceof Data_Either.Left) {
            return Data_Function.apply(Data_Either.Left.create)(new Data_Either.Left(thing.value0));
        };
        if (thing instanceof Data_Either.Right) {
            return Data_Either.either(function ($34) {
                return Data_Either.Left.create(Data_Either.Right.create($34));
            })(Data_Either.Right.create)(v.value1(thing.value0));
        };
        throw new Error("Failed pattern match at Optic.Internal.Prism line 28, column 65 - line 30, column 56: " + [ thing.constructor.name ]);
    });
});
module.exports = {
    Market: Market, 
    functorMarket: functorMarket, 
    profunctorMarket: profunctorMarket, 
    choiceMarket: choiceMarket
};

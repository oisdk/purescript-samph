// Generated by psc version 0.9.1
"use strict";
var Data_Either = require("../Data.Either");
var Data_Identity = require("../Data.Identity");
var Data_Maybe = require("../Data.Maybe");
var Data_Profunctor = require("../Data.Profunctor");
var Data_Profunctor_Choice = require("../Data.Profunctor.Choice");
var Optic_Internal_Prism = require("../Optic.Internal.Prism");
var Optic_Types = require("../Optic.Types");
var Prelude = require("../Prelude");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Applicative = require("../Control.Applicative");
var Data_Functor = require("../Data.Functor");
var Data_Function = require("../Data.Function");
var Data_Unit = require("../Data.Unit");
var Data_Eq = require("../Data.Eq");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var withPrism = function (stab) {
    return function (f) {
        var $13 = stab(new Optic_Internal_Prism.Market(Data_Identity.Identity, Data_Either.Right.create));
        return f(function ($18) {
            return Data_Identity.runIdentity($13.value0($18));
        })(function ($19) {
            return Data_Either.either(function ($20) {
                return Data_Either.Left.create(Data_Identity.runIdentity($20));
            })(Data_Either.Right.create)($13.value1($19));
        });
    };
};
var prism = function (dictApplicative) {
    return function (dictChoice) {
        return function (b2t) {
            return function (s2Eta) {
                return function (pafb) {
                    return Data_Profunctor.dimap(dictChoice["__superclass_Data.Profunctor.Profunctor_0"]())(s2Eta)(Data_Either.either(Control_Applicative.pure(dictApplicative))(Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(b2t)))(Data_Profunctor_Choice.right(dictChoice)(pafb));
                };
            };
        };
    };
};
var prism$prime = function (b2s) {
    return function (s2Ma) {
        return function (dictApplicative) {
            return function (dictChoice) {
                return prism(dictApplicative)(dictChoice)(b2s)(function (s) {
                    return Data_Function.apply(Data_Maybe.maybe(new Data_Either.Left(s))(Data_Either.Right.create))(s2Ma(s));
                });
            };
        };
    };
};
var nearly = function (x) {
    return function (p) {
        return function (dictApplicative) {
            return function (dictChoice) {
                var guard = function (v) {
                    if (v) {
                        return new Data_Maybe.Just(Data_Unit.unit);
                    };
                    if (!v) {
                        return Data_Maybe.Nothing.value;
                    };
                    throw new Error("Failed pattern match at Optic.Prism line 44, column 7 - line 44, column 30: " + [ v.constructor.name ]);
                };
                return Data_Function.apply(prism$prime(Data_Function["const"](x)))(function ($21) {
                    return guard(p($21));
                })(dictApplicative)(dictChoice);
            };
        };
    };
};
var only = function (dictEq) {
    return function (x) {
        return function (dictApplicative) {
            return function (dictChoice) {
                return nearly(x)(Data_Eq.eq(dictEq)(x))(dictApplicative)(dictChoice);
            };
        };
    };
};
var matching = function (stab) {
    return withPrism(stab)(function (v) {
        return function (s) {
            return s;
        };
    });
};
var is = function (stab) {
    return function (s) {
        return Data_Function.apply(Data_Either.either(Data_Function["const"](false))(Data_Function["const"](true)))(matching(stab)(s));
    };
};
var isn$primet = function (stab) {
    return function (s) {
        return Data_Function.apply(Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraBoolean))(is(stab)(s));
    };
};
var clonePrism = function (dictApplicative) {
    return function (dictChoice) {
        return function (stab) {
            return withPrism(stab)(prism(dictApplicative)(dictChoice));
        };
    };
};
module.exports = {
    clonePrism: clonePrism, 
    is: is, 
    "isn't": isn$primet, 
    matching: matching, 
    nearly: nearly, 
    only: only, 
    prism: prism, 
    "prism'": prism$prime, 
    withPrism: withPrism
};

// Generated by psc version 0.9.1
"use strict";
var Prelude = require("../Prelude");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Console = require("../Control.Monad.Eff.Console");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Control_Monad_Eff_Random = require("../Control.Monad.Eff.Random");
var Data_List = require("../Data.List");
var Data_Unfoldable = require("../Data.Unfoldable");
var Test_QuickCheck_Arbitrary = require("../Test.QuickCheck.Arbitrary");
var Test_QuickCheck_Gen = require("../Test.QuickCheck.Gen");
var Test_QuickCheck_LCG = require("../Test.QuickCheck.LCG");
var Control_Applicative = require("../Control.Applicative");
var Data_Function = require("../Data.Function");
var Control_Bind = require("../Control.Bind");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Show = require("../Data.Show");
var Data_Semigroup = require("../Data.Semigroup");
var Control_Monad_State_Trans = require("../Control.Monad.State.Trans");
var Data_Identity = require("../Data.Identity");
var Data_Unit = require("../Data.Unit");
var Data_Semiring = require("../Data.Semiring");
var Data_Eq = require("../Data.Eq");
var Success = (function () {
    function Success() {

    };
    Success.value = new Success();
    return Success;
})();
var Failed = (function () {
    function Failed(value0) {
        this.value0 = value0;
    };
    Failed.create = function (value0) {
        return new Failed(value0);
    };
    return Failed;
})();
var Testable = function (test) {
    this.test = test;
};
var withHelp = function (v) {
    return function (v1) {
        if (v) {
            return Success.value;
        };
        if (!v) {
            return new Failed(v1);
        };
        throw new Error("Failed pattern match at Test.QuickCheck line 107, column 1 - line 107, column 26: " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var testableResult = new Testable(Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity)));
var testableBoolean = new Testable(function (v) {
    if (v) {
        return Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity))(Success.value);
    };
    if (!v) {
        return Data_Function.apply(Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity)))(new Failed("Test returned false"));
    };
    throw new Error("Failed pattern match at Test.QuickCheck line 86, column 3 - line 86, column 27: " + [ v.constructor.name ]);
});
var test = function (dict) {
    return dict.test;
};
var testableFunction = function (dictArbitrary) {
    return function (dictTestable) {
        return new Testable(function (f) {
            return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Data_Identity.monadIdentity))(Test_QuickCheck_Arbitrary.arbitrary(dictArbitrary))(function ($35) {
                return test(dictTestable)(f($35));
            });
        });
    };
};
var showResult = new Data_Show.Show(function (v) {
    if (v instanceof Success) {
        return "Success";
    };
    if (v instanceof Failed) {
        return "Failed: " + v.value0;
    };
    throw new Error("Failed pattern match at Test.QuickCheck line 96, column 3 - line 97, column 3: " + [ v.constructor.name ]);
});
var quickCheckPure = function (dictTestable) {
    return function (s) {
        return function (n) {
            return function (prop) {
                return Test_QuickCheck_Gen.evalGen(Data_Unfoldable.replicateA(Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity))(Data_List.unfoldableList)(Data_List.traversableList)(n)(test(dictTestable)(prop)))({
                    newSeed: s, 
                    size: 10
                });
            };
        };
    };
};
var quickCheck$prime = function (dictTestable) {
    return function (n) {
        return function (prop) {
            var throwOnFirstFailure = function (__copy_v) {
                return function (__copy_v1) {
                    var v = __copy_v;
                    var v1 = __copy_v1;
                    tco: while (true) {
                        if (v1 instanceof Data_List.Nil) {
                            return Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(Data_Unit.unit);
                        };
                        if (v1 instanceof Data_List.Cons && v1.value0 instanceof Failed) {
                            return Data_Function.apply(Control_Monad_Eff_Exception.throwException)(Data_Function.apply(Control_Monad_Eff_Exception.error)("Test " + (Data_Show.show(Data_Show.showInt)(v) + (" failed: \n" + v1.value0.value0))));
                        };
                        if (v1 instanceof Data_List.Cons) {
                            var __tco_v = v + 1 | 0;
                            var __tco_v1 = v1.value1;
                            v = __tco_v;
                            v1 = __tco_v1;
                            continue tco;
                        };
                        throw new Error("Failed pattern match at Test.QuickCheck line 57, column 3 - line 57, column 40: " + [ v.constructor.name, v1.constructor.name ]);
                    };
                };
            };
            var countSuccesses = function (v) {
                if (v instanceof Data_List.Nil) {
                    return 0;
                };
                if (v instanceof Data_List.Cons && v.value0 instanceof Success) {
                    return 1 + countSuccesses(v.value1) | 0;
                };
                if (v instanceof Data_List.Cons) {
                    return countSuccesses(v.value1);
                };
                throw new Error("Failed pattern match at Test.QuickCheck line 62, column 3 - line 62, column 28: " + [ v.constructor.name ]);
            };
            return function __do() {
                var v = Test_QuickCheck_LCG.randomSeed();
                var results = quickCheckPure(dictTestable)(v)(n)(prop);
                var successes = countSuccesses(results);
                Data_Function.apply(Control_Monad_Eff_Console.log)(Data_Show.show(Data_Show.showInt)(successes) + ("/" + (Data_Show.show(Data_Show.showInt)(n) + " test(s) passed.")))();
                return throwOnFirstFailure(1)(results)();
            };
        };
    };
};
var quickCheck = function (dictTestable) {
    return function (prop) {
        return quickCheck$prime(dictTestable)(100)(prop);
    };
};
var assertNotEquals = function (dictEq) {
    return function (dictShow) {
        return function (a) {
            return function (b) {
                return withHelp(Data_Eq.notEq(dictEq)(a)(b))(Data_Show.show(dictShow)(a) + (" == " + Data_Show.show(dictShow)(b)));
            };
        };
    };
};
var assertEquals = function (dictEq) {
    return function (dictShow) {
        return function (a) {
            return function (b) {
                return withHelp(Data_Eq.eq(dictEq)(a)(b))(Data_Show.show(dictShow)(a) + (" /= " + Data_Show.show(dictShow)(b)));
            };
        };
    };
};
module.exports = {
    Success: Success, 
    Failed: Failed, 
    Testable: Testable, 
    assertEquals: assertEquals, 
    assertNotEquals: assertNotEquals, 
    quickCheck: quickCheck, 
    "quickCheck'": quickCheck$prime, 
    quickCheckPure: quickCheckPure, 
    test: test, 
    withHelp: withHelp, 
    testableResult: testableResult, 
    testableBoolean: testableBoolean, 
    testableFunction: testableFunction, 
    showResult: showResult
};

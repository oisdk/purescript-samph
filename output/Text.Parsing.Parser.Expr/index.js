// Generated by psc version 0.9.1
"use strict";
var Prelude = require("../Prelude");
var Control_Alt = require("../Control.Alt");
var Data_Foldable = require("../Data.Foldable");
var Data_List = require("../Data.List");
var Text_Parsing_Parser = require("../Text.Parsing.Parser");
var Text_Parsing_Parser_Combinators = require("../Text.Parsing.Parser.Combinators");
var Control_Bind = require("../Control.Bind");
var Control_Applicative = require("../Control.Applicative");
var Control_Category = require("../Control.Category");
var AssocNone = (function () {
    function AssocNone() {

    };
    AssocNone.value = new AssocNone();
    return AssocNone;
})();
var AssocLeft = (function () {
    function AssocLeft() {

    };
    AssocLeft.value = new AssocLeft();
    return AssocLeft;
})();
var AssocRight = (function () {
    function AssocRight() {

    };
    AssocRight.value = new AssocRight();
    return AssocRight;
})();
var Infix = (function () {
    function Infix(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Infix.create = function (value0) {
        return function (value1) {
            return new Infix(value0, value1);
        };
    };
    return Infix;
})();
var Prefix = (function () {
    function Prefix(value0) {
        this.value0 = value0;
    };
    Prefix.create = function (value0) {
        return new Prefix(value0);
    };
    return Prefix;
})();
var Postfix = (function () {
    function Postfix(value0) {
        this.value0 = value0;
    };
    Postfix.create = function (value0) {
        return new Postfix(value0);
    };
    return Postfix;
})();
var termP = function (dictMonad) {
    return function (prefixP) {
        return function (term) {
            return function (postfixP) {
                return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(prefixP)(function (v) {
                    return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(term)(function (v1) {
                        return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(postfixP)(function (v2) {
                            return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(v2(v(v1)));
                        });
                    });
                });
            };
        };
    };
};
var splitOp = function (v) {
    return function (accum) {
        if (v instanceof Infix && v.value1 instanceof AssocNone) {
            var $25 = {};
            for (var $26 in accum) {
                if (accum.hasOwnProperty($26)) {
                    $25[$26] = accum[$26];
                };
            };
            $25.nassoc = new Data_List.Cons(v.value0, accum.nassoc);
            return $25;
        };
        if (v instanceof Infix && v.value1 instanceof AssocLeft) {
            var $29 = {};
            for (var $30 in accum) {
                if (accum.hasOwnProperty($30)) {
                    $29[$30] = accum[$30];
                };
            };
            $29.lassoc = new Data_List.Cons(v.value0, accum.lassoc);
            return $29;
        };
        if (v instanceof Infix && v.value1 instanceof AssocRight) {
            var $33 = {};
            for (var $34 in accum) {
                if (accum.hasOwnProperty($34)) {
                    $33[$34] = accum[$34];
                };
            };
            $33.rassoc = new Data_List.Cons(v.value0, accum.rassoc);
            return $33;
        };
        if (v instanceof Prefix) {
            var $37 = {};
            for (var $38 in accum) {
                if (accum.hasOwnProperty($38)) {
                    $37[$38] = accum[$38];
                };
            };
            $37.prefix = new Data_List.Cons(v.value0, accum.prefix);
            return $37;
        };
        if (v instanceof Postfix) {
            var $40 = {};
            for (var $41 in accum) {
                if (accum.hasOwnProperty($41)) {
                    $40[$41] = accum[$41];
                };
            };
            $40.postfix = new Data_List.Cons(v.value0, accum.postfix);
            return $40;
        };
        throw new Error("Failed pattern match at Text.Parsing.Parser.Expr line 70, column 1 - line 70, column 76: " + [ v.constructor.name, accum.constructor.name ]);
    };
};
var rassocP1 = function (dictMonad) {
    return function (x) {
        return function (rassocOp) {
            return function (prefixP) {
                return function (term) {
                    return function (postfixP) {
                        return Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(rassocP(dictMonad)(x)(rassocOp)(prefixP)(term)(postfixP))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(x));
                    };
                };
            };
        };
    };
};
var rassocP = function (dictMonad) {
    return function (x) {
        return function (rassocOp) {
            return function (prefixP) {
                return function (term) {
                    return function (postfixP) {
                        return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(rassocOp)(function (v) {
                            return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(termP(dictMonad)(prefixP)(term)(postfixP))(function (v1) {
                                return rassocP1(dictMonad)(v1)(rassocOp)(prefixP)(term)(postfixP);
                            }))(function (v1) {
                                return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(v(x)(v1));
                            });
                        });
                    };
                };
            };
        };
    };
};
var nassocP = function (dictMonad) {
    return function (x) {
        return function (nassocOp) {
            return function (prefixP) {
                return function (term) {
                    return function (postfixP) {
                        return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(nassocOp)(function (v) {
                            return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(termP(dictMonad)(prefixP)(term)(postfixP))(function (v1) {
                                return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(v(x)(v1));
                            });
                        });
                    };
                };
            };
        };
    };
};
var lassocP1 = function (dictMonad) {
    return function (x) {
        return function (lassocOp) {
            return function (prefixP) {
                return function (term) {
                    return function (postfixP) {
                        return Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(lassocP(dictMonad)(x)(lassocOp)(prefixP)(term)(postfixP))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(x));
                    };
                };
            };
        };
    };
};
var lassocP = function (dictMonad) {
    return function (x) {
        return function (lassocOp) {
            return function (prefixP) {
                return function (term) {
                    return function (postfixP) {
                        return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(lassocOp)(function (v) {
                            return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(termP(dictMonad)(prefixP)(term)(postfixP))(function (v1) {
                                return lassocP1(dictMonad)(v(x)(v1))(lassocOp)(prefixP)(term)(postfixP);
                            });
                        });
                    };
                };
            };
        };
    };
};
var makeParser = function (dictMonad) {
    return function (term) {
        return function (ops) {
            var accum = Data_Foldable.foldr(Data_Foldable.foldableArray)(splitOp)({
                rassoc: Data_List.Nil.value, 
                lassoc: Data_List.Nil.value, 
                nassoc: Data_List.Nil.value, 
                prefix: Data_List.Nil.value, 
                postfix: Data_List.Nil.value
            })(ops);
            var lassocOp = Text_Parsing_Parser_Combinators.choice(Data_List.foldableList)(dictMonad)(accum.lassoc);
            var nassocOp = Text_Parsing_Parser_Combinators.choice(Data_List.foldableList)(dictMonad)(accum.nassoc);
            var postfixOp = Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Text_Parsing_Parser_Combinators.choice(Data_List.foldableList)(dictMonad)(accum.postfix))("");
            var postfixP = Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(postfixOp)(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(Control_Category.id(Control_Category.categoryFn)));
            var prefixOp = Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Text_Parsing_Parser_Combinators.choice(Data_List.foldableList)(dictMonad)(accum.prefix))("");
            var prefixP = Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(prefixOp)(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(Control_Category.id(Control_Category.categoryFn)));
            var rassocOp = Text_Parsing_Parser_Combinators.choice(Data_List.foldableList)(dictMonad)(accum.rassoc);
            return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(termP(dictMonad)(prefixP)(term)(postfixP))(function (v) {
                return Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(rassocP(dictMonad)(v)(rassocOp)(prefixP)(term)(postfixP))(lassocP(dictMonad)(v)(lassocOp)(prefixP)(term)(postfixP)))(nassocP(dictMonad)(v)(nassocOp)(prefixP)(term)(postfixP)))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(v)))("operator");
            });
        };
    };
};
var buildExprParser = function (dictMonad) {
    return function (operators) {
        return function (simpleExpr) {
            return Data_Foldable.foldl(Data_Foldable.foldableArray)(makeParser(dictMonad))(simpleExpr)(operators);
        };
    };
};
module.exports = {
    AssocNone: AssocNone, 
    AssocLeft: AssocLeft, 
    AssocRight: AssocRight, 
    Infix: Infix, 
    Prefix: Prefix, 
    Postfix: Postfix, 
    buildExprParser: buildExprParser
};
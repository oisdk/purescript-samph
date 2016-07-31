
/**
 *  | Helper functions for working with immutable Javascript arrays.
 *  |
 *  | _Note_: Depending on your use-case, you may prefer to use `Data.List` or
 *  | `Data.Sequence` instead, which might give better performance for certain
 *  | use cases. This module is useful when integrating with JavaScript libraries
 *  | which use arrays, but immutable arrays are not a practical data structure
 *  | for many use cases due to their poor asymptotics.
 *  |
 *  | In addition to the functions in this module, Arrays have a number of
 *  | useful instances:
 *  |
 *  | * `Functor`, which provides `map :: forall a b. (a -> b) -> Array a ->
 *  |   Array b`
 *  | * `Apply`, which provides `(<*>) :: forall a b. Array (a -> b) -> Array a
 *  |   -> Array b`. This function works a bit like a Cartesian product; the
 *  |   result array is constructed by applying each function in the first
 *  |   array to each value in the second, so that the result array ends up with
 *  |   a length equal to the product of the two arguments' lengths.
 *  | * `Bind`, which provides `(>>=) :: forall a b. (a -> Array b) -> Array a
 *  |   -> Array b` (this is the same as `concatMap`).
 *  | * `Semigroup`, which provides `(<>) :: forall a. Array a -> Array a ->
 *  |   Array a`, for concatenating arrays.
 *  | * `Foldable`, which provides a slew of functions for *folding* (also known
 *  |   as *reducing*) arrays down to one value. For example,
 *  |   `Data.Foldable.or` tests whether an array of `Boolean` values contains
 *  |   at least one `true` value.
 *  | * `Traversable`, which provides the PureScript version of a for-loop,
 *  |   allowing you to iterate over an array and accumulate effects.
 *  |
 */
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Lazy = require("../Control.Lazy");
var Data_Foldable = require("../Data.Foldable");
var Data_Maybe = require("../Data.Maybe");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Data_Unfoldable = require("../Data.Unfoldable");
var Partial_Unsafe = require("../Partial.Unsafe");
var Data_Function = require("../Data.Function");
var Data_Ordering = require("../Data.Ordering");
var Data_Ring = require("../Data.Ring");
var Data_Ord = require("../Data.Ord");
var Data_Eq = require("../Data.Eq");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Control_Apply = require("../Control.Apply");
var Data_Functor = require("../Data.Functor");
var Control_Applicative = require("../Control.Applicative");
var Data_Boolean = require("../Data.Boolean");
var Data_Semiring = require("../Data.Semiring");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Bind = require("../Control.Bind");
var Data_Semigroup = require("../Data.Semigroup");
var Control_Category = require("../Control.Category");

/**
 *  | A generalization of `zipWith` which accumulates results in some `Applicative`
 *  | functor.
 */
var zipWithA = function (dictApplicative) {
    return function (f) {
        return function (xs) {
            return function (ys) {
                return Data_Traversable.sequence(Data_Traversable.traversableArray)(dictApplicative)($foreign.zipWith(f)(xs)(ys));
            };
        };
    };
};

/**
 *  | Rakes two lists and returns a list of corresponding pairs.
 *  | If one input list is short, excess elements of the longer list are discarded.
 */
var zip = $foreign.zipWith(Data_Tuple.Tuple.create);

/**
 *  | Change the element at the specified index, creating a new array, or
 *  | returning `Nothing` if the index is out of bounds.
 */
var updateAt = $foreign._updateAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var unzip = $foreign["uncons'"](function (v) {
    return new Data_Tuple.Tuple([  ], [  ]);
})(function (v) {
    return function (ts) {
        var $39 = unzip(ts);
        return new Data_Tuple.Tuple($foreign.cons(v.value0)($39.value0), $foreign.cons(v.value1)($39.value1));
    };
});

/**
 *  | Break an array into its first element and remaining elements.
 *  |
 *  | Using `uncons` provides a way of writing code that would use cons patterns
 *  | in Haskell or pre-PureScript 0.7:
 *  | ``` purescript
 *  | f (x : xs) = something
 *  | f [] = somethingElse
 *  | ```
 *  | Becomes:
 *  | ``` purescript
 *  | f arr = case uncons arr of
 *  |   Just { head: x, tail: xs } -> something
 *  |   Nothing -> somethingElse
 *  | ```
 */
var uncons = $foreign["uncons'"](Data_Function["const"](Data_Maybe.Nothing.value))(function (x) {
    return function (xs) {
        return new Data_Maybe.Just({
            head: x, 
            tail: xs
        });
    };
});

/**
 *  | Convert an `Array` into an `Unfoldable` structure.
 */
var toUnfoldable = function (dictUnfoldable) {
    return Data_Function.apply(Data_Unfoldable.unfoldr(dictUnfoldable))($foreign["uncons'"](Data_Function["const"](Data_Maybe.Nothing.value))(function (h) {
        return function (t) {
            return new Data_Maybe.Just(new Data_Tuple.Tuple(h, t));
        };
    }));
};

/**
 *  | Get all but the first element of an array, creating a new array, or `Nothing` if the array is empty
 *  |
 *  | Running time: `O(n)` where `n` is the length of the array
 */
var tail = $foreign["uncons'"](Data_Function["const"](Data_Maybe.Nothing.value))(function (v) {
    return function (xs) {
        return new Data_Maybe.Just(xs);
    };
});

/**
 *  | Split an array into two parts:
 *  |
 *  | 1. the longest initial subarray for which all element satisfy the specified
 *  |    predicate
 *  | 2. the remaining elements
 *  |
 *  | ```purescript
 *  | span (\n -> n % 2 == 1) [1,3,2,4,5] == { init: [1,3], rest: [2,4,5] }
 *  | ```
 */
var span = function (p) {
    var go = function (__copy_acc) {
        return function (__copy_xs) {
            var acc = __copy_acc;
            var xs = __copy_xs;
            tco: while (true) {
                var $45 = uncons(xs);
                if ($45 instanceof Data_Maybe.Just && p($45.value0.head)) {
                    var __tco_acc = $foreign.cons($45.value0.head)(acc);
                    acc = __tco_acc;
                    xs = $45.value0.tail;
                    continue tco;
                };
                return {
                    init: $foreign.reverse(acc), 
                    rest: xs
                };
            };
        };
    };
    return go([  ]);
};

/**
 *  | Calculate the longest initial subarray for which all element satisfy the
 *  | specified predicate, creating a new array.
 */
var takeWhile = function (p) {
    return function (xs) {
        return (span(p)(xs)).init;
    };
};

/**
 *  | Sort the elements of an array in increasing order, where elements are compared using
 *  | the specified partial ordering, creating a new array.
 */
var sortBy = function (comp) {
    return function (xs) {
        var comp$prime = function (x) {
            return function (y) {
                var $49 = comp(x)(y);
                if ($49 instanceof Data_Ordering.GT) {
                    return 1;
                };
                if ($49 instanceof Data_Ordering.EQ) {
                    return 0;
                };
                if ($49 instanceof Data_Ordering.LT) {
                    return -1;
                };
                throw new Error("Failed pattern match at Data.Array line 436, column 15 - line 441, column 1: " + [ $49.constructor.name ]);
            };
        };
        return $foreign.sortImpl(comp$prime)(xs);
    };
};

/**
 * ------------------------------------------------------------------------------
 *  Sorting ---------------------------------------------------------------------
 * ------------------------------------------------------------------------------
 *  | Sort the elements of an array in increasing order, creating a new array.
 */
var sort = function (dictOrd) {
    return function (xs) {
        return sortBy(Data_Ord.compare(dictOrd))(xs);
    };
};

/**
 *  | Create an array of one element
 */
var singleton = function (a) {
    return [ a ];
};

/**
 * ------------------------------------------------------------------------------
 *  Array size ------------------------------------------------------------------
 * ------------------------------------------------------------------------------
 *  | Test whether an array is empty.
 */
var $$null = function (xs) {
    return $foreign.length(xs) === 0;
};
var nubBy = function (eq) {
    return function (xs) {
        var $50 = uncons(xs);
        if ($50 instanceof Data_Maybe.Just) {
            return $foreign.cons($50.value0.head)(nubBy(eq)($foreign.filter(function (y) {
                return !eq($50.value0.head)(y);
            })($50.value0.tail)));
        };
        if ($50 instanceof Data_Maybe.Nothing) {
            return [  ];
        };
        throw new Error("Failed pattern match at Data.Array line 525, column 3 - line 527, column 18: " + [ $50.constructor.name ]);
    };
};

/**
 *  | Remove the duplicates from an array, creating a new array.
 */
var nub = function (dictEq) {
    return nubBy(Data_Eq.eq(dictEq));
};

/**
 *  | Apply a function to each element in an array, supplying a generated
 *  | zero-based index integer along with the element, creating an array
 *  | with the new elements.
 */
var mapWithIndex = function (f) {
    return function (xs) {
        return $foreign.zipWith(f)($foreign.range(0)($foreign.length(xs) - 1))(xs);
    };
};
var some = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Control_Apply.apply((dictAlternative["__superclass_Control.Applicative.Applicative_0"]())["__superclass_Control.Apply.Apply_0"]())(Data_Functor.map(((dictAlternative["__superclass_Control.Plus.Plus_1"]())["__superclass_Control.Alt.Alt_0"]())["__superclass_Data.Functor.Functor_0"]())($foreign.cons)(v))(Control_Lazy.defer(dictLazy)(function (v1) {
                return many(dictAlternative)(dictLazy)(v);
            }));
        };
    };
};
var many = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Control_Alt.alt((dictAlternative["__superclass_Control.Plus.Plus_1"]())["__superclass_Control.Alt.Alt_0"]())(some(dictAlternative)(dictLazy)(v))(Control_Applicative.pure(dictAlternative["__superclass_Control.Applicative.Applicative_0"]())([  ]));
        };
    };
};

/**
 *  | Insert an element at the specified index, creating a new array, or
 *  | returning `Nothing` if the index is out of bounds.
 */
var insertAt = $foreign._insertAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);

/**
 *  | Get all but the last element of an array, creating a new array, or `Nothing` if the array is empty.
 *  |
 *  | Running time: `O(n)` where `n` is the length of the array
 */
var init = function (xs) {
    if ($$null(xs)) {
        return Data_Maybe.Nothing.value;
    };
    if (Data_Boolean.otherwise) {
        return new Data_Maybe.Just($foreign.slice(0)($foreign.length(xs) - 1)(xs));
    };
    throw new Error("Failed pattern match at Data.Array line 227, column 1 - line 229, column 55: " + [ xs.constructor.name ]);
};

/**
 * ------------------------------------------------------------------------------
 *  Indexed operations ----------------------------------------------------------
 * ------------------------------------------------------------------------------
 *  | This function provides a safe way to read a value at a particular index
 *  | from an array.
 */
var index = $foreign.indexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);

/**
 *  | Get the last element in an array, or `Nothing` if the array is empty
 *  |
 *  | Running time: `O(1)`.
 */
var last = function (xs) {
    return index(xs)($foreign.length(xs) - 1);
};

/**
 *  | Apply a function to the element at the specified index, creating a new
 *  | array, or returning `Nothing` if the index is out of bounds.
 */
var modifyAt = function (i) {
    return function (f) {
        return function (xs) {
            var go = function (x) {
                return updateAt(i)(f(x))(xs);
            };
            return Data_Maybe.maybe(Data_Maybe.Nothing.value)(go)(index(xs)(i));
        };
    };
};

/**
 * ------------------------------------------------------------------------------
 *  Non-indexed reads -----------------------------------------------------------
 * ------------------------------------------------------------------------------
 *  | Get the first element in an array, or `Nothing` if the array is empty
 *  |
 *  | Running time: `O(1)`.
 */
var head = $foreign["uncons'"](Data_Function["const"](Data_Maybe.Nothing.value))(function (x) {
    return function (v) {
        return new Data_Maybe.Just(x);
    };
});

/**
 *  | Group equal, consecutive elements of an array into arrays, using the
 *  | specified equivalence relation to detemine equality.
 */
var groupBy = function (op) {
    var go = function (__copy_acc) {
        return function (__copy_xs) {
            var acc = __copy_acc;
            var xs = __copy_xs;
            tco: while (true) {
                var $55 = uncons(xs);
                if ($55 instanceof Data_Maybe.Just) {
                    var sp = span(op($55.value0.head))($55.value0.tail);
                    var __tco_acc = $foreign.cons($foreign.cons($55.value0.head)(sp.init))(acc);
                    acc = __tco_acc;
                    xs = sp.rest;
                    continue tco;
                };
                if ($55 instanceof Data_Maybe.Nothing) {
                    return $foreign.reverse(acc);
                };
                throw new Error("Failed pattern match at Data.Array line 511, column 15 - line 515, column 27: " + [ $55.constructor.name ]);
            };
        };
    };
    return go([  ]);
};

/**
 *  | Group equal, consecutive elements of an array into arrays.
 *  |
 *  | ```purescript
 *  | group [1,1,2,2,1] == [[1,1],[2,2],[1]]
 *  | ```
 */
var group = function (dictEq) {
    return function (xs) {
        return groupBy(Data_Eq.eq(dictEq))(xs);
    };
};

/**
 *  | Sort and then group the elements of an array into arrays.
 *  |
 *  | ```purescript
 *  | group' [1,1,2,2,1] == [[1,1,1],[2,2]]
 *  | ```
 */
var group$prime = function (dictOrd) {
    return function ($69) {
        return group(dictOrd["__superclass_Data.Eq.Eq_0"]())(sort(dictOrd)($69));
    };
};

/**
 *  | Convert a `Foldable` structure into an `Array`.
 */
var fromFoldable = function (dictFoldable) {
    return $foreign.fromFoldableImpl(Data_Foldable.foldr(dictFoldable));
};
var foldM = function (dictMonad) {
    return function (f) {
        return function (a) {
            return $foreign["uncons'"](function (v) {
                return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())(a);
            })(function (b) {
                return function (bs) {
                    return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(f(a)(b))(function (a$prime) {
                        return foldM(dictMonad)(f)(a$prime)(bs);
                    });
                };
            });
        };
    };
};

/**
 *  | Find the last index for which a predicate holds.
 */
var findLastIndex = $foreign.findLastIndexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);

/**
 *  | Insert an element into a sorted array, using the specified function to
 *  | determine the ordering of elements.
 */
var insertBy = function (cmp) {
    return function (x) {
        return function (ys) {
            var i = Data_Maybe.maybe(0)(function (v) {
                return v + 1 | 0;
            })(findLastIndex(function (y) {
                return Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(x)(y))(Data_Ordering.GT.value);
            })(ys));
            return Partial_Unsafe.unsafePartial(function (dictPartial) {
                return Data_Maybe.fromJust(dictPartial)(insertAt(i)(x)(ys));
            });
        };
    };
};

/**
 *  | Insert an element into a sorted array.
 */
var insert = function (dictOrd) {
    return insertBy(Data_Ord.compare(dictOrd));
};

/**
 *  | Find the first index for which a predicate holds.
 */
var findIndex = $foreign.findIndexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);

/**
 *  | Calculate the intersection of two arrays, using the specified equivalence
 *  | relation to compare elements, creating a new array. Note that duplicates
 *  | in the first array are preserved while duplicates in the second array are
 *  | removed.
 */
var intersectBy = function (eq) {
    return function (xs) {
        return function (ys) {
            return $foreign.filter(function (x) {
                return Data_Maybe.isJust(findIndex(eq(x))(ys));
            })(xs);
        };
    };
};

/**
 *  | Calculate the intersection of two arrays, creating a new array. Note that
 *  | duplicates in the first array are preserved while duplicates in the second
 *  | array are removed.
 */
var intersect = function (dictEq) {
    return intersectBy(Data_Eq.eq(dictEq));
};
var filterM = function (dictMonad) {
    return function (p) {
        return $foreign["uncons'"](function (v) {
            return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())([  ]);
        })(function (x) {
            return function (xs) {
                return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(p(x))(function (v) {
                    return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(filterM(dictMonad)(p)(xs))(function (v1) {
                        return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())((function () {
                            if (v) {
                                return $foreign.cons(x)(v1);
                            };
                            if (!v) {
                                return v1;
                            };
                            throw new Error("Failed pattern match at Data.Array line 403, column 3 - line 403, column 34: " + [ v.constructor.name ]);
                        })());
                    });
                });
            };
        });
    };
};

/**
 *  | Find the index of the last element equal to the specified element.
 */
var elemLastIndex = function (dictEq) {
    return function (x) {
        return findLastIndex(function (v) {
            return Data_Eq.eq(dictEq)(v)(x);
        });
    };
};

/**
 *  | Find the index of the first element equal to the specified element.
 */
var elemIndex = function (dictEq) {
    return function (x) {
        return findIndex(function (v) {
            return Data_Eq.eq(dictEq)(v)(x);
        });
    };
};

/**
 *  | Remove the longest initial subarray for which all element satisfy the
 *  | specified predicate, creating a new array.
 */
var dropWhile = function (p) {
    return function (xs) {
        return (span(p)(xs)).rest;
    };
};

/**
 *  | Delete the element at the specified index, creating a new array, or
 *  | returning `Nothing` if the index is out of bounds.
 */
var deleteAt = $foreign._deleteAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);

/**
 *  | Delete the first element of an array which matches the specified value,
 *  | under the equivalence relation provided in the first argument, creating a
 *  | new array.
 */
var deleteBy = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2.length === 0) {
                return [  ];
            };
            return Data_Maybe.maybe(v2)(function (i) {
                return Data_Function.apply(Partial_Unsafe.unsafePartial)(function (dictPartial) {
                    return Data_Maybe.fromJust(dictPartial)(deleteAt(i)(v2));
                });
            })(findIndex(v(v1))(v2));
        };
    };
};

/**
 *  | Calculate the union of two arrays, using the specified function to
 *  | determine equality of elements. Note that duplicates in the first array
 *  | are preserved while duplicates in the second array are removed.
 */
var unionBy = function (eq) {
    return function (xs) {
        return function (ys) {
            return Data_Semigroup.append(Data_Semigroup.semigroupArray)(xs)(Data_Foldable.foldl(Data_Foldable.foldableArray)(Data_Function.flip(deleteBy(eq)))(nubBy(eq)(ys))(xs));
        };
    };
};

/**
 *  | Calculate the union of two arrays. Note that duplicates in the first array
 *  | are preserved while duplicates in the second array are removed.
 *  |
 *  | Running time: `O(n^2)`
 */
var union = function (dictEq) {
    return unionBy(Data_Eq.eq(dictEq));
};

/**
 *  | Delete the first element of an array which is equal to the specified value,
 *  | creating a new array.
 */
var $$delete = function (dictEq) {
    return deleteBy(Data_Eq.eq(dictEq));
};
var difference = function (dictEq) {
    return function (xs) {
        return function (ys) {
            if ($$null(xs)) {
                return [  ];
            };
            if (Data_Boolean.otherwise) {
                return $foreign["uncons'"](Data_Function["const"](xs))(function (z) {
                    return function (zs) {
                        return difference(dictEq)($$delete(dictEq)(z)(xs))(zs);
                    };
                })(ys);
            };
            throw new Error("Failed pattern match at Data.Array line 557, column 1 - line 559, column 67: " + [ xs.constructor.name, ys.constructor.name ]);
        };
    };
};

/**
 *  | Apply a function to each element in an array, and flatten the results
 *  | into a single, new array.
 */
var concatMap = Data_Function.flip(Control_Bind.bind(Control_Bind.bindArray));

/**
 *  | Apply a function to each element in an array, keeping only the results
 *  | which contain a value, creating a new array.
 */
var mapMaybe = function (f) {
    return concatMap(function ($70) {
        return Data_Maybe.maybe([  ])(singleton)(f($70));
    });
};

/**
 *  | Filter an array of optional values, keeping only the elements which contain
 *  | a value, creating a new array.
 */
var catMaybes = mapMaybe(Control_Category.id(Control_Category.categoryFn));

/**
 *  | Update or delete the element at the specified index by applying a
 *  | function to the current value, returning a new array or `Nothing` if the
 *  | index is out-of-bounds.
 */
var alterAt = function (i) {
    return function (f) {
        return function (xs) {
            var go = function (x) {
                var $67 = f(x);
                if ($67 instanceof Data_Maybe.Nothing) {
                    return deleteAt(i)(xs);
                };
                if ($67 instanceof Data_Maybe.Just) {
                    return updateAt(i)($67.value0)(xs);
                };
                throw new Error("Failed pattern match at Data.Array line 361, column 10 - line 363, column 32: " + [ $67.constructor.name ]);
            };
            return Data_Maybe.maybe(Data_Maybe.Nothing.value)(go)(index(xs)(i));
        };
    };
};
module.exports = {
    alterAt: alterAt, 
    catMaybes: catMaybes, 
    concatMap: concatMap, 
    "delete": $$delete, 
    deleteAt: deleteAt, 
    deleteBy: deleteBy, 
    difference: difference, 
    dropWhile: dropWhile, 
    elemIndex: elemIndex, 
    elemLastIndex: elemLastIndex, 
    filterM: filterM, 
    findIndex: findIndex, 
    findLastIndex: findLastIndex, 
    foldM: foldM, 
    fromFoldable: fromFoldable, 
    group: group, 
    "group'": group$prime, 
    groupBy: groupBy, 
    head: head, 
    index: index, 
    init: init, 
    insert: insert, 
    insertAt: insertAt, 
    insertBy: insertBy, 
    intersect: intersect, 
    intersectBy: intersectBy, 
    last: last, 
    many: many, 
    mapMaybe: mapMaybe, 
    mapWithIndex: mapWithIndex, 
    modifyAt: modifyAt, 
    nub: nub, 
    nubBy: nubBy, 
    "null": $$null, 
    singleton: singleton, 
    some: some, 
    sort: sort, 
    sortBy: sortBy, 
    span: span, 
    tail: tail, 
    takeWhile: takeWhile, 
    toUnfoldable: toUnfoldable, 
    uncons: uncons, 
    union: union, 
    unionBy: unionBy, 
    unzip: unzip, 
    updateAt: updateAt, 
    zip: zip, 
    zipWithA: zipWithA, 
    concat: $foreign.concat, 
    cons: $foreign.cons, 
    drop: $foreign.drop, 
    filter: $foreign.filter, 
    length: $foreign.length, 
    partition: $foreign.partition, 
    range: $foreign.range, 
    reverse: $foreign.reverse, 
    slice: $foreign.slice, 
    snoc: $foreign.snoc, 
    take: $foreign.take, 
    zipWith: $foreign.zipWith
};

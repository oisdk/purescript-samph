module Data.Equivalence where

import Data.HeytingAlgebra ((&&))
import Data.Comparison (Comparison(..))
import Data.Eq (class Eq, eq, (==))
import Data.Function (on)
import Data.Functor.Contravariant (class Contravariant)
import Data.Monoid (class Monoid)
import Data.Ordering (Ordering(..))
import Data.Semigroup (class Semigroup)

-- | An adaptor allowing `>$<` to map over the inputs of an equivalence
-- | relation.
newtype Equivalence a = Equivalence (a -> a -> Boolean)

runEquivalence :: forall a. Equivalence a -> a -> a -> Boolean
runEquivalence (Equivalence a) = a

instance contravariantEquivalence :: Contravariant Equivalence where
  cmap f (Equivalence g) = Equivalence (g `on` f)

instance semigroupEquivalence :: Semigroup (Equivalence a) where
  append (Equivalence p) (Equivalence q) = Equivalence (\a b -> p a b && q a b)

instance monoidEquivalence :: Monoid (Equivalence a) where
  mempty = Equivalence (\_ _ -> true)

-- | The default equivalence relation for any values with an `Eq` instance.
defaultEquivalence :: forall a. Eq a => Equivalence a
defaultEquivalence = Equivalence eq

-- | An equivalence relation for any `Comparison`.
comparisonEquivalence :: forall a. Comparison a -> Equivalence a
comparisonEquivalence (Comparison p) = Equivalence (\a b -> p a b == EQ)

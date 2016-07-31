module Control.Monad.Rec.Class
  ( class MonadRec
  , tailRec
  , tailRecM
  , tailRecM2
  , tailRecM3
  , forever
  ) where

import Prelude (class Monad, unit, (<$), (<$>), ($), pure, bind, (<<<))

import Control.Monad.Eff (Eff(), untilE)
import Control.Monad.Eff.Unsafe as U
import Control.Monad.ST (ST(), runST, newSTRef, readSTRef, writeSTRef)

import Data.Either (Either(..), fromRight)
import Data.Identity (Identity(..), runIdentity)

import Partial.Unsafe (unsafePartial)

-- | This type class captures those monads which support tail recursion in constant stack space.
-- |
-- | The `tailRecM` function takes a step function, and applies that step function recursively
-- | until a pure value of type `b` is found.
-- |
-- | Instances are provided for standard monad transformers.
-- |
-- | For example:
-- |
-- | ```purescript
-- | loopWriter :: Number -> WriterT Sum (Eff (trace :: Trace)) Unit
-- | loopWriter n = tailRecM go n
-- |   where
-- |   go 0 = do
-- |     lift $ trace "Done!"
-- |     pure (Right unit)
-- |   go n = do
-- |     tell $ Sum n
-- |     pure (Left (n - 1))
-- | ```
class Monad m <= MonadRec m where
  tailRecM :: forall a b. (a -> m (Either a b)) -> a -> m b

-- | Create a tail-recursive function of two arguments which uses constant stack space.
tailRecM2
  :: forall m a b c
   . MonadRec m
  => (a -> b -> m (Either { a :: a, b :: b } c))
  -> a
  -> b
  -> m c
tailRecM2 f a b = tailRecM (\o -> f o.a o.b) { a, b }

-- | Create a tail-recursive function of three arguments which uses constant stack space.
tailRecM3
  :: forall m a b c d
   . MonadRec m
  => (a -> b -> c -> m (Either { a :: a, b :: b, c :: c } d))
  -> a
  -> b
  -> c
  -> m d
tailRecM3 f a b c = tailRecM (\o -> f o.a o.b o.c) { a, b, c }

-- | Create a pure tail-recursive function of one argument
-- |
-- | For example:
-- |
-- | ```purescript
-- | pow :: Number -> Number -> Number
-- | pow n p = tailRec go { accum: 1, power: p }
-- |   where
-- |   go :: _ -> Either _ Number
-- |   go { accum: acc, power: 0 } = Right acc
-- |   go { accum: acc, power: p } = Left { accum: acc * n, power: p - 1 }
-- | ```
tailRec :: forall a b. (a -> Either a b) -> a -> b
tailRec f a = go (f a)
  where
  go (Left a) = go (f a)
  go (Right b) = b

instance monadRecIdentity :: MonadRec Identity where
  tailRecM f = Identity <<< tailRec (runIdentity <<< f)

instance monadRecEff :: MonadRec (Eff eff) where
  tailRecM = tailRecEff

instance monadRecEither :: MonadRec (Either e) where
  tailRecM f a0 =
    let
      g (Left e) = Right (Left e)
      g (Right (Left a)) =  Left (f a)
      g (Right (Right b)) =  Right (Right b)
    in tailRec g (f a0)

tailRecEff :: forall a b eff. (a -> Eff eff (Either a b)) -> a -> Eff eff b
tailRecEff f a = runST do
  e <- f' a
  r <- newSTRef e
  untilE do
    e' <- readSTRef r
    case e' of
      Left a' -> do
        e'' <- f' a'
        writeSTRef r e''
        pure false
      Right b -> pure true
  unsafePartial $ fromRight <$> readSTRef r
  where
  f' :: forall h. a -> Eff (st :: ST h | eff) (Either a b)
  f' = U.unsafeInterleaveEff <<< f

-- | `forever` runs an action indefinitely, using the `MonadRec` instance to
-- | ensure constant stack usage.
-- |
-- | For example:
-- |
-- | ```purescript
-- | main = forever $ trace "Hello, World!"
-- | ```
forever :: forall m a b. MonadRec m => m a -> m b
forever ma = tailRecM (\u -> Left u <$ ma) unit
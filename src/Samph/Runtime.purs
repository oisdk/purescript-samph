module Samph.Runtime where

import Control.Monad.State
import Control.Monad.Error.Class
import Data.Array
import Data.Maybe
import Samph.Types
import Prelude
import Data.Int.Bits

type SamphireState =
  { ram :: Array Lit
  , al  :: Lit
  , bl  :: Lit
  , cl  :: Lit
  , dl  :: Lit
  , ip  :: Lit
  , sp  :: Lit
  , sr  :: Lit
  , io  :: Array Lit }

data RuntimeError =
  OverflowedInstruction Lit |
  CorruptedCode String String

type ErrorState a = forall m. (MonadState SamphireState m, MonadError RuntimeError m) => m a

popLit :: ErrorState Lit
popLit = do
  Lit i <- gets (_.ip)
  r <- gets (_.ram)
  case r !! i of
    Nothing -> throwError (OverflowedInstruction (Lit i))
    Just x -> do
      modify (\s -> s { ip = Lit i + Lit 1 } )
      pure x

popReg :: ErrorState Reg
popReg = do
  Lit x <- popLit
  case x of
    0 -> pure AL
    1 -> pure BL
    2 -> pure CL
    3 -> pure DL
    n -> throwError (CorruptedCode "register" (show n))

popIns :: ErrorState MachineCode
popIns = do
  Lit x <- popLit
  case x of
    0xA0 -> A0 <$> popReg <*> popReg
    0xA1 -> A1 <$> popReg <*> popReg
    0xA2 -> A2 <$> popReg <*> popReg
    0xA3 -> A3 <$> popReg <*> popReg
    0xA4 -> A4 <$> popReg
    0xA5 -> A5 <$> popReg
    0xA6 -> A6 <$> popReg <*> popReg
    0xAA -> AA <$> popReg <*> popReg
    0xAB -> AB <$> popReg <*> popReg
    0xAC -> AC <$> popReg <*> popReg
    0xAD -> AD <$> popReg
    0xA9 -> A9 <$> popReg
    0xB9 -> B9 <$> popReg
    0xC9 -> C9 <$> popReg
    0xD9 -> D9 <$> popReg
    0xB0 -> B0 <$> popReg <*> popLit
    0xB1 -> B1 <$> popReg <*> popLit
    0xB2 -> B2 <$> popReg <*> popLit
    0xB3 -> B3 <$> popReg <*> popLit
    0xB6 -> B6 <$> popReg <*> popLit
    0xBA -> BA <$> popReg <*> popLit
    0xBB -> BB <$> popReg <*> popLit
    0xBC -> BC <$> popReg <*> popLit
    0xC0 -> C0 <$> popLit
    0xC1 -> C1 <$> popLit
    0xC2 -> C2 <$> popLit
    0xC3 -> C3 <$> popLit
    0xC4 -> C4 <$> popLit
    0xC5 -> C5 <$> popLit
    0xC6 -> C6 <$> popLit
    0xCA -> CA <$> popLit
    0xCB -> pure CB
    0xCC -> CC <$> popLit
    0xCD -> pure CD
    0xE0 -> E0 <$> popReg
    0xE1 -> E1 <$> popReg
    0xEA -> pure EA
    0xEB -> pure EB
    0xF0 -> F0 <$> popLit
    0xF1 -> F1 <$> popLit
    0xFE -> pure FE
    0x00 -> pure O0
    0xFF -> pure FF
    0xFC -> pure FC
    0xFD -> pure FD
    0xD0 -> D0 <$> popReg <*> popLit
    0xD1 -> D1 <$> popReg <*> popAddrLit
    0xD2 -> D2 <$> popAddrLit <*> popReg
    0xD3 -> D3 <$> popReg <*> map AddrReg popReg
    0xD4 -> D4 <$> map AddrReg popReg <*> popReg
    0xDA -> DA <$> popReg <*> popReg
    0xDB -> DB <$> popReg <*> popLit
    0xDC -> DC <$> popReg <*> popAddrLit
    n    -> throwError (CorruptedCode "instruction" (show n))
    where popAddrLit = map (\(Lit n) -> AddrLit n) popLit

data BoundsCheck =
  Underflow | Overflow | Inbounds

data ZeroCheck =
  Zero | NonZero

type ArithResult =
  { value :: Lit
  , bounds :: BoundsCheck
  , zero :: ZeroCheck }

addCheck :: Lit -> Lit -> ArithResult
addCheck (Lit 0) (Lit 0) =
  { value: Lit 0
  , bounds: Inbounds
  , zero: Zero }
addCheck (Lit x) (Lit y) = case compare z 256 of
  LT -> {value: Lit z, bounds: Inbounds, zero: NonZero}
  GT -> {value: Lit (z .&. 255), bounds: Overflow, zero: NonZero}
  EQ -> {value: Lit 0, bounds: Overflow, zero: Zero}
  where z = x + y

mulCheck :: Lit -> Lit -> ArithResult
mulCheck (Lit 0) (Lit 0) =
  { value: Lit 0
  , bounds: Inbounds
  , zero: Zero }
mulCheck (Lit x) (Lit y) = case compare z 256 of
  LT -> {value: Lit z, bounds: Inbounds, zero: NonZero}
  GT -> {value: Lit (z .&. 255), bounds: Overflow, zero: NonZero}
  EQ -> {value: Lit 0, bounds: Overflow, zero: Zero}
  where z = x * y

subCheck :: Lit -> Lit -> ArithResult
subCheck (Lit 0) (Lit 0) =
  { value: Lit 0
  , bounds: Inbounds
  , zero: Zero }
subCheck (Lit x) (Lit y) = case compare (-1) z of
  LT -> {value: Lit z, bounds: Inbounds, zero: NonZero}
  GT -> {value: Lit (z .&. 255), bounds: Underflow, zero: NonZero}
  EQ -> {value: Lit 255, bounds: Underflow, zero: NonZero}
  where z = x - y

setUnderflow :: Lit -> Lit
setUnderflow (Lit n) = Lit ((n .|. 16) .&. complement 32)

setOverflow :: Lit -> Lit
setOverflow (Lit n) = Lit ((n .|. 32) .&. complement 16)

setInBounds :: Lit -> Lit
setInBounds (Lit n) = Lit ((n .&. complement 16) .&. complement 32)

setZero :: Lit -> Lit
setZero (Lit n) = Lit (n .|. 64)

unSetZero :: Lit -> Lit
unSetZero (Lit n) = Lit (n .&. complement 64)

setFlags :: forall m. (MonadState SamphireState m)
         => ArithResult -> m Lit
setFlags r = do
  case r.bounds of
    Underflow ->
      modify (\s -> s { sr = setUnderflow s.sr })
    Overflow ->
      modify (\s -> s { sr = setOverflow s.sr })
    Inbounds ->
      modify (\s -> s { sr = setInBounds s.sr })
  case r.zero of
    Zero ->
      modify (\s -> s { sr = setZero s.sr })
    NonZero ->
      modify (\s -> s { sr = unSetZero s.sr })
  pure r.value

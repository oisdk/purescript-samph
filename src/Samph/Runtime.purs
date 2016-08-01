module Samph.Runtime where

import Control.Monad.State
import Control.Monad.Error.Class
import Optic.Lens
import Optic.Getter
import Optic.Setter
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

type Lens' s a = forall f. (Functor f) => (a -> f a) -> s -> f s

ip :: Lens' SamphireState Lit
ip = lens (\s -> s.ip ) (\s x -> s { ip = x } )

sr :: Lens' SamphireState Lit
sr = lens (\s -> s.sr ) (\s x -> s { sr = x } )

sp :: Lens' SamphireState Lit
sp = lens (\s -> s.sp ) (\s x -> s { sp = x } )

ram :: Lens' SamphireState (Array Lit)
ram = lens (\s -> s.ram ) (\s x -> s { ram = x } )

io :: Lens' SamphireState (Array Lit)
io = lens (\s -> s.io ) (\s x -> s { io = x } )

i :: Lens' SamphireState Boolean
i = lens (\s -> getLit s.sr .&.  8 > 0) (set  8)

s :: Lens' SamphireState Boolean
s = lens (\s -> getLit s.sr .&. 16 > 0) (set 16)

o :: Lens' SamphireState Boolean
o = lens (\s -> getLit s.sr .&. 32 > 0) (set 32)

z :: Lens' SamphireState Boolean
z = lens (\s -> getLit s.sr .&. 64 > 0) (set 64)

set :: Int -> SamphireState -> Boolean -> SamphireState
set n s true  = s { sr = Lit (getLit s.sr .|. n) }
set n s false = s { sr = Lit (getLit s.sr .&. complement n) }

getLit :: Lit -> Int
getLit (Lit n) = n

use :: forall s m a. (MonadState s m) => Lens' s a -> m a
use l = gets (view l)

uses :: forall s m a b. (MonadState s m) => Lens' s a -> (a -> b) -> m b
uses l f = gets (f <<< view l)

data RuntimeError =
  OverflowedInstruction Lit |
  CorruptedCode String String |
  StackOverflow |
  StackUnderflow |
  IndexOutOfBounds Lit |
  InvalidPort Lit |
  Finished

type ErrorState a = forall m. (MonadState SamphireState m, MonadError RuntimeError m) => m a

popLit :: ErrorState Lit
popLit = do
  Lit i <- use ip
  r <- use ram
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

regLens :: Reg -> Lens' SamphireState Lit
regLens AL = lens (\s -> s.al ) (\s x -> s { al = x } )
regLens BL = lens (\s -> s.bl ) (\s x -> s { bl = x } )
regLens CL = lens (\s -> s.cl ) (\s x -> s { cl = x } )
regLens DL = lens (\s -> s.dl ) (\s x -> s { dl = x } )

arith :: forall m. (MonadState SamphireState m)
      => Reg
      -> (Int -> Int)
      -> m Unit
arith xreg op = do
  Lit xval <- use x
  let res = op xval
  modify (sr .~ Lit 0)
  when (res <   0) (modify (s .~ true))
  when (res > 255) (modify (o .~ true))
  let wrp = res .&. 255
  when (wrp ==  0) (modify (z .~ true))
  modify (x .~ Lit wrp)
  where x = regLens xreg

jmp :: forall m. (MonadState SamphireState m)
       => Lit -> m Unit
jmp i = modify (ip .~ i)

binArith :: forall m. (MonadState SamphireState m)
            => (Int -> Int -> Int)
            -> Reg
            -> Lens' SamphireState Lit
            -> m Unit
binArith op xreg ylens =
  arith xreg =<< (uses ylens (op <<< getLit))

push :: Lit -> ErrorState Unit
push x = do
  p <- use sp
  when (p == Lit 255) (throwError StackOverflow)
  setRam p x
  modify (sp %~ (_ - Lit 1))

pop :: ErrorState Lit
pop = do
  p <- use sp
  when (p == Lit 191) (throwError StackUnderflow)
  el <- getRam p
  modify (sp %~ (_ + Lit 1))
  pure el

getRam :: Lit -> ErrorState Lit
getRam (Lit i) = do
  el <- uses ram (_ !! i)
  case el of
    Nothing -> throwError (IndexOutOfBounds (Lit i))
    Just x -> pure x

setRam :: Lit -> Lit -> ErrorState Unit
setRam (Lit i) x = do
  arr <- uses ram (updateAt i x)
  case arr of
    Nothing -> throwError (IndexOutOfBounds (Lit i))
    Just a -> modify (ram .~ a)

getIO :: Lit -> ErrorState Lit
getIO (Lit i) = do
  el <- uses io (_ !! i)
  case el of
    Nothing -> throwError (InvalidPort (Lit i))
    Just x -> pure x

setIO :: Lit -> Lit -> ErrorState Unit
setIO (Lit i) x = do
  arr <- uses io (updateAt i x)
  case arr of
    Nothing -> throwError (InvalidPort (Lit i))
    Just a -> modify (io .~ a)

runInst :: MachineCode -> ErrorState Unit
runInst ins = case ins of
  A0 x y -> binArith (+) x (regLens y)
  A1 x y -> binArith (-) x (regLens y)
  A2 x y -> binArith (*) x (regLens y)
  A3 x y -> binArith (/) x (regLens y)
  A4 x -> arith x (_ + 1)
  A5 x -> arith x (_ - 1)
  A6 x y -> binArith mod x (regLens y)
  AA x y -> binArith (.&.) x (regLens y)
  AB x y -> binArith (.|.) x (regLens y)
  AC x y -> binArith (.^.) x (regLens y)
  AD x -> arith x complement
  A9 x -> arith x (\i -> shl i 1)
  B9 x -> arith x (\i -> shr i 1)
  C9 x -> arith x (\i -> shl i 1)
  D9 x -> arith x (\i -> shr i 1)
  B0 x (Lit y) -> arith x (_ + y)
  B1 x (Lit y) -> arith x (_ - y)
  B2 x (Lit y) -> arith x (_ * y)
  B3 x (Lit y) -> arith x (_ / y)
  B6 x (Lit y) -> arith x (\n -> mod n y)
  BA x (Lit y) -> arith x (_ .&. y)
  BB x (Lit y) -> arith x (_ .|. y)
  BC x (Lit y) -> arith x (_ .^. y)
  C0 x -> jmp x
  C1 x -> join $ when <$> use  z     <*> (pure<<<jmp) x
  C2 x -> join $ when <$> uses z not <*> (pure<<<jmp) x
  C3 x -> join $ when <$> use  s     <*> (pure<<<jmp) x
  C4 x -> join $ when <$> uses s not <*> (pure<<<jmp) x
  C5 x -> join $ when <$> use  o     <*> (pure<<<jmp) x
  C6 x -> join $ when <$> uses o not <*> (pure<<<jmp) x
  CA x -> do
    i <- use ip
    push i
    jmp x
  CB -> do
    i <- pop
    modify (ip .~ i)
  CD -> do
    i <- pop
    modify (ip .~ i)
  CC x -> do
    i <- use ip
    push i
    el <- getRam x
    push el
  E0 x -> do
    i <- use (regLens x)
    push i
  E1 x -> do
    i <- pop
    modify (regLens x .~ i)
  EA -> do
    i <- use sr
    push i
  EB -> do
    i <- pop
    modify (sr .~ i)
  F0 x -> do
    i <- getIO x
    modify (regLens AL .~ i)
  F1 x -> do
    i <- use (regLens AL)
    setIO x i
  FE -> pure unit
  O0 -> throwError Finished
  FF -> pure unit
  FC -> modify (i .~ true)
  FD -> modify (i .~ false)
  D0 x y -> modify (regLens x .~ y)
  D1 x (AddrLit y) -> do
    i <- getRam (Lit y)
    modify (regLens x .~ i)
  D2 (AddrLit x) y -> do
    i <- use (regLens y)
    setRam (Lit x) i
  D3 x (AddrReg y) -> do
    r <- use (regLens y)
    i <- getRam r
    modify (regLens x .~ i)
  D4 (AddrReg x) y -> do
    r <- use (regLens x)
    i <- use (regLens y)
    setRam r i
  DA x y -> do
    lhs <- use (regLens x)
    rhs <- use (regLens y)
    c (compare lhs rhs)
  DB x y -> do
    lhs <- use (regLens x)
    c (compare lhs y)
  DC x (AddrLit y) -> do
    lhs <- use (regLens x)
    rhs <- getRam (Lit y)
    c (compare lhs rhs)
  where
    c LT = modify (s .~ true)
    c EQ = modify (z .~ true)
    c GT = pure unit

initialState :: SamphireState
initialState =
  { ram: []
  , al: Lit 0
  , bl: Lit 0
  , cl: Lit 0
  , dl: Lit 0
  , ip: Lit 0
  , sr: Lit 0
  , sp: Lit 191
  , io: [] }

step :: ErrorState Unit
step = do
  i <- popIns
  runInst i

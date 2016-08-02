module Samph.Runtime where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.State (class MonadState, modify, gets)

import Data.Array (updateAt, (!!))
import Data.Int.Bits (shr, shl, complement, (.^.), (.|.), (.&.))
import Data.Maybe (Maybe(..))

import Optic.Getter (view)
import Optic.Lens (lens)
import Optic.Setter ((.~), (%~))

import Samph.Types (AddrLit(..), AddrReg(..), Lit(..), MachineCode(..), Reg(..))

-- | The state of a running Samphire program
type SamphireState =
  { ram :: Array Lit
  , al  :: Lit
  , bl  :: Lit
  , cl  :: Lit
  , dl  :: Lit
  , instructionPointer  :: Lit
  , stackPointer  :: Lit
  , statusRegister  :: Lit
  , io  :: Array Lit }

type Lens' s a = forall f. (Functor f) => (a -> f a) -> s -> f s

instructionPointer :: Lens' SamphireState Lit
instructionPointer = lens (\s -> s.instructionPointer ) (\s x -> s { instructionPointer = x } )

statusRegister :: Lens' SamphireState Lit
statusRegister = lens (\s -> s.statusRegister ) (\s x -> s { statusRegister = x } )

stackPointer :: Lens' SamphireState Lit
stackPointer = lens (\s -> s.stackPointer ) (\s x -> s { stackPointer = x } )

ram :: Lens' SamphireState (Array Lit)
ram = lens (\s -> s.ram ) (\s x -> s { ram = x } )

io :: Lens' SamphireState (Array Lit)
io = lens (\s -> s.io ) (\s x -> s { io = x } )

interrupt :: Lens' SamphireState Boolean
interrupt = lens (\s -> getLit s.statusRegister .&.  8 > 0) (set  8)

underflow :: Lens' SamphireState Boolean
underflow = lens (\s -> getLit s.statusRegister .&. 16 > 0) (set 16)

overflow :: Lens' SamphireState Boolean
overflow = lens (\s -> getLit s.statusRegister .&. 32 > 0) (set 32)

zeroFlag :: Lens' SamphireState Boolean
zeroFlag = lens (\s -> getLit s.statusRegister .&. 64 > 0) (set 64)

set :: Int -> SamphireState -> Boolean -> SamphireState
set n s true  = s { statusRegister = Lit (getLit s.statusRegister .|. n) }
set n s false = s { statusRegister = Lit (getLit s.statusRegister .&. complement n) }

getLit :: Lit -> Int
getLit (Lit n) = n

use :: forall s m a. (MonadState s m) => Lens' s a -> m a
use l = gets (view l)

uses :: forall s m a b. (MonadState s m) => Lens' s a -> (a -> b) -> m b
uses l f = gets (f <<< view l)

setState :: forall s m a. (MonadState s m) => Lens' s a -> a -> m Unit
setState l x = modify (l .~ x)

infixl 3 setState as .=

plusState :: forall s m a. (MonadState s m, Semiring a) => Lens' s a -> a -> m Unit
plusState l x = modify (l %~ flip add x)

infixl 3 plusState as +=

subState :: forall s m a. (MonadState s m, Ring a) => Lens' s a -> a -> m Unit
subState l x = modify (l %~ flip sub x)

infixl 3 subState as -=

data RuntimeError =
  CorruptedCode String String |
  StackOverflow |
  StackUnderflow |
  IndexOutOfBounds Lit |
  InvalidPort Lit |
  Finished

type ErrorState a = forall m. (MonadState SamphireState m, MonadError RuntimeError m) => m a

popLit :: ErrorState Lit
popLit = do
  i <- use instructionPointer
  x <- getRam i
  instructionPointer += Lit 1
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
regLens AL = lens (\st -> st.al ) (\st x -> st { al = x } )
regLens BL = lens (\st -> st.bl ) (\st x -> st { bl = x } )
regLens CL = lens (\st -> st.cl ) (\st x -> st { cl = x } )
regLens DL = lens (\st -> st.dl ) (\st x -> st { dl = x } )

arith :: forall m. (MonadState SamphireState m)
      => Reg
      -> (Int -> Int)
      -> m Unit
arith xreg op = do
  Lit xval <- use x
  let res = op xval
  statusRegister .= Lit 0
  when (res <   0) (underflow .= true)
  when (res > 255) (overflow  .= true)
  let wrp = res .&. 255
  when (wrp ==  0) (zeroFlag  .= true)
  x .= Lit wrp
  where x = regLens xreg

jmp :: forall m. (MonadState SamphireState m)
       => Lit -> m Unit
jmp i = instructionPointer .= i

binArith :: forall m. (MonadState SamphireState m)
            => (Int -> Int -> Int)
            -> Reg
            -> Lens' SamphireState Lit
            -> m Unit
binArith op xreg ylens =
  arith xreg =<< (uses ylens (op <<< getLit))

push :: Lit -> ErrorState Unit
push x = do
  p <- use stackPointer
  when (p == Lit 255) (throwError StackOverflow)
  setRam p x
  stackPointer -= Lit 1

pop :: ErrorState Lit
pop = do
  p <- use stackPointer
  when (p == Lit 191) (throwError StackUnderflow)
  el <- getRam p
  stackPointer += Lit 1
  pure el

getArr :: forall m. (MonadError RuntimeError m, MonadState SamphireState m)
          => (Lit -> RuntimeError)
          -> (SamphireState -> Array Lit)
          -> Lit -> m Lit
getArr e l (Lit i) = do
  arr <- gets l
  case arr !! i of
    Nothing -> throwError (e (Lit i))
    Just x -> pure x

setArr :: forall m. (MonadError RuntimeError m, MonadState SamphireState m)
          => (SamphireState -> Array Lit)
          -> (Array Lit -> SamphireState -> SamphireState)
          -> (Lit -> RuntimeError)
          -> Lit -> Lit -> m Unit
setArr g s e (Lit i) x = do
  arr <- gets (updateAt i x <<< g)
  case arr of
    Nothing -> throwError (e (Lit i))
    Just a -> modify (s a)

getRam :: forall m. (MonadError RuntimeError m, MonadState SamphireState m) => Lit -> m Lit
getRam = getArr IndexOutOfBounds (_.ram)

setRam :: forall m. (MonadError RuntimeError m, MonadState SamphireState m) => Lit -> Lit -> m Unit
setRam = setArr (_.ram) (\nr ss -> ss { ram = nr } ) IndexOutOfBounds

getIO ::  forall m. (MonadError RuntimeError m, MonadState SamphireState m) => Lit -> m Lit
getIO = getArr InvalidPort (_.io)

setIO :: forall m. (MonadError RuntimeError m, MonadState SamphireState m) => Lit -> Lit -> m Unit
setIO = setArr (_.io) (\nr ss -> ss { io = nr } ) InvalidPort

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
  C1 x -> join $ when <$> use  zeroFlag      <*> (pure<<<jmp) x
  C2 x -> join $ when <$> uses zeroFlag  not <*> (pure<<<jmp) x
  C3 x -> join $ when <$> use  underflow     <*> (pure<<<jmp) x
  C4 x -> join $ when <$> uses underflow not <*> (pure<<<jmp) x
  C5 x -> join $ when <$> use  overflow      <*> (pure<<<jmp) x
  C6 x -> join $ when <$> uses overflow  not <*> (pure<<<jmp) x
  CA x -> do
    i <- use instructionPointer
    push i
    jmp x
  CB -> do
    i <- pop
    instructionPointer .= i
  CD -> do
    i <- pop
    instructionPointer .= i
  CC x -> do
    i <- use instructionPointer
    push i
    el <- getRam x
    push el
  E0 x -> do
    i <- use (regLens x)
    push i
  E1 x -> do
    i <- pop
    regLens x .= i
  EA -> do
    i <- use statusRegister
    push i
  EB -> do
    i <- pop
    statusRegister .= i
  F0 x -> do
    i <- getIO x
    regLens AL .= i
  F1 x -> do
    i <- use (regLens AL)
    setIO x i
  FE -> pure unit
  O0 -> throwError Finished
  FF -> pure unit
  FC -> interrupt .= true
  FD -> interrupt .= false
  D0 x y -> regLens x .= y
  D1 x (AddrLit y) -> do
    i <- getRam (Lit y)
    regLens x .= i
  D2 (AddrLit x) y -> do
    i <- use (regLens y)
    setRam (Lit x) i
  D3 x (AddrReg y) -> do
    r <- use (regLens y)
    i <- getRam r
    regLens x .= i
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
    c LT = underflow .= true
    c EQ = zeroFlag  .= true
    c GT = pure unit

initialState :: SamphireState
initialState =
  { ram: []
  , al: Lit 0
  , bl: Lit 0
  , cl: Lit 0
  , dl: Lit 0
  , instructionPointer: Lit 0
  , statusRegister: Lit 0
  , stackPointer: Lit 191
  , io: [] }

step :: ErrorState Unit
step = do
  i <- popIns
  runInst i

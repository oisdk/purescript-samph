module Samph.Types where

import Prelude
import Data.Int (toStringAs, hexadecimal)
import Data.Int.Bits ((.&.))

data Reg = AL | BL | CL | DL

instance showReg :: Show Reg where
  show AL = "AL"
  show BL = "BL"
  show CL = "CL"
  show DL = "DL"

instance eqReg :: Eq Reg where
  eq AL AL = true
  eq BL BL = true
  eq CL CL = true
  eq DL DL = true
  eq _ _ = false

newtype Lit = Lit Int

instance showLit :: Show Lit where
  show (Lit n) = toStringAs hexadecimal n

instance eqLit :: Eq Lit where
  eq (Lit n) (Lit m) = eq n m

instance boundedLit :: Bounded Lit where
  top = Lit 255
  bottom = Lit 0

instance ordLit :: Ord Lit where
  compare (Lit n) (Lit m) = compare n m

instance semiRingLit :: Semiring Lit where
  add (Lit n) (Lit m) = Lit (add n m .&. 255)
  zero = Lit 0
  mul (Lit n) (Lit m) = Lit (mul n m .&. 255)
  one = Lit 1

instance ringLit :: Ring Lit where
  sub (Lit n) (Lit m) = Lit (sub n m .&. 255)

instance commutativeRingLit :: CommutativeRing Lit

instance euclidianRingLit :: EuclideanRing Lit where
  degree (Lit n) = degree n
  div (Lit n) (Lit m) = Lit (div n m .&. 255)
  mod (Lit n) (Lit m) = Lit (mod n m .&. 255)

newtype AddrLit = AddrLit Int

instance showAddrLit :: Show AddrLit where
  show (AddrLit n) = "[" <> toStringAs hexadecimal n <>"]"

instance eqAddrLit :: Eq AddrLit where
  eq (AddrLit x) (AddrLit y) = eq x y

newtype AddrReg = AddrReg Reg

instance showAddrReg :: Show AddrReg where
  show (AddrReg n) = "[" <> show n <> "]"

instance eqAddrReg :: Eq AddrReg where
  eq (AddrReg x) (AddrReg y) = eq x y

data MachineCode =
  A0 Reg Reg     | -- Add
  A1 Reg Reg     | -- Sub
  A2 Reg Reg     | -- Mul
  A3 Reg Reg     | -- Div
  A4 Reg         | -- Inc
  A5 Reg         | -- Dec
  A6 Reg Reg     | -- Mod
  AA Reg Reg     | -- And
  AB Reg Reg     | -- Or
  AC Reg Reg     | -- Xor
  AD Reg         | -- Not
  A9 Reg         | -- Rol
  B9 Reg         | -- Ror
  C9 Reg         | -- Shl
  D9 Reg         | -- Shr
  B0 Reg Lit     | -- Add
  B1 Reg Lit     | -- Sub
  B2 Reg Lit     | -- Mul
  B3 Reg Lit     | -- Div
  B6 Reg Lit     | -- Mod
  BA Reg Lit     | -- And
  BB Reg Lit     | -- Or
  BC Reg Lit     | -- Xor
  C0 Lit         | -- Jmp
  C1 Lit         | -- Jz
  C2 Lit         | -- Jnz
  C3 Lit         | -- Js
  C4 Lit         | -- Jns
  C5 Lit         | -- Jo
  C6 Lit         | -- Jno
  CA Lit         | -- Call
  CB             | -- Ret
  CC Lit         | -- Int
  CD             | -- Iret
  E0 Reg         | -- Push
  E1 Reg         | -- Pop
  EA             | -- Pushf
  EB             | -- Popf
  F0 Lit         | -- In
  F1 Lit         | -- Out
  FE             | -- Clo
  O0             | -- Halt
  FF             | -- Nop
  FC             | -- Sti
  FD             | -- Cli
  D0 Reg Lit     | -- Mov
  D1 Reg AddrLit | -- Mov
  D2 AddrLit Reg | -- Mov
  D3 Reg AddrReg | -- Mov
  D4 AddrReg Reg | -- Mov
  DA Reg Reg     | -- Cmp
  DB Reg Lit     | -- Cmp
  DC Reg AddrLit   -- Cmp

data Arg =
  RegA Reg | LitA Lit | AddrLitA AddrLit | AddrRegA AddrReg | Empty

instance eqArg :: Eq Arg where
  eq (RegA x) (RegA y) = eq x y
  eq (LitA x) (LitA y) = eq x y
  eq (AddrLitA x) (AddrLitA y) = eq x y
  eq (AddrRegA x) (AddrRegA y) = eq x y
  eq Empty Empty = true
  eq _ _ = false

data Compact = Compact Int Arg Arg

instance showCompact :: Show Compact where
  show (Compact ins x y) = toStringAs hexadecimal ins <> show' x y where
    show' Empty _ = ""
    show' z Empty = " " <> show1 z
    show' w z = " " <> show1 w <> " " <> show1 z
    show1 (RegA r) = show r
    show1 (LitA n) = show n
    show1 (AddrLitA a) = show a
    show1 (AddrRegA a) = show a
    show1 Empty = ""

instance eqCompact :: Eq Compact where
  eq (Compact n w x) (Compact m y z) = eq n m && eq w y && eq x z

compact :: MachineCode -> Compact
compact (A0 x y) = Compact 0xA0 (RegA x) (RegA y)
compact (A1 x y) = Compact 0xA1 (RegA x) (RegA y)
compact (A2 x y) = Compact 0xA2 (RegA x) (RegA y)
compact (A3 x y) = Compact 0xA3 (RegA x) (RegA y)
compact (A4 x)   = Compact 0xA4 (RegA x)  Empty
compact (A5 x)   = Compact 0xA5 (RegA x)  Empty
compact (A6 x y) = Compact 0xA6 (RegA x) (RegA y)
compact (AA x y) = Compact 0xAA (RegA x) (RegA y)
compact (AB x y) = Compact 0xAB (RegA x) (RegA y)
compact (AC x y) = Compact 0xAC (RegA x) (RegA y)
compact (AD x)   = Compact 0xAD (RegA x)  Empty
compact (A9 x)   = Compact 0xA9 (RegA x)  Empty
compact (B9 x)   = Compact 0xB9 (RegA x)  Empty
compact (C9 x)   = Compact 0xC9 (RegA x)  Empty
compact (D9 x)   = Compact 0xD9 (RegA x)  Empty
compact (B0 x y) = Compact 0xB0 (RegA x) (LitA y)
compact (B1 x y) = Compact 0xB1 (RegA x) (LitA y)
compact (B2 x y) = Compact 0xB2 (RegA x) (LitA y)
compact (B3 x y) = Compact 0xB3 (RegA x) (LitA y)
compact (B6 x y) = Compact 0xB6 (RegA x) (LitA y)
compact (BA x y) = Compact 0xBA (RegA x) (LitA y)
compact (BB x y) = Compact 0xBB (RegA x) (LitA y)
compact (BC x y) = Compact 0xBC (RegA x) (LitA y)
compact (C0 x)   = Compact 0xC0 (LitA x)  Empty
compact (C1 x)   = Compact 0xC1 (LitA x)  Empty
compact (C2 x)   = Compact 0xC2 (LitA x)  Empty
compact (C3 x)   = Compact 0xC3 (LitA x)  Empty
compact (C4 x)   = Compact 0xC4 (LitA x)  Empty
compact (C5 x)   = Compact 0xC5 (LitA x)  Empty
compact (C6 x)   = Compact 0xC6 (LitA x)  Empty
compact (CA x)   = Compact 0xCA (LitA x)  Empty
compact  CB      = Compact 0xCB  Empty    Empty
compact (CC x)   = Compact 0xCC (LitA x)  Empty
compact  CD      = Compact 0xCD  Empty    Empty
compact (E0 x)   = Compact 0xE0 (RegA x)  Empty
compact (E1 x)   = Compact 0xE1 (RegA x)  Empty
compact  EA      = Compact 0xEA  Empty    Empty
compact  EB      = Compact 0xEB  Empty    Empty
compact (F0 x)   = Compact 0xF0 (LitA x)  Empty
compact (F1 x)   = Compact 0xF1 (LitA x)  Empty
compact  FE      = Compact 0xFE  Empty    Empty
compact  O0      = Compact 0x00  Empty    Empty
compact  FF      = Compact 0xFF  Empty    Empty
compact  FC      = Compact 0xFC  Empty    Empty
compact  FD      = Compact 0xFD  Empty    Empty
compact (D0 x y) = Compact 0xD0 (RegA x)     (LitA y)
compact (D1 x y) = Compact 0xD1 (RegA x)     (AddrLitA y)
compact (D2 x y) = Compact 0xD2 (AddrLitA x) (RegA y)
compact (D3 x y) = Compact 0xD3 (RegA x)     (AddrRegA y)
compact (D4 x y) = Compact 0xD4 (AddrRegA x) (RegA y)
compact (DA x y) = Compact 0xDA (RegA x)     (RegA y)
compact (DB x y) = Compact 0xDB (RegA x)     (LitA y)
compact (DC x y) = Compact 0xDC (RegA x)     (AddrLitA y)

instance showMach :: Show MachineCode where
  show = show <<< compact

instance eqMach :: Eq MachineCode where
  eq x y = eq (compact x) (compact y)

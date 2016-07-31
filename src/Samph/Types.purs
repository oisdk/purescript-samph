module Samph.Types where

import Prelude

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
  show (Lit n) = show n

instance eqLit :: Eq Lit where
  eq (Lit n) (Lit m) = eq n m

newtype AddrLit = AddrLit Int

instance showAddrLit :: Show AddrLit where
  show (AddrLit n) = show n

instance eqAddrLit :: Eq AddrLit where
  eq (AddrLit x) (AddrLit y) = eq x y

newtype AddrReg = AddrReg Reg

instance showAddrReg :: Show AddrReg where
  show (AddrReg n) = show n

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

instance showMach :: Show MachineCode where
  show (A0 x y) = "A0 " <> show x <> " " <> show y
  show (A1 x y) = "A1 " <> show x <> " " <> show y
  show (A2 x y) = "A2 " <> show x <> " " <> show y
  show (A3 x y) = "A3 " <> show x <> " " <> show y
  show (A4 x)   = "A4 " <> show x
  show (A5 x)   = "A5 " <> show x
  show (A6 x y) = "A6 " <> show x <> " " <> show y
  show (AA x y) = "AA " <> show x <> " " <> show y
  show (AB x y) = "AB " <> show x <> " " <> show y
  show (AC x y) = "AC " <> show x <> " " <> show y
  show (AD x)   = "AD " <> show x
  show (A9 x)   = "A9 " <> show x
  show (B9 x)   = "B9 " <> show x
  show (C9 x)   = "C9 " <> show x
  show (D9 x)   = "D9 " <> show x
  show (B0 x y) = "B0 " <> show x <> " " <> show y
  show (B1 x y) = "B1 " <> show x <> " " <> show y
  show (B2 x y) = "B2 " <> show x <> " " <> show y
  show (B3 x y) = "B3 " <> show x <> " " <> show y
  show (B6 x y) = "B6 " <> show x <> " " <> show y
  show (BA x y) = "BA " <> show x <> " " <> show y
  show (BB x y) = "BB " <> show x <> " " <> show y
  show (BC x y) = "BC " <> show x <> " " <> show y
  show (C0 x)   = "C0 " <> show x
  show (C1 x)   = "C1 " <> show x
  show (C2 x)   = "C2 " <> show x
  show (C3 x)   = "C3 " <> show x
  show (C4 x)   = "C4 " <> show x
  show (C5 x)   = "C5 " <> show x
  show (C6 x)   = "C6 " <> show x
  show (CA x)   = "CA " <> show x
  show  CB      = "CB"
  show (CC x)   = "CC " <> show x
  show  CD      = "CD"
  show (E0 x)   = "E0 " <> show x
  show (E1 x)   = "E1 " <> show x
  show  EA      = "EA"
  show  EB      = "EB"
  show (F0 x)   = "F0 " <> show x
  show (F1 x)   = "F1 " <> show x
  show  FE      = "FE"
  show  O0      = "O0"
  show  FF      = "FF"
  show  FC      = "FC"
  show  FD      = "FD"
  show (D0 x y) = "D0 " <> show x <> " " <> show y
  show (D1 x y) = "D1 " <> show x <> " " <> show y
  show (D2 x y) = "D2 " <> show x <> " " <> show y
  show (D3 x y) = "D3 " <> show x <> " " <> show y
  show (D4 x y) = "D4 " <> show x <> " " <> show y
  show (DA x y) = "DA " <> show x <> " " <> show y
  show (DB x y) = "DB " <> show x <> " " <> show y
  show (DC x y) = "DC " <> show x <> " " <> show y

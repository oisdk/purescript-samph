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

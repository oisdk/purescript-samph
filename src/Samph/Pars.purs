module Samph.Pars
  ( unBase
  , firstPass
  , program
  , instruction
  , labelDecl
  , addrReg
  , addrLit
  , reg
  , hex
  ) where

import Prelude

import Samph.Types (AddrLit(..), AddrReg(..), Lit(..), MachineCode(..), Reg(..))

import Control.Alternative ((<|>))
import Control.Apply (lift2)

import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Reader (class MonadReader, runReader, ask)
import Control.Monad.ST (ST, STRef, newSTRef, modifySTRef, runST, readSTRef)
import Control.Monad.Trans (lift)

import Data.Traversable (class Foldable, foldl)
import Data.Array (catMaybes, many, init, some)

import Data.Char.Unicode (digitToInt)

import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)

import Data.StrMap (StrMap, lookup, freezeST)
import Data.StrMap.ST (STStrMap, new, poke, peek)

import Text.Parsing.Parser (ParserT(..), fail, unParserT)
import Text.Parsing.Parser.Combinators (lookAhead, choice, skipMany, try, notFollowedBy, (<?>))
import Text.Parsing.Parser.String (anyChar)
import Text.Parsing.Parser.Token (GenTokenParser, GenLanguageDef(..), makeTokenParser, letter, alphaNum)

-- | Converts a list of integers into the number they represent in a
-- | given base
-- | ```purescript
-- | unBase 10 [1,2,3] == 123
-- | unBase 2  [1,0,1] == 5
-- | unBase 16 [3,15,12] == 1020
-- | ```
unBase :: forall f. Foldable f => Int -> f Int -> Int
unBase b = foldl f 0 where f n e = n * b + e

parseMaybe :: forall a b s m. Monad m
              => (a -> Maybe b)
              -> String
              -> ParserT s m a
              -> ParserT s m b
parseMaybe f e p =
  maybe (fail ("Expected " <> e)) pure <<< f =<< p

samph :: forall m. Monad m => GenLanguageDef String m
samph = LanguageDef
  { commentStart   : ""
  , commentEnd     : ""
  , commentLine    : ";"
  , nestedComments : true
  , identStart     : letter
  , identLetter    : alphaNum
  , opStart        : letter
  , opLetter       : letter
  , reservedNames  : names
  , reservedOpNames: names
  , caseSensitive  : false }

names :: Array String
names = binops <> unops <> nops <> regs

regs :: Array String
regs = [ "AL", "BL", "CL", "DL" ]

binops :: Array String
binops =
  [ "ADD", "SUB", "MUL", "DIV", "MOD"
  , "AND", "OR", "XOR", "CMP", "MOV"]

unops :: Array String
unops =
  [ "INC", "DEC", "NOT", "ROL", "ROR"
  , "SHL", "SHR", "JMP", "JZ", "JNZ"
  , "JS", "JNS", "JO", "JNO", "CALL"
  , "INT", "PUSH", "POP", "IN", "OUT"
  , "ORG", "DB"]

nops :: Array String
nops =
  [ "RET", "IRET", "PUSHF", "POPF"
  , "CLO", "HALT", "NOP", "STI", "CLI"
  , "END" ]

type StringParser = forall m. Monad m => ParserT String m String

tokenParser :: forall m. Monad m => GenTokenParser String m
tokenParser = makeTokenParser samph

lex :: forall m a. Monad m => ParserT String m a -> ParserT String m a
lex = tokenParser.lexeme
brackets :: forall m a. Monad m => ParserT String m a -> ParserT String m a
brackets = tokenParser.brackets

reserved :: forall m. Monad m => String -> ParserT String m Unit
reserved = tokenParser.reserved

ident :: StringParser
ident = tokenParser.identifier
colon :: StringParser
colon = tokenParser.colon
comma :: StringParser
comma = tokenParser.comma

wSpace :: forall m. Monad m => ParserT String m Unit
wSpace = tokenParser.whiteSpace

-- | Parses a hex number
hex :: forall m. Monad m => ParserT String m Int
hex = lex (map (unBase 16) (some (try hexDigs)) <?> "a hex number") where
  hexDigs = parseMaybe digitToInt "a hex digit" anyChar

reg :: forall m. Monad m => ParserT String m Reg
reg = choice
  [ reserved "AL" $> AL
  , reserved "BL" $> BL
  , reserved "CL" $> CL
  , reserved "DL" $> DL
  ] <?> "a register"

word8 :: forall m. Monad m => ParserT String m Int
word8 = (do
  n <- hex
  when (n > 255) (fail "Only numbers up to 255 supported")
  pure n) <?> "a hex literal"

lit :: forall m. Monad m => ParserT String m Lit
lit = map Lit word8

addrLit :: forall m. Monad m => ParserT String m AddrLit
addrLit = brackets (map AddrLit word8) <?> "An address literal"

addrReg :: forall m. Monad m => ParserT String m AddrReg
addrReg = brackets (map AddrReg reg) <?> "An address in a register"

labelDecl :: forall m. Monad m => ParserT String m String
labelDecl = (ident <?> "label") <* colon

-- First-pass parsers

logLabel :: forall h.
            STRef h Int
         -> STStrMap h Int
         -> ParserT String (Eff (st :: ST h)) Unit
logLabel i m = do
  name <- labelDecl
  prev <- lift (peek m name)
  when (isJust prev) (fail ("Repeated label: " <> name))
  lift (do
    i <- readSTRef i
    void (poke m name i))

instruction :: forall m. Monad m
            => ParserT String m String
instruction = binop <|> unop <|> nop

binop :: forall m. Monad m => ParserT String m String
binop = choice (map reserved binops) *> (sepSpace <$> (arg' <* comma) <*> arg')
  where
    sepSpace x y = x <> " " <> y
    arg' = ident' <|> arg

unop :: forall m. Monad m => ParserT String m String
unop = choice (map reserved unops) *> (ident' <|> arg)

nop :: forall m. Monad m => ParserT String m String
nop = choice (map reserved (fromMaybe [] (init nops))) $> ""

ident' :: forall m. Monad m => ParserT String m String
ident' = try (ident <* notFollowedBy colon)

arg :: forall m. Monad m => ParserT String m String
arg = choice [ map show (try addrReg)
             , map show (try addrLit)
             , map show reg
             , map show lit ]

firstPass :: forall m. Monad m => ParserT String m (StrMap Int)
firstPass = wSpace *> ParserT p where
  p s = pure (runPure (runST (unParserT firstPass' s)))
  instr :: forall h. STRef h Int -> ParserT String (Eff (st :: ST h)) Unit
  instr i = (binop *> globInc 3) <|> (unop *> globInc 2) <|> (nop *> globInc 1)
    where globInc n= lift (void (modifySTRef i (_ + n)))
  firstPass' :: forall h. ParserT String (Eff (st :: ST h)) (StrMap Int)
  firstPass' = do
    m <- lift new
    i <- lift (newSTRef 0)
    skipMany (instr i <|> logLabel i m)
    reserved "END"
    lift (freezeST m)

-- Second-pass parsers

unMon :: forall n m a. Monad m => (n ~> m) -> ParserT String n a -> ParserT String m a
unMon f (ParserT p) = ParserT $ \s -> f (p s)

revAp :: forall f a b. Apply f => f a -> f (a -> b) -> f b
revAp = lift2 (flip id)

infixl 4 revAp as <**>

revMap :: forall f a b c. (Functor f) => (a -> b -> c) -> f b -> f (a -> c)
revMap f = map (flip f)

infixl 4 revMap as <$$>

binArith :: forall m. Monad m => ParserT String m MachineCode
binArith = choice
  [ arith' "ADD" A0 B0
  , arith' "SUB" A1 B1
  , arith' "MUL" A2 B2
  , arith' "DIV" A3 B3
  , arith' "MOD" A6 B6
  , arith' "AND" AA BA
  , arith' "OR"  AB BB
  , arith' "XOR" AC BC
  ] where
    arith' n a b =
      reserved n *> ((reg <* comma) <**> (a <$$> reg <|> b <$$> lit))

unArith :: forall m. Monad m => ParserT String m MachineCode
unArith = choice
  [ un' "INC"  A4
  , un' "DEC"  A5
  , un' "NOT"  AD
  , un' "ROL"  A9
  , un' "ROR"  B9
  , un' "SHL"  C9
  , un' "SHR"  D9
  , un' "PUSH" E0
  , un' "POP"  E1
  ] where
    un' n c = reserved n *> map c reg

mov ::  forall m. Monad m => ParserT String m MachineCode
mov = reserved "MOV" *> (regOpts <|> othOpts) where
  regOpts = (reg <* comma) <**> (D0 <$$> lit <|> D3 <$$> addrReg <|> D1 <$$> addrLit)
  othOpts = (d4 <|> d2) <* comma <*> reg
  d4 = map D4 addrReg
  d2 = map D2 addrLit

cmp :: forall m. Monad m => ParserT String m MachineCode
cmp =
  reserved "CMP" *>
  (reg <* comma) <**>
  (DA <$$> reg <|> DB <$$> lit <|> DC <$$> addrLit)

jmp :: forall m. (Monad m, MonadReader (StrMap Int) m)
    => ParserT String m MachineCode
jmp = do
  c <- choice [ reserved "JMP" $> C0
              , reserved "JZ"  $> C1
              , reserved "JNZ" $> C2
              , reserved "JS"  $> C3
              , reserved "JNS" $> C4
              , reserved "JO"  $> C5
              , reserved "JNO" $> C6 ]
  name <- ident
  labels <- lift ask
  case lookup name labels of
    Nothing -> fail ("Unrecognised label: " <> name)
    Just n -> pure (c (Lit n))

misc :: forall m. Monad m => ParserT String m MachineCode
misc = choice
  [ reserved "HALT"  $> O0
  , reserved "CLO"   $> FE
  , reserved "NOP"   $> FF
  , reserved "STI"   $> FC
  , reserved "CLI"   $> FD
  , reserved "RET"   $> CB
  , reserved "IRET"  $> CD
  , reserved "PUSHF" $> EA
  , reserved "POPF"  $> EB ]

instructions' :: forall m. (Monad m, MonadReader (StrMap Int) m)
             => ParserT String m (Maybe MachineCode)
instructions' = map Just (choice [misc, mov, cmp, jmp, unArith, binArith]) <|> labelDecl $> Nothing

instructions :: forall m. Monad m
             => StrMap Int
             -> ParserT String m (Array MachineCode)
instructions m = do
  ins <- unMon (\p -> pure (runReader p m)) (many instructions')
  reserved "END"
  pure (catMaybes ins)

program :: forall m. Monad m => ParserT String m (Array MachineCode)
program = wSpace *> (lookAhead firstPass >>= instructions)


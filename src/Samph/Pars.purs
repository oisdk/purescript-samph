module Samph.Pars where

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String
import Data.Array
import Data.Traversable
import Prelude
import Data.Maybe
import Samph.Types
import Data.StrMap.ST
import Control.Monad.Eff
import Control.Monad.Trans
import Control.Monad.Eff.Console
import Control.Alternative
import Control.Monad.ST
import Data.Either
import Text.Parsing.Parser.Pos
import Data.Char.Unicode (digitToInt)
import Text.Parsing.Parser.Language (emptyDef)
import Data.StrMap hiding (pureST,runST)
import Text.Parsing.Parser.Token hiding (when)
import Data.Foldable hiding (oneOf)

unBase :: forall f. Foldable f => Int -> f Int -> Int
unBase b = foldl f 0 where f n e = n * b + e

hex :: forall m. Monad m => ParserT String m Int
hex = lex (map (unBase 16) (some (try hexDigs)) <?> "a hex number") where
  hexDigs = do
    c <- anyChar
    maybe (fail "Expected a hex digit") pure (digitToInt c)

reg :: forall m. Monad m => ParserT String m Reg
reg = choice
  [ reserved "AL" $> AL
  , reserved "BL" $> BL
  , reserved "CL" $> CL
  , reserved "DL" $> DL
  ] <?> "a register"

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

tokenParser = makeTokenParser samph
lex         = tokenParser.lexeme
reserved    = tokenParser.reserved
brackets    = tokenParser.brackets
ident       = tokenParser.identifier
colon       = tokenParser.colon
comma       = tokenParser.comma
op          = tokenParser.reservedOp
wSpace      = tokenParser.whiteSpace

lit :: forall m. Monad m => ParserT String m Lit
lit = (do
  n <- hex
  when (n > 255) (fail "Only numbers up to 255 supported")
  pure (Lit n)) <?> "a hex literal"

addrLit :: forall m. Monad m => ParserT String m AddrLit
addrLit = brackets (do
  Lit n <- lit
  pure (AddrLit n)) <?> "An address literal"

addrReg :: forall m. Monad m => ParserT String m AddrReg
addrReg = brackets (map AddrReg reg) <?> "An address in a register"

labelDecl :: forall m. Monad m => ParserT String m String
labelDecl = do
  named <- ident <?> "label"
  colon
  pure named

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
instruction = binop <|> unop <|> nop where
  binop = do
    _ <- choice (map op binops)
    x <- ident' <|> arg
    comma
    y <- ident' <|> arg
    pure (x <> " " <> y)
  unop = do
    _ <- choice (map op unops)
    ident' <|> arg
  nop = choice (map op (fromMaybe [] (init nops))) $> ""
  ident' = try (ident <* notFollowedBy colon)
  arg = choice [ map show (try addrReg)
               , map show (try addrLit)
               , map show reg
               , map show lit ]

firstPass :: forall m. Monad m => ParserT String m (StrMap Int)
firstPass = wSpace *> ParserT p where
  p s = pure (runPure (runST (unParserT firstPass' s)))
  instr :: forall h. STRef h Int -> ParserT String (Eff (st :: ST h)) Unit
  instr i = do
    instruction
    lift (void (modifySTRef i (_ + 1)))
  firstPass' :: forall h. ParserT String (Eff (st :: ST h)) (StrMap Int)
  firstPass' = do
    m <- lift new
    i <- lift (newSTRef 0)
    skipMany (instr i <|> logLabel i m)
    reserved "END"
    lift (freezeST m)

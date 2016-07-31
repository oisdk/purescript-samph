-- | Primitive parsers for strings.

module Text.Parsing.StringParser.String
  ( eof
  , anyChar
  , anyDigit
  , string
  , satisfy
  , char
  , whiteSpace
  , skipSpaces
  , oneOf
  , noneOf
  , lowerCaseChar
  , upperCaseChar
  , anyLetter
  , alphaNum
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((..))
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, elem, notElem)
import Data.Maybe (Maybe(..))
import Data.String (charAt, length, indexOf', singleton)
import Text.Parsing.StringParser (Parser(..), ParseError(..), try, fail)
import Text.Parsing.StringParser.Combinators (many, (<?>))

-- | Match the end of the file.
eof :: Parser Unit
eof = Parser \s ->
  case s of
    { str, pos } | pos < length str -> Left { pos, error: ParseError "Expected EOF" }
    _ -> Right { result: unit, suffix: s }

-- | Match any character.
anyChar :: Parser Char
anyChar = Parser \{ str, pos } ->
  case charAt pos str of
    Just chr -> Right { result: chr, suffix: { str, pos: pos + 1 } }
    Nothing -> Left { pos, error: ParseError "Unexpected EOF" }

-- | Match any digit.
anyDigit :: Parser Char
anyDigit = try do
  c <- anyChar
  if c >= '0' && c <= '9'
     then pure c
     else fail $ "Character " <> show c <> " is not a digit"

-- | Match the specified string.
string :: String -> Parser String
string nt = Parser \s ->
  case s of
    { str, pos } | indexOf' nt pos str == Just pos -> Right { result: nt, suffix: { str, pos: pos + length nt } }
    { pos } -> Left { pos, error: ParseError ("Expected '" <> nt <> "'.") }

-- | Match a character satisfying the given predicate.
satisfy :: (Char -> Boolean) -> Parser Char
satisfy f = try do
  c <- anyChar
  if f c
     then pure c
     else fail $ "Character " <> show c <> " did not satisfy predicate"

-- | Match the specified character.
char :: Char -> Parser Char
char c = satisfy (_ == c) <?> "Could not match character " <> show c

-- | Match many whitespace characters.
whiteSpace :: Parser String
whiteSpace = do
  cs <- many (satisfy \ c -> c == '\n' || c == '\r' || c == ' ' || c == '\t')
  pure (foldMap singleton cs)

-- | Skip many whitespace characters.
skipSpaces :: Parser Unit
skipSpaces = void whiteSpace

-- | Match one of the characters in the foldable structure.
oneOf :: forall f. (Foldable f) => f Char -> Parser Char
oneOf = satisfy <<< flip elem

-- | Match any character not in the foldable structure.
noneOf :: forall f. (Foldable f) => f Char -> Parser Char
noneOf = satisfy <<< flip notElem

-- | Match any lower case character.
lowerCaseChar :: Parser Char
lowerCaseChar = try do
  c <- anyChar
  if toCharCode c `elem` (97 .. 122)
     then pure c
     else fail $ "Expected a lower case character but found " <> show c

-- | Match any upper case character.
upperCaseChar :: Parser Char
upperCaseChar = try do
  c <- anyChar
  if toCharCode c `elem` (65 .. 90)
     then pure c
     else fail $ "Expected an upper case character but found " <> show c

-- | Match any letter.
anyLetter :: Parser Char
anyLetter = lowerCaseChar <|> upperCaseChar <?> "Expected a letter"

-- | Match a letter or a number.
alphaNum :: Parser Char
alphaNum = anyLetter <|> anyDigit <?> "Expected a letter or a number"

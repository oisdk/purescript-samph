module Test.Main where

import Prelude (class Show, class Eq, class Ord, Unit, bind, negate, show, map, compare, eq, (<<<), (*), (<), (<$>), (+))
import Test.QuickCheck (class Testable, Result, quickCheck, quickCheck', (===))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Samph.Pars (program, firstPass, instruction, labelDecl, addrReg, addrLit, reg, unBase, hex)
import Samph.Types (AddrLit(..), AddrReg(..), Lit(..), MachineCode(..), Reg(..))
import Data.String (toCharArray)
import Data.Tuple (Tuple(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Int (toStringAs, hexadecimal)
import Data.StrMap (fromFoldable)
import Data.Either (Either(..))
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Text.Parsing.Parser (runParser, Parser)

newtype Positive = Positive Int

instance showPos :: Show Positive where
  show (Positive n) = show n

instance eqPos :: Eq Positive where
  eq (Positive n) (Positive m) = eq n m

instance ordPos :: Ord Positive where
  compare (Positive n) (Positive m) = compare n m

instance arbPos :: Arbitrary Positive where
  arbitrary = (Positive <<< abs) <$> arbitrary

slowToBase :: Int -> Array Int
slowToBase = map toInt <<< toCharArray <<< show where
  toInt '0' = 0
  toInt '1' = 1
  toInt '2' = 2
  toInt '3' = 3
  toInt '4' = 4
  toInt '5' = 5
  toInt '6' = 6
  toInt '7' = 7
  toInt '8' = 8
  toInt '9' = 9
  toInt _ = 0

abs :: Int -> Int
abs n | n < 0 = n * (-1)
abs n = n

checkHexParse :: Positive -> Result
checkHexParse (Positive n) =
  Right n === runParser (toStringAs hexadecimal n) hex

checkUnBase :: Positive -> Result
checkUnBase (Positive n) = n === (unBase 10 <<< slowToBase) n

once :: forall a. Testable a => a -> Quick Unit
once = quickCheck' 1

type Quick a = forall e.
  Eff ( err :: EXCEPTION
      , console :: CONSOLE
      , random :: RANDOM
      | e ) a

type QuickFS a = forall e.
  Eff ( err :: EXCEPTION
      , console :: CONSOLE
      , random :: RANDOM
      , fs :: FS
      | e ) a

parseExample :: forall a. (Eq a, Show a)
             => Parser String a -> a -> String -> Quick Unit
parseExample prs res str =
  once (Right res === runParser str prs)

fromFile :: forall a. (Eq a, Show a)
         => String -> Parser String a -> a
         -> QuickFS Unit
fromFile loc prs res = do
  file <- readTextFile UTF8 loc
  parseExample prs res file

main :: QuickFS Unit
main = do
  quickCheck checkUnBase
  quickCheck checkHexParse
  once (Lit 255 + Lit 1 === Lit 0)
  parseExample reg AL "AL"
  parseExample reg BL "BL"
  parseExample reg CL "CL"
  parseExample reg DL "DL"
  parseExample reg DL "dL"
  parseExample addrLit (AddrLit 0) "[0]"
  parseExample addrLit (AddrLit 0) "[0 ]"
  parseExample addrLit (AddrLit 0) "[ 0]"
  parseExample addrLit (AddrLit 10) "[A]"
  parseExample addrReg (AddrReg AL) "[AL ]"
  parseExample instruction "AL 4" "ADD AL, 4"
  parseExample instruction "[AL] 4" "MUL [AL],4"
  parseExample instruction "[BL] AL" "SUB [BL],AL"
  parseExample labelDecl "jump" "jump :  "
  parseExample instruction "" "CLO"
  parseExample instruction "Clear" "JZ Clear"
  parseExample instruction "Start" "JMP Start"
  fromFile "test/Examples/example1" firstPass
    (fromFoldable [ Tuple "Start" 0
                  , Tuple "Here" 2
                  , Tuple "Clear" 8
                  , Tuple "Loop" 11])
  fromFile "test/Examples/example2" firstPass
    (fromFoldable [ Tuple "Start" 1 ])
  fromFile "test/Examples/example3" firstPass
    (fromFoldable [ Tuple "Foo" 5 ])
  fromFile "test/Examples/example4" firstPass
    (fromFoldable [ Tuple "Rep" 1 ])
  fromFile "test/Examples/example5" firstPass
    (fromFoldable [ Tuple "Rep" 1 ])
  fromFile "test/Examples/example6" firstPass
    (fromFoldable [ Tuple "Start" 0, Tuple "Rep" 10 ])
  fromFile "test/Examples/example1" program
    [ D0 BL (Lit 192)
    , D0 AL (Lit 60)
    , D4 (AddrReg BL) AL
    , DB AL (Lit 123)
    , C1 (Lit 8)
    , A4 AL
    , A4 BL
    , C0 (Lit 2)
    , D0 CL (Lit 64)
    , D0 AL (Lit 32)
    , D0 BL (Lit 192)
    , D4 (AddrReg BL) AL
    , A4 BL
    , A5 CL
    , C2 (Lit 11)
    , C0 (Lit 0) ]

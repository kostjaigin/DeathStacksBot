module DeathStacksFormat where

import System.IO
import Util
import Data.List
import Control.Monad
import Test.HUnit

import DeathStacksBot

isRow r = r `elem` ['1'..'6']
isCol c = c `elem` ['a'..'f']
isDigit c = c `elem` ['0'..'9']

formatMove (a:b:c:d:e:f:g:h:[])
    | (isCol a) && (isRow b) && c == '-' && (isDigit d) && (isDigit e) && f == '-' && (isCol g) && (isRow h) = True
formatMove (a:b:c:d:f:g:h:[])
    | (isCol a) && (isRow b) && c == '-' && (isDigit d) && f == '-' && (isCol g) && (isRow h) = True
formatMove _ = False

formatList s
    | (head s == '[') && (last s == ']') = foldr (\ sm y -> y && (formatMove sm)) True (Util.splitOn "," (init (tail s)) )
formatList _ = False

assertFormat :: String -> (String -> Bool) -> Assertion
assertFormat actual check =
    unless (check actual) (assertFailure msg)
    where msg = "Wrong format! Looks like: \"" ++ actual ++ "\""

--------------------------------------------------------------------------

format = TestList [  (TestLabel "MOVE FORMAT WRONG!" (TestCase (assertFormat (DeathStacksBot.getMove "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb r") formatMove))),
                     (TestLabel "LIST FORMAT WRONG!" (TestCase (assertFormat (DeathStacksBot.listMoves "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb r") formatList))) ]

main :: IO (Counts, Int)
main =  runTestText (putTextToHandle stdout False) format

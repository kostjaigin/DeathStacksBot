--- module (NICHT AENDERN!)
module DeathStacksBot where
--- imports (NICHT AENDERN!)
import Data.Char
import Util

--- external signatures (NICHT AENDERN!)
getMove :: String -> String
listMoves :: String -> String

--- YOUR IMPLEMENTATION STARTS HERE ---
getMove input = if (listMoves input) == "[]" then ""
                else fst(splitAt 7 (tail (listMoves input)))


listMoves input = let positions = parsePosition (parseFENstring input) in
                  let startFieldsInt = map indexToInt (index positions) in
                  let maxMoveCounts = getMaxMovesCountInArray input positions in
                  let arrayOfArraysOfMoveCounts = getArrayOfArraysOfMoveCounts maxMoveCounts in
                  let triple = (zip startFieldsInt arrayOfArraysOfMoveCounts) in
                  let filteredTriple = filter onlyFieldsWithPieces triple in
                  let moveSets = concatMap createTuplesForEndFields filteredTriple in
                  let endsOfMoveSets = map getEndFields moveSets in
                  let newZip = zip moveSets endsOfMoveSets in
                  let moveInts = concatMap createTriplesForMoves newZip in
                  let filteredMoveInts = filter filterMovesFromStartToStartOut moveInts in
                  let moveStrings = concatMap createMoveStringFromSet filteredMoveInts in
                  "[" ++ cutOffLastComma moveStrings ++ "]"


---parses FEN-String and returns seperate String for each Coordinate
parsePosition :: String -> [String]
parsePosition s = splitOn "," (replace (s))

--replace '/' with ',' in FEN-String
replace :: String -> String
replace [] = []
replace (x:xs) =
     if x == '/'
     then ',' : replace xs
     else x : replace xs

-- get positions on board --
indexToInt:: Int -> Int
indexToInt 0  = 61
indexToInt 1  = 62
indexToInt 2  = 63
indexToInt 3  = 64
indexToInt 4  = 65
indexToInt 5  = 66
indexToInt 6  = 51
indexToInt 7  = 52
indexToInt 8  = 53
indexToInt 9  = 54
indexToInt 10 = 55
indexToInt 11 = 56
indexToInt 12 = 41
indexToInt 13 = 42
indexToInt 14 = 43
indexToInt 15 = 44
indexToInt 16 = 45
indexToInt 17 = 46
indexToInt 18 = 31
indexToInt 19 = 32
indexToInt 20 = 33
indexToInt 21 = 34
indexToInt 22 = 35
indexToInt 23 = 36
indexToInt 24 = 21
indexToInt 25 = 22
indexToInt 26 = 23
indexToInt 27 = 24
indexToInt 28 = 25
indexToInt 29 = 26
indexToInt 30 = 11
indexToInt 31 = 12
indexToInt 32 = 13
indexToInt 33 = 14
indexToInt 34 = 15
indexToInt 35 = 16
-- Mapping from numbers to real fields to build move strings later --
intToString :: Int -> String
intToString 61 = "a6"
intToString 62 = "b6"
intToString 63 = "c6"
intToString 64 = "d6"
intToString 65 = "e6"
intToString 66 = "f6"
intToString 51 = "a5"
intToString 52 = "b5"
intToString 53 = "c5"
intToString 54 = "d5"
intToString 55 = "e5"
intToString 56 = "f5"
intToString 41 = "a4"
intToString 42 = "b4"
intToString 43 = "c4"
intToString 44 = "d4"
intToString 45 = "e4"
intToString 46 = "f4"
intToString 31 = "a3"
intToString 32 = "b3"
intToString 33 = "c3"
intToString 34 = "d3"
intToString 35 = "e3"
intToString 36 = "f3"
intToString 21 = "a2"
intToString 22 = "b2"
intToString 23 = "c2"
intToString 24 = "d2"
intToString 25 = "e2"
intToString 26 = "f2"
intToString 11 = "a1"
intToString 12 = "b1"
intToString 13 = "c1"
intToString 14 = "d1"
intToString 15 = "e1"
intToString 16 = "f1"

-- < Eingabe String auf zwei nutzliche Teile teilen > --
parseFENstring :: String -> String
parseFENstring s = fst(splitAt 59 s)

parsePlayerColor :: String -> Char
parsePlayerColor s = last s

---------------------------------------------------------

getMaxMoveCount :: String -> Int
getMaxMoveCount piecesOnStart = length piecesOnStart

goLeftDown :: Int -> Int -> Int
goLeftDown value movesCount = if value > movesCount
                                  then value-movesCount
                                  else goRightUp 1 (movesCount-(value-1))

goRightUp :: Int -> Int -> Int
goRightUp value movesCount = if (6-value) > movesCount
                                then value+movesCount
                                else goLeftDown 6 (movesCount-(6-value))

isFirstElementOfThisStringR :: String -> String -> Bool
isFirstElementOfThisStringR botInput input = if input == "" then False
                                    else (head input) == parsePlayerColor botInput


index :: [String] -> [Int]
index x = [0 .. length x - 1]

getMaxMovesCountInArray :: String -> [String] -> [Int]
getMaxMovesCountInArray botInput positions = map (\x -> if isFirstElementOfThisStringR botInput x then getMaxMoveCount x else 0) positions

tooTallRegel :: [Int] -> [Int]
tooTallRegel positions = if any (>4) positions then map(\x -> if x<5
                                                                then 0
                                                                else x) positions
                                                else positions

getArrayOfArraysOfMoveCounts :: [Int] -> [[Int]]
getArrayOfArraysOfMoveCounts maxMoveCounts = map (\x -> if x /=0 && x <5 then [1 .. x] else (if x>4 then [(x-4) .. x] else [])) (tooTallRegel maxMoveCounts)
-- zerlege die Position Int zu Row und column --
getRow :: Int -> Int
getRow row = div row 10

getColumn :: Int -> Int
getColumn column = mod column 10
------------------------------------------------

-- create Position Int from row and column --
createPositionInt :: Int -> Int -> Int
createPositionInt row column = row*10+column
-----------------------------------------------

-- unique function -> move Strings must be unique --
has :: [Int] -> Int -> Bool
has [] _ = False
has (x:xs) a
  | x == a    = True
  | otherwise = has xs a

unique :: [Int] -> [Int]
unique [] = []
unique (x:xs)
  | has xs x  = unique xs
  | otherwise = x : unique xs
--

-- Get all end field positions as integers from 16 to 66... all 8 combos for one start field
getEndFields :: (Int, Int) -> [Int]
getEndFields (start, mc) = unique ([createPositionInt (goRightUp (getRow start) mc) (getColumn start)] -- up
                                ++[createPositionInt (goRightUp (getRow start) mc) (goRightUp (getColumn start) mc)] -- right up
                                ++[createPositionInt (getRow start) (goRightUp (getColumn start) mc)] -- right
                                ++[createPositionInt (goLeftDown (getRow start) mc) (goRightUp (getColumn start) mc)] -- right down
                                ++[createPositionInt (goLeftDown (getRow start) mc) (getColumn start)] -- down
                                ++[createPositionInt (goLeftDown (getRow start) mc) (goLeftDown (getColumn start) mc)] -- left down
                                ++[createPositionInt (getRow start) (goLeftDown (getColumn start) mc)] -- left
                                ++[createPositionInt (goRightUp (getRow start) mc) (goLeftDown (getColumn start) mc)]) -- left up
-------------------------------------------------------------
-- Create temporar sets
createTuplesForEndFields :: (Int, [Int]) -> [(Int, Int)]
createTuplesForEndFields (start, mc) = zip (replicate (length mc) start) mc

createTriplesForMoves :: ((Int, Int), [Int]) -> [(Int, Int, Int)]
createTriplesForMoves ((start, mc), ends) = zip3 (replicate (length ends) start) (replicate (length ends) mc) ends
-------------------------------------------------------------

-- For Filter function
onlyFieldsWithPieces :: (Int, [Int]) -> Bool
onlyFieldsWithPieces (z, mc) = mc /= []
----------------------

--- create final string ---

createMoveStringFromSet :: (Int, Int, Int) -> String
createMoveStringFromSet (start, mc, end) = (intToString start)++("-")++(show mc)++("-")++(intToString end)++(",")

cutOffLastComma :: String -> String
cutOffLastComma [] = []
cutOffLastComma  s = if [last(s)] == ","
                        then init (s)
                        else s

filterMovesFromStartToStartOut :: (Int, Int, Int) -> Bool
filterMovesFromStartToStartOut (start, mc, end) = (mod mc 10) /= 0 && start /= end

----------------------------

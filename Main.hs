{-# LANGUAGE DoAndIfThenElse #-}
module Main
where

-- Diese Datei ist nicht Teil der Abgabe, sondern
-- nur als Hilfe fuer das Erstellen eines ausfuehrbaren Bots

import DeathStacksBot
import System.Environment


main :: IO ()
main = do
	args <- getArgs
	let oneString = foldr (\x y -> if y == "" then x else x ++ " " ++ y) "" args in
		putStrLn ( getMove oneString )

--
-- EPITECH PROJECT, 2019
-- FUN_deBruijn_2018
-- File description:
-- Main
--

module Main where

import Lib
import System.Environment
import System.Exit
import Numeric.Natural
import Data.List
import Data.Char

--main :: IO ()
main = do
    getArgs >>= parse

parse [n, alphabet, "--check"] = do
    print "check"
    checkNumber n
    if allUnique alphabet == True
        then do
            let x = rotate 1 alphabet
            exit
        else usage >> exitError

parse [n, alphabet, "--unique"] = do
    print "unique"
    checkNumber n
    exit
parse [n, alphabet, "--clean"] = print "clean" >> checkNumber n >> exit
parse [n, "--check"] = do
    checkNumber n
    exit
parse [n, "--unique"] = checkNumber n >> exit
parse [n, "--clean"] = checkNumber n >> exit
parse ["-h"] = usage >> exit
parse otherwise = usage >> exitError

rotate :: Int -> String -> String
rotate n xs = bs ++ as where (as, bs) = splitAt n xs

allUnique ::(Eq a) => [a] -> Bool
allUnique [] = True
allUnique(x:xs)
    | x `notElem` xs && allUnique xs = True
    | otherwise = False

usage = putStrLn "USAGE: ./deBruijn n [a] [--check|--unique|--clean]\n\n\t--check\t\tcheck if a sequence is a de Bruijn sequence\n\t--unique\tcheck if 2 sequences are distinct de Bruijn sequences\n\t--clean\t\tlist cleaning\n\tn\t\torder of the sequence\n\ta\t\talphabet [def: “01”]"

exitError   =   exitWith (ExitFailure 84)
exit    =   exitWith ExitSuccess

checkNumber n = do
    if isNumeric n == True && n /= "0" then return n else usage >> exitError

isNumeric :: String -> Bool
isNumeric str =
    case (reads str) :: [(Natural, String)] of
    [(_, "")]  ->  True
    _   ->  False
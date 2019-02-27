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

main :: IO ()
main = getArgs >>= parse

parse [n, alphabet, "--check"] = print "check" >> checkNumber n >> exit
parse [n, alphabet, "--unique"] = print "unique" >> checkNumber n >> exit
parse [n, alphabet, "--clean"] = print "clean" >> checkNumber n >> exit
parse [n, "--check"] = checkNumber n >> exit
parse [n, "--unique"] = checkNumber n >> exit
parse [n, "--clean"] = checkNumber n >> exit
parse ["-h"] = usage >> exit
parse otherwise = usage >> exitError

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
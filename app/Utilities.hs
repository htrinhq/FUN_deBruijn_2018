--
-- EPITECH PROJECT, 2019
-- FUN_deBruijn_2018
-- File description:
-- Utilities
--

module Utilities (
    allUnique,
    usage,
    exit,
    exitError,
    checkRead,
    checkNumber,
    isNumeric
) where

import Lib
import System.Environment
import System.Exit
import Data.List
import Data.Char
import Data.Maybe
import Text.Read

allUnique :: (Eq a) => [a] -> Bool
allUnique [] = True
allUnique(x:xs) = x `notElem` xs && allUnique xs

usage = putStrLn "USAGE: ./deBruijn n [a] [--check|--unique|--clean]\n\n\t--check\t\tcheck if a sequence is a de Bruijn sequence\n\t--unique\tcheck if 2 sequences are distinct de Bruijn sequences\n\t--clean\t\tlist cleaning\n\tn\t\torder of the sequence\n\ta\t\talphabet [def: “01”]"

exitError   =   exitWith (ExitFailure 84)
exit    =   exitWith ExitSuccess

checkRead :: Maybe Int -> Maybe Int
checkRead mnb =
    if fromJust mnb >= 0
        then mnb
        else Nothing

checkNumber :: String -> Maybe Int
checkNumber n =
    if isNumeric n  && n /= "0"
        then checkRead (readMaybe n)
        else Nothing

isNumeric :: String -> Bool
isNumeric str =
    case (reads str) :: [(Int, String)] of
    [(_, "")] -> True
    _ -> False
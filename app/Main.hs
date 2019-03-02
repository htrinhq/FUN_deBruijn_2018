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
import Data.List
import Data.Char
import Data.Maybe
import Text.Read

main :: IO ()
main = getArgs >>= parse

parse [n, alphabet, "--check"] = do
    let nb = fromJust $ checkNumber n
    if allUnique alphabet
        then do
            input <- getLine
            if check input alphabet nb
                then putStrLn "OK"
                else putStrLn "KO"
        else usage >> exitError

parse [n, alphabet, "--unique"] = do
    let nb = fromJust $ checkNumber n
    if allUnique alphabet
        then do
            input1 <- getLine
            input2 <- getLine
            if check input1 alphabet nb && check input2 alphabet nb && isUnique input1 input2 0
                then putStrLn "OK"
                else putStrLn "KO"
        else usage >> exitError

parse [n, alphabet, "--clean"] = do
    let nb = fromJust $ checkNumber n
    if allUnique alphabet
        then print "clean"
        else usage >> exitError

parse [n, "--check"] = do
    let nb = fromJust $ checkNumber n
    if allUnique "01"
        then do
            input <- getLine
            if check input "01" nb
                then putStrLn "OK"
                else putStrLn "KO"
        else usage >> exitError

parse [n, "--unique"] = do
    let nb = fromJust $ checkNumber n
    if allUnique "01"
        then do
            input1 <- getLine
            input2 <- getLine
            if check input1 "01" nb && check input2 "01" nb && isUnique input1 input2 0
                then putStrLn "OK"
                else putStrLn "KO"
        else usage >> exitError

parse [n, "--clean"] = usage

parse ["-h"] = usage

parse otherwise = usage >> exitError

check :: String -> String -> Int -> Bool
check input alphabet nb = checkInput input alphabet && len == length input && length list == len
    where list = nub $ createTab input nb []
          len = getRealLen alphabet nb

isUnique :: String -> String -> Int -> Bool
isUnique input1 input2 x
    | x >= length input1 = True
    | input1 == input2 = False
    | otherwise = isUnique input1 (rotate input2) (x + 1)

createTab :: String -> Int -> [String] -> [String]
createTab str nb list
    | list == [] = createTab (str ++ (take (nb) str)) nb ((take nb str) : list)
    | length str < nb = list
    | otherwise = createTab (tail str) nb ((take nb str) : list)

rotate :: String -> String
rotate xs = bs ++ as where (as, bs) = splitAt 1 xs

getRealLen :: [Char] -> Int -> Int
getRealLen str n = (length str) ^ n

checkInput :: String -> String -> Bool
checkInput [] alphabet = True
checkInput (x:xs) alphabet = x `elem` alphabet && checkInput xs alphabet

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
    [(_, "")]  ->  True
    _   ->  False
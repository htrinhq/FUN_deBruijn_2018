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

main :: IO ()
main = do
    getArgs >>= parse

parse [n, alphabet, "--check"] = do
    nb <- checkNumber n
    if allUnique alphabet == True
        then do
            input <- getLine
            if checkInput input alphabet == True && getRealLen alphabet nb == length input
                then do
                    putStrLn "OK"
                    exit
                else putStrLn "KO" >> exit
        else usage >> exitError

parse [n, alphabet, "--unique"] = do
    checkNumber n
    if allUnique alphabet == True
        then do
            print "unique"
            exit
        else usage >> exitError

parse [n, alphabet, "--clean"] = do
    checkNumber n
    if allUnique alphabet == True
        then do
            print "clean"
            exit
        else usage >> exitError

parse [n, "--check"] = do
    nb <- checkNumber n
    input <- getLine
    if checkInput input "01" == True && getRealLen "01" nb == length input
        then do
            putStrLn "OK"
            exit
        else putStrLn "KO" >> exit

parse [n, "--unique"] = do
    checkNumber n
    exit

parse [n, "--clean"] = do
    checkNumber n
    exit

parse ["-h"] = do
    usage
    exit

parse otherwise = do
    usage
    exitError

rotate :: String -> String
rotate xs = bs ++ as where (as, bs) = splitAt 1 xs

getRealLen :: [Char] -> Natural -> Int
getRealLen str n = (length str) ^ n

checkInput :: [Char] -> String -> Bool
checkInput []  alphabet = True
checkInput (x:xs) alphabet
    | x `elem` alphabet && checkInput xs alphabet = True
    | otherwise = False

allUnique :: (Eq a) => [a] -> Bool
allUnique [] = True
allUnique(x:xs)
    | x `notElem` xs && allUnique xs = True
    | otherwise = False

usage = putStrLn "USAGE: ./deBruijn n [a] [--check|--unique|--clean]\n\n\t--check\t\tcheck if a sequence is a de Bruijn sequence\n\t--unique\tcheck if 2 sequences are distinct de Bruijn sequences\n\t--clean\t\tlist cleaning\n\tn\t\torder of the sequence\n\ta\t\talphabet [def: “01”]"

exitError   =   exitWith (ExitFailure 84)
exit    =   exitWith ExitSuccess

checkNumber n = do
    if isNumeric n == True && n /= "0" then return (read n :: Natural) else usage >> exitError

isNumeric :: String -> Bool
isNumeric str =
    case (reads str) :: [(Natural, String)] of
    [(_, "")]  ->  True
    _   ->  False
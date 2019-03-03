--
-- EPITECH PROJECT, 2019
-- FUN_deBruijn_2018
-- File description:
-- Flags
--

module Flags (
    printTab,
    filterTab,
    arrayFromInput,
    check,
    isUnique,
    createTab,
    rotate,
    checkInput
) where

import Lib
import System.Environment
import System.Exit
import Data.List
import Data.Char
import Data.Maybe
import Text.Read
import Generation
import Utilities

printTab :: [String] -> IO ()
printTab (x:xs)
    | x == [] || xs == [] = exit
    | otherwise = putStrLn x >> printTab xs

filterTab :: [String] -> [String] -> String -> Int -> [String]
filterTab (x:xs) filtered alphabet nb
    | check x alphabet nb = filterTab xs (x : filtered) alphabet nb
    | xs == [] =  reverse $ nub filtered
    | otherwise = filterTab xs filtered alphabet nb

arrayFromInput :: [String] -> IO [String]
arrayFromInput list = do
    str <- getLine
    if str == "END"
        then return (reverse list)
        else arrayFromInput (str : list)

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

checkInput :: String -> String -> Bool
checkInput [] alphabet = True
checkInput (x:xs) alphabet = x `elem` alphabet && checkInput xs alphabet
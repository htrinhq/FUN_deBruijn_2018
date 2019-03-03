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
import Generation
import Flags
import Utilities

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
        then do
            inputs <- arrayFromInput []
            if inputs /= []
                then do
                    let filtered = filterTab inputs [] alphabet nb
                    if filtered /= []
                        then printTab filtered
                        else exit
                else exit
        else usage >> exitError
parse [n, "--check"] = do
    let nb = fromJust $ checkNumber n
    input <- getLine
    if check input "01" nb
        then putStrLn "OK"
        else putStrLn "KO"
parse [n, "--unique"] = do
    let nb = fromJust $ checkNumber n
    input1 <- getLine
    input2 <- getLine
    if check input1 "01" nb && check input2 "01" nb && isUnique input1 input2 0
        then putStrLn "OK"
        else putStrLn "KO"
parse [n, "--clean"] = do
    let nb = fromJust $ checkNumber n
    inputs <- arrayFromInput []
    if inputs /= []
        then do
            let filtered = filterTab inputs [] "01" nb
            if filtered /= []
                then printTab filtered
                else exit
        else exit
parse ["-h"] = usage
parse [n] = do
    let nb = fromJust $ checkNumber n
    deBruijn nb "01"
parse [n, alphabet] = do
    let nb = fromJust $ checkNumber n
    deBruijn nb alphabet
parse otherwise = usage >> exitError
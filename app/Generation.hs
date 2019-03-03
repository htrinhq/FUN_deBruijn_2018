--
-- EPITECH PROJECT, 2019
-- FUN_deBruijn_2018
-- File description:
-- Generation
--

module Generation (deBruijn,
    createSuit,
    deBruijnGen,
    fillDeBruijnSuit,
    getRealLen) where

import Lib
import System.Environment
import System.Exit
import Data.List
import Data.Char
import Data.Maybe
import Text.Read

deBruijn :: Int -> String -> IO ()
deBruijn nb alphabet = putStrLn $ reverse $ deBruijnGen alphabet (createSuit alphabet [] nb) nb (getRealLen alphabet nb)

createSuit :: String -> String -> Int -> String
createSuit alphabet list nb
    | length list < nb = createSuit alphabet (list ++ [(head alphabet)]) nb
    | otherwise = list

deBruijnGen :: String -> String -> Int -> Int -> String
deBruijnGen alphabet list nb max
    | length list < max = deBruijnGen alphabet (list ++ (fillDeBruijnSuit list alphabet nb)) nb max
    | otherwise = list

fillDeBruijnSuit :: String -> String -> Int -> String
fillDeBruijnSuit list alphabet nb
    | isInfixOf ((drop ((length list) - (nb - 1)) list) ++ [last alphabet]) list = [head alphabet]
    | otherwise = [last alphabet]

getRealLen :: [Char] -> Int -> Int
getRealLen str n = (length str) ^ n
module Main where

import Lib
import System.IO

main :: IO()
main =  do
    hSetBuffering stdout $ BlockBuffering $ Just 1
    putStr "Enter N for the N x N grid: "
    z <- getLine
    putStr "Enter an integer number as the random seed: "
    seed <- getLine
    let z' = read z :: Int
    let zz = z' * z'
    let initGrid = genRand01 zz (read seed :: Int)
    prtGrid z' initGrid

    putStr "Enter how many evolutions you want to play: "
    x <- getLine
    let n = read x::Int
    playGame n z' zz (evolution initGrid z' zz)

{--
main :: IO ()
main = someFunc
--}

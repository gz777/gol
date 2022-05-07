module Lib 
( prtGrid,evolution, playGame, mkGrid,genRand01 ) where

import Data.List
import System.IO
import System.Random (randoms, mkStdGen)

{--
CSC 321 Project Template
4/19/2022 gz All rights reserved.
updated: 5/4/2022 gz
--}

{--
Conway Game of Life
Rules:
   C   N                 new C
   1   0,1             ->  0  # Lonely
   1   4,5,6,7,8       ->  0  # Overcrowded
   1   2,3             ->  1  # Lives
   0   3               ->  1  # It takes three to give birth!
   0   0,1,2,4,5,6,7,8 ->  0  # Barren
--}


{--
brew install haskell-stack
stack ghci --package random
--}


-- Display the grid z items in a row, potentially z * z numbers in [0,1] 
-- z * z grid
mkGrid :: Show a => Int-> [a] -> String 
mkGrid _ [] = "" 
mkGrid z a =  show (take z a) ++ "\n" ++ mkGrid z (drop z a) 

-- Replace 0 with ' ' and 1 with 'o' for better viewing result 5/4/22
repl::Int->Char
repl 0 = ' '
repl 1 = 'o'
repl _ = '@'

-- Generate z*z random 0s and 1s
-- The function uses the following built-in function
-- randoms :: (Random a, RandomGen g) => g -> [a]
genRand01:: Int->Int -> [Int]
genRand01 zz n =  
    map (`mod` 2)  (map abs (take zz randInfList))   
    where
      randInfList = randoms (mkStdGen n) :: [Int]

-- return the a list of indice of neighbors wrt n in list a
-- input: n <- index in the list, a <- the list, z <- grid dim size N
getNeighbors :: Int -> Int -> [Int] -> [Int]
getNeighbors n z a
     | mod n z == 0 = 
        [x | x <- [n-z, n-z-1, n+1, n+z, n+z+1], x >= 0, x < length a, n>=0, n< length a ]
     | mod (n+1) z == 0 =
        [x | x <- [n-z-1, n-z, n-1, n+z-1, n+z], x >= 0, x < length a, n>=0, n< length a ] 
     | otherwise =  
        [x | x <- [ n-z-1, n-z, n-z+1, n-1, n+1, n+z-1, n+z, n+z+1], x >= 0, x < length a, n>=0, n< length a ] 


-- index -> list -> #lives
-- input item's index, n, in the list, the list, a. The grid dim size, z.
-- return sum of neighboring cell values
countLives :: Int -> Int -> [Int] -> Int
countLives n z [] = 0
countLives n z a = sum [ a!!x | x <- (getNeighbors n z a)]

-- input: n <- index of the list, a <- the list, z <- grid dim size
-- output: new value of n  
-- when 3 always 1, regardless to keep or revive 
lifeOrDeath n z a
    | a!!n == 1 && k == 2 = 1
    | k == 3 = 1
    | otherwise = 0
    where k = countLives n z a


-- core function
-- traverse each cell in the grid, z*z cells in total, to re-evaluate the cell value.
evolution :: [Int] -> Int -> Int -> [Int]
evolution = \a z zz -> [ lifeOrDeath n z a | n <- take zz [0,1..]]

-- print grid
prtGrid :: Int -> [Int] -> IO ()
--prtGrid = \z y -> putStrLn (mkGrid z y)
prtGrid = \z y -> putStrLn (mkGrid z (map repl y) ++ "\n")


playGame :: Int -> Int-> Int -> [Int] -> IO()
playGame n z zz l 
    | n==0  = putStrLn ("Game over.")
    | l==[] = error ("No data")
    | n > 0  = prtGrid z l >> playGame (n-1) z zz (evolution l z zz) 

{--
main :: IO()
main =  do
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
--}

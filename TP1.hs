module TP1(negar, andLogico, orLogico, sumatoria, longitud, promedio) where

-- Pattern Matching

negar :: Bool -> Bool
negar True = False
negar False = True

andLogico :: Bool -> Bool -> Bool
andLogico True True = True
andLogico _ _ = False

orLogico :: Bool -> Bool -> Bool
orLogico False False = False
orLogico _ _ = True



-- Recursividad

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs
longitud _ = 0

promedio :: [Int] -> Int
promedio l = sumatoria l `div` longitud l

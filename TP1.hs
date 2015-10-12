module TP1(sumatoria, longitud, promedio) where

-- Recursividad

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

promedio :: [Int] -> Int
promedio l = sumatoria l `div` longitud l

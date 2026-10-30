module Laboratorio06 where

-- 1. Potencia --------------------------------------------------------------

-- Precondicion: el exponente es mayor o igual que cero.
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia base exponente =
  base * potencia base (exponente - 1)

potenciaCola :: Int -> Int -> Int
potenciaCola = undefined

potenciaCPS :: Int -> Int -> (Int -> r) -> r
potenciaCPS = undefined

-- 2. Longitud de una lista -------------------------------------------------

longitud :: [a] -> Int
longitud [] = 0
longitud (_ : xs) = 1 + longitud xs

longitudCola :: [a] -> Int -> Int
longitudCola [] acc = acc
longitudCola (x:xs) = longitudCola xs (1 + x)

longitudCPS :: [a] -> (Int -> r) -> r
longitudCPS [] k = k []
longitudCPS (x:xs) k = longitudCPS xs (\v -> k(1 + v)) 

-- 3. Reversa de una lista --------------------------------------------------

reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs ++ [x]

reversaCola :: [a] -> [a] -> [a]
reversaCola [] acc = acc
reversaCola (x:xs) acc = reversaCola xs ([x] ++ acc)

reversaCPS :: [a] -> ([a] -> r) -> r
reversaCPS [] k = k []
reversaCPS (x:xs) k = reversaCPS xs (\v -> k(v ++[x]))

-- 4. Concatenacion de una lista de listas ---------------------------------

concatena :: [[a]] -> [a]
concatena [] = []
concatena (xs : xss) = xs ++ concatena xss

concatenaCola :: [[a]] -> [a]
concatenaCola = undefined

concatenaCPS :: [[a]] -> ([a] -> r) -> r
concatenaCPS = undefined

-- 5. Intercalado de dos listas --------------------------------------------

intercala :: [a] -> [a] -> [a]
intercala [] ys = ys
intercala xs [] = xs
intercala (x : xs) (y : ys) =
  x : y : intercala xs ys

intercalaCola :: [a] -> [a] -> [a]
intercalaCola = undefined

intercalaCPS :: [a] -> [a] -> ([a] -> r) -> r
intercalaCPS = undefined

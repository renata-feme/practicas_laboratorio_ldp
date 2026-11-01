module Laboratorio06 where

-- 1. Potencia --------------------------------------------------------------

-- Precondicion: el exponente es mayor o igual que cero.
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia base exponente =
  base * potencia base (exponente - 1)

potenciaCola :: Int -> Int -> Int
potenciaCola base exponente = potenciaAux base exponente 1

potenciaAux :: Int -> Int -> Int -> Int
potenciaAux a 0 acc = acc
potenciaAux a b acc =potenciaAux a (b-1) (acc*a)

potenciaCPS :: Int -> Int -> (Int -> r) -> r
potenciaCPS a 0 k = k 1
potenciaCPS a b k = potenciaCPS a (b-1) (\v -> k (a *v))

-- 2. Longitud de una lista -------------------------------------------------

longitud :: [a] -> Int
longitud [] = 0
longitud (_ : xs) = 1 + longitud xs

longitudCola :: [a] -> Int
longitudCola xs = longitudAux xs 0

longitudAux :: [a] -> Int -> Int
longitudAux [] acc = acc
longitudAux (x:xs) acc =longitudAux xs (1+acc)

longitudCPS :: [a] -> (Int -> r) -> r
longitudCPS [] k = k 0
longitudCPS (x:xs) k = longitudCPS xs (\v -> k (1+v))

-- 3. Reversa de una lista --------------------------------------------------

reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs ++ [x]

reversaCola :: [a] -> [a]
reversaCola xs = reversaAux xs []

reversaAux :: [a] -> [a] -> [a]
reversaAux [] acc= acc
reversaAux (x:xs) acc = reversaAux xs (x:acc)

reversaCPS :: [a] -> ([a] -> r) -> r
reversaCPS [] k = k []
reversaCPS (x:xs) k = reversaCPS xs (\v -> k (v++ [x]))

-- 4. Concatenacion de una lista de listas ---------------------------------

concatena :: [[a]] -> [a]
concatena [] =[]
concatena (xs : xss) = xs ++ concatena xss

concatenaCola :: [[a]] -> [a]
concatenaCola xss = concatenaAux xss []

concatenaAux :: [[a]] -> [a] -> [a]
concatenaAux [] acc =acc
concatenaAux (xs:xss) acc = concatenaAux xss (acc++xs)

concatenaCPS :: [[a]] -> ([a] -> r) -> r
concatenaCPS [] k = k []
concatenaCPS (xs:xss) k = concatenaCPS xss (\v -> k (xs++v ))

-- 5. Intercalado de dos listas --------------------------------------------

intercala :: [a] -> [a] -> [a]
intercala [] ys = ys
intercala xs [] = xs
intercala (x : xs) (y : ys) = x:y :intercala xs ys

intercalaCola :: [a] -> [a] -> [a]
intercalaCola xs ys = intercalaAux xs ys []

intercalaAux :: [a] -> [a] -> [a] -> [a]
intercalaAux [] ys acc = acc ++ ys
intercalaAux xs [] acc = acc++ xs
intercalaAux (x:xs) (y:ys) acc =intercalaAux xs ys (acc ++[x,y])

intercalaCPS :: [a] -> [a] -> ([a] -> r) -> r
intercalaCPS [] ys k = k ys
intercalaCPS xs [] k = k xs
intercalaCPS (x:xs) (y:ys) k = intercalaCPS xs ys (\v -> k (x:y:v) )

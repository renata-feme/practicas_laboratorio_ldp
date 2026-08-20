module Laboratorio01 where

distanciaOrigen :: Double -> Double -> Double
distanciaOrigen x y = sqrt ((x * x) + (y * y))

sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares [] = 0
sumaCuadradosPares (x) = sum (map (^2) (filter even x))


aplicaTresVeces :: (a -> a) -> a -> a
aplicaTresVeces f x = f ( f ( f x))

varianza2 :: Double -> Double -> Double
varianza2 x1 x2 = 
      let m = (x1 + x2) /2 in ((x1 - m)*(x1 - m) + (x2 - m)*(x2 - m)) / 2  


clasificaTemperatura :: Int -> String
clasificaTemperatura n 
    | n < 1 = "frio extremo"
    | n <= 15 = "frio"
    | 15 < n && n <= 25 = "templado"
    | 26 <= n && n <= 35 = "calido"
    | otherwise = "calor extremo"

intercala :: a -> [a] -> [a]  
intercala a [] = []
intercala a [b] = [b]
intercala a (x:y) = (x : a : intercala a y)   

data Expr
  = Lit Int
  | Suma Expr Expr
  | Producto Expr Expr
  deriving (Eq, Show)

evalua :: Expr -> Int
evalua (Lit a) = a
evalua (Suma a b) = evalua a + evalua b
evalua (Producto a b) = evalua a * evalua b
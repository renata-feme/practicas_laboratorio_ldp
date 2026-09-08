module Interp where

import Grammars

-- RETO 3: sustitucion nominal que evita captura
freeVars :: ASA -> [String]
freeVars (Num _) = []
freeVars (Id x) = [x]
freeVars (Boolean _) = []
freeVars (Not e) = freeVars e
freeVars (Add1 e) = freeVars e
freeVars (Sub1 e) = freeVars e
freeVars (ZeroP e) = freeVars e
freeVars (Expt e1 e2) = freeVars e1 ++ freeVars e2
freeVars (EqP e1 e2) = freeVars e1 ++ freeVars e2
freeVars (And e) = freeVarListas e
freeVars (Or e) = freeVarListas e
freeVars (Add e) = freeVarListas e
freeVars (Sub e) = freeVarListas e
freeVars (Mul e) = freeVarListas e
freeVars (Div e) = freeVarListas e
freeVars (Lt e) = freeVarListas e
freeVars (Gt e) = freeVarListas e
freeVars (Le e) = freeVarListas e
freeVars (Ge e) = freeVarListas e
freeVars (Let e1 e2) = asaDeBindings e1 ++ varsDiferentesListas (freeVars e2) (varDeBindings e1)
freeVars (LetStar [] e2) = freeVars e2
freeVars (LetStar [x] e2) = freeVars (Let [x] e2)
freeVars (LetStar (x : cola) e2) = freeVars (Let [x] (LetStar cola e2))

freeVarListas :: [ASA] -> [String]
freeVarListas [] = []
freeVarListas (x : xs) = freeVars x ++ freeVarListas xs

asaDeBindings :: [Binding] -> [String]
asaDeBindings [] = []
asaDeBindings ((_, b) : cola) = freeVars b ++ asaDeBindings cola

varDeBindings :: [Binding] -> [String]
varDeBindings [] = []
varDeBindings ((a, _) : cola) = a : varDeBindings cola

varsDiferentesListas :: [String] -> [String] -> [String]
varsDiferentesListas [] _ = []
varsDiferentesListas (x : xs) ligadores
  | elem x ligadores = varsDiferentesListas xs ligadores
  | otherwise = x : varsDiferentesListas xs ligadores



names :: ASA -> [String]
names (Num _) = []
names (Id x) = [x]
names (Boolean _) = []
names (Not e) = names e
names (Add1 e) = names e
names (Sub1 e) = names e
names (ZeroP e) = names e
names (Expt e1 e2) = names e1 ++ names e2
names (EqP e1 e2) = names e1 ++ names e2
names (And e) = namesListas e
names (Or e) = namesListas e
names (Add e) = namesListas e
names (Sub e) = namesListas e
names (Mul e) = namesListas e
names (Div e) = namesListas e
names (Lt e) = namesListas e
names (Gt e) = namesListas e
names (Le e) = namesListas e
names (Ge e) = namesListas e
names (Let e1 e2) = varDeBindings e1 ++ namesDeBindings e1 ++ names e2
names (LetStar e1 e2) = varDeBindings e1 ++ namesDeBindings e1 ++ names e2


namesListas :: [ASA] -> [String]
namesListas [] = []
namesListas (x : xs) = names x ++ namesListas xs

namesDeBindings :: [Binding] -> [String]
namesDeBindings [] = []
namesDeBindings ((_, b) : cola) = names b ++ namesDeBindings cola


freshName :: [String] -> String
freshName varsUsadas = checaNombreNuevo 0 varsUsadas

checaNombreNuevo :: Int -> [String] -> String
checaNombreNuevo n varsUsadas
  | elem ("x" ++ show n) varsUsadas = checaNombreNuevo (n + 1) varsUsadas
  | otherwise = "x" ++ show n

interseccion :: [String] -> [String] -> [String]
interseccion xs ys = [ x | x <- xs, elem x ys ]

frescosPara :: [String] -> [String] -> [(String, String)]
frescosPara [] _ = []
frescosPara (v : vs) evitar =
  let nuevo = freshName evitar
   in (v, nuevo) : frescosPara vs (nuevo : evitar)

nuevoNombre :: [(String, String)] -> String -> String
nuevoNombre frescos v = case lookup v frescos of
  Just n -> n
  Nothing -> v

renombraBindings :: [(String, String)] -> [Binding] -> [Binding]
renombraBindings frescos bs = [ (nuevoNombre frescos v, ex) | (v, ex) <- bs ]

renombraCuerpo :: [(String, String)] -> ASA -> ASA
renombraCuerpo [] e = e
renombraCuerpo ((v, n) : resto) e = renombraCuerpo resto (sust e v (Id n))

pegaAlFrente :: Binding -> ASA -> ASA
pegaAlFrente b (LetStar bs e2) = LetStar (b : bs) e2
pegaAlFrente b otro = LetStar [b] otro

sust :: ASA -> String -> ASA -> ASA
sust (Num n) _ _ = Num n
sust (Boolean b) _ _ = Boolean b
sust (Id y) x s
  | y == x = s
  | otherwise = Id y
sust (Not e) x s = Not (sust e x s)
sust (Add1 e) x s = Add1 (sust e x s)
sust (Sub1 e) x s = Sub1 (sust e x s)
sust (ZeroP e) x s = ZeroP (sust e x s)
sust (Expt e1 e2) x s = Expt (sust e1 x s) (sust e2 x s)
sust (EqP e1 e2) x s = EqP (sust e1 x s) (sust e2 x s)
sust (And e) x s = And (sustListas e x s)
sust (Or e) x s = Or (sustListas e x s)
sust (Add e) x s = Add (sustListas e x s)
sust (Sub e) x s = Sub (sustListas e x s)
sust (Mul e) x s = Mul (sustListas e x s)
sust (Div e) x s = Div (sustListas e x s)
sust (Lt e) x s = Lt (sustListas e x s)
sust (Gt e) x s = Gt (sustListas e x s)
sust (Le e) x s = Le (sustListas e x s)
sust (Ge e) x s = Ge (sustListas e x s)
sust (Let e1 e2) x s
  -- Caso 1
  | elem x (varDeBindings e1) = Let (sustBindings e1 x s) e2
  -- Caso 2
  | null (interseccion (varDeBindings e1) (freeVars s)) =
      Let (sustBindings e1 x s) (sust e2 x s)
  -- Caso 3:
  | otherwise =
      let peligrosos = interseccion (varDeBindings e1) (freeVars s)
          frescos = frescosPara peligrosos (freeVars s ++ names e2 ++ varDeBindings e1)
          e1' = renombraBindings frescos e1
          e2' = renombraCuerpo frescos e2
       in Let (sustBindings e1' x s) (sust e2' x s)
sust (LetStar [] e2) x s = LetStar [] (sust e2 x s)
sust (LetStar ((w, ew) : cola) e2) x s
  | w == x = LetStar ((w, sust ew x s) : cola) e2
  | elem w (freeVars s) =
      let n = freshName (freeVars s ++ names (LetStar cola e2) ++ [w])
          resto = sust (LetStar cola e2) w (Id n)
       in pegaAlFrente (n, sust ew x s) (sust resto x s)
  | otherwise =
      pegaAlFrente (w, sust ew x s) (sust (LetStar cola e2) x s)

sustListas :: [ASA] -> String -> ASA -> [ASA]
sustListas [] _ _ = []
sustListas (e : cola) x s = sust e x s : sustListas cola x s

sustBindings :: [Binding] -> String -> ASA -> [Binding]
sustBindings [] _ _ = []
sustBindings ((y, ex) : cola) x s = (y, sust ex x s) : sustBindings cola x s

sustMany :: ASA -> [Binding] -> ASA
sustMany (Id y) amb = case lookup y amb of
  Just t -> t
  Nothing -> Id y
sustMany (Num n) _ = Num n
sustMany (Boolean b) _ = Boolean b
sustMany (Not e) amb = Not (sustMany e amb)
sustMany (Add1 e) amb = Add1 (sustMany e amb)
sustMany (Sub1 e) amb = Sub1 (sustMany e amb)
sustMany (ZeroP e) amb = ZeroP (sustMany e amb)
sustMany (Expt a b) amb = Expt (sustMany a amb) (sustMany b amb)
sustMany (EqP a b) amb = EqP (sustMany a amb) (sustMany b amb)
sustMany (And e) amb = And (sustManyListas e amb)
sustMany (Or e) amb = Or (sustManyListas e amb)
sustMany (Add e) amb = Add (sustManyListas e amb)
sustMany (Sub e) amb = Sub (sustManyListas e amb)
sustMany (Mul e) amb = Mul (sustManyListas e amb)
sustMany (Div e) amb = Div (sustManyListas e amb)
sustMany (Lt e) amb = Lt (sustManyListas e amb)
sustMany (Gt e) amb = Gt (sustManyListas e amb)
sustMany (Le e) amb = Le (sustManyListas e amb)
sustMany (Ge e) amb = Ge (sustManyListas e amb)
sustMany (Let e1 e2) amb =
  Let (sustManyBindings e1 amb)
      (sustMany e2 (quitaClaves (varDeBindings e1) amb))
sustMany (LetStar [] e2) amb = LetStar [] (sustMany e2 amb)
sustMany (LetStar ((w, ew) : cola) e2) amb =
  pegaAlFrente (w, sustMany ew amb)
               (sustMany (LetStar cola e2) (quitaClaves [w] amb))

sustManyListas :: [ASA] -> [Binding] -> [ASA]
sustManyListas [] _ = []
sustManyListas (e : cola) amb = sustMany e amb : sustManyListas cola amb

sustManyBindings :: [Binding] -> [Binding] -> [Binding]
sustManyBindings [] _ = []
sustManyBindings ((w, ex) : cola) amb = (w, sustMany ex amb) : sustManyBindings cola amb

quitaClaves :: [String] -> [Binding] -> [Binding]
quitaClaves ks amb = [ (k, v) | (k, v) <- amb, notElem k ks ]


-- RETO 4: semantica operacional de paso grande
-- let es simultaneo; let* se evalua directamente, asociacion por asociacion.
bigStep :: ASA -> Maybe ASA
bigStep (Num n) = Just (Num n)
bigStep (Boolean b) = Just (Boolean b)
bigStep (Id _) = Nothing
bigStep (Add e) = aplicaNum e sum
bigStep (Mul e) = aplicaNum e product
bigStep (Sub e) = aplicaNum e (foldl1 monus)
bigStep (Div e) = case evaluaTodos e of
  Nothing -> Nothing
  Just vs -> case numeros vs of
    Nothing -> Nothing
    Just ns
      | divisoresNoCero ns -> Just (Num (foldl1 div ns))
      | otherwise -> Nothing
bigStep (And e) = aplicaBool e and
bigStep (Or e) = aplicaBool e or
bigStep (Lt e) = aplicaComp e (<)
bigStep (Gt e) = aplicaComp e (>)
bigStep (Le e) = aplicaComp e (<=)
bigStep (Ge e) = aplicaComp e (>=)
bigStep (Expt e1 e2) = case (bigStep e1, bigStep e2) of
  (Just (Num n), Just (Num m)) -> Just (Num (n ^ m))
  _ -> Nothing
bigStep (EqP e1 e2) = case (bigStep e1, bigStep e2) of
  (Just (Num a), Just (Num b)) -> Just (Boolean (a == b))
  (Just (Boolean a), Just (Boolean b)) -> Just (Boolean (a == b))
  _ -> Nothing
bigStep (Not e) = case bigStep e of
  Just (Boolean b) -> Just (Boolean (not b))
  Just (Num _) -> Just (Boolean False)
  _ -> Nothing
bigStep (Add1 e) = case bigStep e of
  Just (Num n) -> Just (Num (n + 1))
  _ -> Nothing
bigStep (Sub1 e) = case bigStep e of
  Just (Num n) -> Just (Num (monus n 1))
  _ -> Nothing
bigStep (ZeroP e) = case bigStep e of
  Just (Num n) -> Just (Boolean (n == 0))
  _ -> Nothing
bigStep (Let e1 e2)
  | not (distintos (varDeBindings e1)) = Nothing
  | otherwise = case evaluaTodos (expresionesDeBindings e1) of
      Nothing -> Nothing
      Just vs -> bigStep (sustMany e2 (zip (varDeBindings e1) vs))
bigStep (LetStar [] e2) = bigStep e2
bigStep (LetStar ((x, e) : cola) e2) = case bigStep e of
  Nothing -> Nothing
  Just v -> bigStep (sust (LetStar cola e2) x v)

monus :: Int -> Int -> Int
monus a b = if a > b then a - b else 0

evaluaTodos :: [ASA] -> Maybe [ASA]
evaluaTodos [] = Just []
evaluaTodos (e : es) = case bigStep e of
  Nothing -> Nothing
  Just v -> case evaluaTodos es of
    Nothing -> Nothing
    Just vs -> Just (v : vs)

numeros :: [ASA] -> Maybe [Int]
numeros [] = Just []
numeros (Num n : vs) = case numeros vs of
  Just ns -> Just (n : ns)
  Nothing -> Nothing
numeros _ = Nothing

booleanos :: [ASA] -> Maybe [Bool]
booleanos [] = Just []
booleanos (Boolean b : vs) = case booleanos vs of
  Just bs -> Just (b : bs)
  Nothing -> Nothing
booleanos _ = Nothing

aplicaNum :: [ASA] -> ([Int] -> Int) -> Maybe ASA
aplicaNum e f = case evaluaTodos e of
  Nothing -> Nothing
  Just vs -> case numeros vs of
    Nothing -> Nothing
    Just ns -> Just (Num (f ns))

aplicaBool :: [ASA] -> ([Bool] -> Bool) -> Maybe ASA
aplicaBool e f = case evaluaTodos e of
  Nothing -> Nothing
  Just vs -> case booleanos vs of
    Nothing -> Nothing
    Just bs -> Just (Boolean (f bs))

aplicaComp :: [ASA] -> (Int -> Int -> Bool) -> Maybe ASA
aplicaComp e rel = case evaluaTodos e of
  Nothing -> Nothing
  Just vs -> case numeros vs of
    Nothing -> Nothing
    Just ns -> Just (Boolean (encadena rel ns))

encadena :: (Int -> Int -> Bool) -> [Int] -> Bool
encadena rel (a : b : resto) = rel a b && encadena rel (b : resto)
encadena _ _ = True

divisoresNoCero :: [Int] -> Bool
divisoresNoCero [] = True
divisoresNoCero (_ : resto) = notElem 0 resto

distintos :: [String] -> Bool
distintos [] = True
distintos (x : xs) = notElem x xs && distintos xs

expresionesDeBindings :: [Binding] -> [ASA]
expresionesDeBindings bs = [ ex | (_, ex) <- bs ]

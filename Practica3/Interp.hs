module Interp where

import Grammars

-- RETO 3: sustitucion nominal que evita captura
freeVars :: ASA -> [String]
freeVars (Num n) = []
freeVars (Id x) = [x]
freeVars (Boolean b) = []
freeVars (Not e) = freeVars e
freeVars (Add1 e) = freeVars e
freeVars (Sub1 e )= freeVars e
freeVars (ZeroP e)= freeVars e
freeVars (Expt e1 e2) = freeVars e1 ++ freeVars e2
freeVars (EqP e1 e2) = freeVars e1 ++ freeVars e2
freeVars (And e ) = freeVarListas e 
freeVars (Or e ) = freeVarListas e
freeVars (Add e ) = freeVarListas e
freeVars (Sub e ) = freeVarListas e
freeVars (Mul e ) = freeVarListas e
freeVars (Div e ) = freeVarListas e
freeVars (Lt e ) = freeVarListas e
freeVars (Gt e ) = freeVarListas e
freeVars (Le e ) = freeVarListas e
freeVars (Ge e ) = freeVarListas e
freeVars (Let e1 e2) = asaDeBindings e1 ++ varsDiferentesListas (freeVars e2) (varDeBindings e1)
freeVars (LetStar [x] e2) = freeVars (Let [x] e2)
freeVars (LetStar (x : cola) e2) = freeVars (Let [x] (LetStar cola e2))

freeVarListas :: [ASA] -> [String]
freeVarListas [] = []
freeVarListas (x : xs) = freeVars x ++ freeVarListas xs

asaDeBindings :: [Binding] -> [String]
asaDeBindings [] = []
asaDeBindings ((a,b) : cola ) = freeVars b ++ asaDeBindings cola

varDeBindings :: [Binding] -> [String]
varDeBindings [] = []
varDeBindings ((a,b): cola) = a: varDeBindings cola

varsDiferentesListas :: [String] -> [String] -> [String]
varsDiferentesListas [] ligadores = []
varsDiferentesListas (x:xs) ligadores
  | elem x ligadores = varsDiferentesListas xs ligadores
  | otherwise = x : varsDiferentesListas xs ligadores



names :: ASA -> [String]
names :: ASA -> [String]
names (Num n) = []
names (Id x) = []
names (Boolean b) = []
names (Not e) = names e
names (Add1 e) = names e
names (Sub1 e )= names e
names (ZeroP e)= names e
names (Expt e1 e2) = names e1 ++ names e2
names (EqP e1 e2) = names e1 ++ names e2
names (And e ) = namesListas e 
names (Or e ) = namesListas e
names (Add e ) = namesListas e
names (Sub e ) = namesListas e
names (Mul e ) = namesListas e
names (Div e ) = namesListas e
names (Lt e ) = namesListas e
names (Gt e ) = namesListas e
names (Le e ) = namesListas e
names (Ge e ) = namesListas e
names (Let e1 e2) = varDeBindings e1 ++ namesDeBindings e1 + name e2
names (LetStar e1 e2)= varDeBindings e1 ++ namesDeBindings e1 ++ names e2


namesListas :: [ASA] -> [String]
namesListas [] = []
namesListas (x:xs) = names x ++ namesListas xs

namesDeBindings :: [Binding] -> [String]
namesDeBindings [] = []
namesDeBindings ((a,b) : cola ) = names b ++ namesDeBindings cola


freshName :: [String] -> String
freshName varsUsadas = checaNombreNuevo 0 varsUsadas

checaNombreNuevo :: Int -> [String] -> String
checaNombreNuevo n varsUsadas
  | elem ("x" ++ show n) varsUsadas = checaNombreNuevo (n + 1) varsUsadas
  |otherwise = "x" ++ show n




sust :: ASA -> String -> ASA -> ASA
sust (Num n) x s = (Num n)
sust (Boolean b) x s = (Boolean b)
sust (Id y) x s
  |(y == x) = s
  | otherwise = (Id y)
sust (Not e) x s = Not (sust e x s)
sust (Add1 e) x s = Add1 (sust e x s)
sust (Sub1 e) x s = Sub1 (sust e x s)
sust (ZeroP e) x s = ZeroP (sust e x s)
sust (Expt e1 e2) x s = Expt (sust e1 x s) (sust e2 x s)
sust (EqP e1 e2) x s  = EqP (sust e1 x s) (sust e2 x s)
sust (And e) x s = And (sustListas e x s)
sust (Or e) x s = Or (sustListas e x s)
sust (Add e) x s= Add (sustListas e x s)
sust (Sub e) x s = Sub (sustListas e x s)
sust (Mul e) x s = Mul (sustListas e x s)
sust (Div e) x s = Div (sustListas e x s)
sust (Lt e) x s = Lt (sustListas e x s)
sust (Gt e) x s = Gt (sustListas e x s)
sust (Le e) x s= Le (sustListas e x s)
sust (Ge e) x s = Ge (sustListas e x s)
sust (Let e1 e2) x s
  -- Caso 1
  | elem x (varDeBindings e1) = Let (sustBindings e1 x s) e2
  -- Caso 2
  | 
  -- Caso 3:
  | 

sustListas :: [ASA] -> String -> ASA -> [ASA]
sustListas [] x s = []
sustListas (e : cola) x s = sust e x s : sustListas cola x s


sustBindings :: [Binding] -> String -> ASA -> [Binding]
sustBindings [] x s = []
sustBindings ((y, ex): cola) x s  = (y, sust ex x s) : sustBindings cola x s

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

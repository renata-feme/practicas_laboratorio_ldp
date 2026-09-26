module Interp where

import Grammars

data ASA
  = Id Nombre
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  | Fun Nombre ASA
  | App ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 1: desazucarado ----------------------------------------------------

-- Convierte una lista no vacia de parametros distintos en funciones
-- unarias anidadas. El primer parametro queda en la funcion exterior.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] x = Nothing
curryFun [x] e =  Just (Fun x e)
curryFun (x:xs) e
  | x `elem` xs = Nothing
  | otherwise = case curryFun xs e of 
      Just bodyCurrficado -> Just (Fun x bodyCurrficado)
      Nothing -> Nothing


-- Convierte una aplicacion con uno o mas argumentos en aplicaciones unarias
-- asociadas por la izquierda.
curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp e [] = Nothing
curryApp e (xs) = Just (foldl App e xs)

-- Convierte dos o mas operandos en operaciones binarias asociadas por la
-- izquierda. El constructor recibido sera Add o Sub.
binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp sum_res [] = Nothing
binaryOp sum_res [x] = Nothing
binaryOp sum_res (x1:x2:xs) = Just ( foldl sum_res (sum_res x1 x2) xs)

-- Convierte las ligaduras de let* en let anidados y despues elimina cada let
-- mediante LetS x e1 e2 ==> App (Fun x e2') e1'. La primera ligadura debe
-- quedar en el let exterior para que las siguientes puedan usarla.
desugar :: SASA -> Maybe ASA
desugar (IdS x) = Just (Id x)
desugar (NumS n) = Just (Num n)
desugar (BooleanS b ) = Just (Boolean b)
desugar (AddS xs) = case traverse desugar xs of
  Just xs_1 -> binaryOp Add xs_1
  Nothing -> Nothing
desugar (SubS xs) = case traverse desugar xs of
  Just xs_1 -> binaryOp Sub xs_1
  Nothing -> Nothing
desugar (NotS e) = case desugar e of
  Just (e1) -> Just (Not e1)
  Nothing -> Nothing
desugar (FunS p b) = case desugar b of
  Just body -> curryFun p body 
  Nothing -> Nothing
desugar (AppS f a) = case (desugar f, traverse desugar a) of
  (Just f1, Just a1) -> curryApp f1 a1
  x -> Nothing
desugar (LetS x e1 e2) = case (desugar e1, desugar e2) of
  (Just ex1, Just ex2) -> Just(App (Fun x ex2) ex1)
  x -> Nothing
desugar (LetStarS [] body) = desugar body
desugar (LetStarS ((x, v) : cola ) body) = case (desugar v, desugar (LetStarS cola body)) of
  (Just v1, Just cola1) -> Just( App(Fun x cola1) v1)
  a -> Nothing



-- RETO 2: evaluacion con cerraduras ---------------------------------------

-- Busca la asociacion mas reciente de un identificador.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv x env = lookup x env

-- Evalua con alcance estatico. Fun produce una cerradura con el ambiente
-- actual. App evalua primero la posicion de funcion, despues el argumento y
-- por ultimo el cuerpo en el ambiente guardado por la cerradura.
-- La aplicacion es ansiosa: el argumento se exige aunque el cuerpo no lo use.
-- Conserva la resta truncada y la convencion de que todo numero cuenta como
-- verdadero cuando aparece como operando de Not.
bigStep :: Env -> ASA -> Maybe Value

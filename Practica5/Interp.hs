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
  | If ASA ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  | ExprV ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 3: desazucarado ----------------------------------------------------

-- Recupera estas funciones del laboratorio 4. Las funciones y aplicaciones
-- del nucleo siguen siendo unarias, y las operaciones siguen siendo binarias.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] x = Nothing
curryFun [x] e =  Just (Fun x e)
curryFun (x:xs) e
  | x `elem` xs = Nothing
  | otherwise = fmap (Fun x) (curryFun xs e)

curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp e [] = Nothing
curryApp e (xs) = Just (foldl App e xs)

binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp sum_res [] = Nothing
binaryOp sum_res [x] = Nothing
binaryOp sum_res (x1:x2:xs) = Just ( foldl sum_res (sum_res x1 x2) xs)

junta2 :: (a -> b -> Maybe c ) -> Maybe a -> Maybe b -> Maybe c
junta2 f (Just x) (Just y) = f x y 
junta2 f a b = Nothing

junta3 :: (a -> b -> c -> Maybe d ) -> Maybe a -> Maybe b -> Maybe c -> Maybe d
junta3 f (Just x) (Just y) (Just z) = f x y z
junta3 f a b c = Nothing

-- Desazucara las clausulas ordinarias de cond en If anidados. La alternativa
-- else es el ultimo argumento y se conserva como la rama final.
desugarCond :: [(SASA, SASA)] -> SASA -> Maybe ASA
desugarCond [] ramaElse = desugar ramaElse
desugarCond ((condicion, rama1) : cola) ramaElse =
  junta3 armaIf (desugar condicion) (desugar rama1) (desugarCond cola ramaElse) where
    armaIf con ram alter = Just (If con ram alter)

-- Elimina toda la sintaxis superficial. CondS se traduce a If anidados.
-- LetRecS f definicion cuerpo se traduce usando el identificador Y:
--
--   LetS f (AppS (IdS "Y") (FunS [f] definicion)) cuerpo
--
-- y despues se elimina tambien ese LetS. LetRecS no pertenece al nucleo.
desugar :: SASA -> Maybe ASA
desugar (IdS x) = Just (Id x)
desugar (NumS n) = Just (Num n)
desugar (BooleanS b) = Just (Boolean b)
desugar (AddS xs) = junta2 binaryOp (Just Add) (traverse desugar xs)
desugar (SubS xs) = junta2 binaryOp (Just Sub) (traverse desugar xs)
desugar (NotS e ) = fmap Not (desugar e)
desugar (FunS p b) = junta2 curryFun (Just p) (desugar b)
desugar (AppS f a) = junta2 curryApp (desugar f) (traverse desugar a)
desugar (LetS x e1 e2) =
  junta2 armaLet (desugar e1) (desugar e2) where
    armaLet  val cola = Just(App (Fun x cola) val)
desugar (LetStarS [] e2) = desugar e2
desugar (LetStarS ((x, v) : cola) e2) =
  junta2 armaLetStar (desugar v) (desugar (LetStarS cola e2)) where
    armaLetStar val cola = Just (App (Fun x cola) val)
desugar (IfS condi consec e ) =
  junta3 armaIf (desugar condi) (desugar consec) (desugar e) where
    armaIf condi1 consec1 e1 = Just (If condi1 consec1 e1)
desugar (CondS clausulas ramaElse) = desugarCond clausulas ramaElse
desugar (LetRecS f def cuerpo) = desugar (LetS f (AppS (IdS "Y") [FunS [f] def]) cuerpo)

-- RETO 4: evaluacion perezosa con alcance estatico ------------------------

-- Busca la asociacion mas reciente sin exigir su contenido.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv x env = lookup x env

-- Exige una cerradura de expresion usando el ambiente guardado. Si al
-- evaluarla se obtiene otra ExprV, continua hasta producir otro valor.
strict :: Value -> Maybe Value
strict (NumV n) = Just(NumV n)
strict (BooleanV b) = Just (BooleanV b)
strict (ClosureV p c env) = Just (ClosureV p c env)
strict (ExprV asa env)
  | Just v <- bigStep env asa = strict v 
  | otherwise = Nothing

-- Semantica de paso grande con alcance estatico y evaluacion perezosa.
--
-- * Id devuelve directamente la asociacion encontrada.
-- * Fun produce ClosureV con el ambiente de definicion.
-- * App exige la posicion de funcion, pero liga el argumento como
--   ExprV argumento ambienteDeLaLlamada.
-- * Add, Sub y Not exigen sus operandos.
-- * If exige solamente la condicion y evalua una sola rama.
--
-- La resta sobre naturales permanece truncada en cero.
bigStep :: Env -> ASA -> Maybe Value
bigStep env (Id x) = lookupEnv x env
bigStep env (Num n) = Just (NumV n)
bigStep env (Boolean b) = Just (BooleanV b)
bigStep env (Fun p c) = Just (ClosureV p c env)
bigStep env (Add e1 e2)
  | (Just v1, Just v2) <- (bigStep env e1, bigStep env e2),
    Just (NumV n) <- strict v1,
    Just (NumV m) <- strict v2 = Just(NumV (n + m))
  | otherwise = Nothing
bigStep env (Sub e1 e2)
  | (Just v1, Just v2) <- (bigStep env e1, bigStep env e2),
    Just (NumV n) <- strict v1,
    Just (NumV m) <- strict v2 = Just(NumV (max 0 (n - m)))
  | otherwise = Nothing
bigStep env (Not e)
  | Just v <- bigStep env e,
    Just (BooleanV b) <- strict v = Just (BooleanV (not b))
  | Just v <- bigStep env e,
    Just (NumV n) <- strict v = Just (BooleanV False)
  | otherwise = Nothing
bigStep env (If condi consec e)
  | Just v <- bigStep env condi,
    Just (BooleanV True) <- strict v = bigStep env consec
  | Just v <- bigStep env condi,
    Just (BooleanV False) <- strict v = bigStep env e 
  | otherwise = Nothing
bigStep env (App f a )
  | Just vf <- bigStep env f,
    Just (ClosureV p c envDef) <- strict vf =
      let cerraduraExpr = ExprV a env
        in bigStep ((p, cerraduraExpr): envDef) c 
  | otherwise = Nothing

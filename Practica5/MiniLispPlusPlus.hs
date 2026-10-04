module MiniLispPlusPlus where

import Control.Monad.IO.Class (liftIO)
import Grammars
import Interp
import Lexer
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT)

-- RETO 5: integrar el combinador Y ----------------------------------------

-- Representa en el ASA del nucleo el combinador clasico:
--
-- Y = lambda f.
--       (lambda x. f (x x))
--       (lambda x. f (x x))
combinadorY :: ASA
combinadorY = 
  let autoAplicacion = Fun "x" (App (Id "f") (App (Id "x") (Id "x"))) in
    Fun "f" (App autoAplicacion autoAplicacion)

-- Evalua combinadorY en el ambiente vacio y asocia su valor con el nombre Y.
prelude :: Env
prelude = 
  let Just valorY = bigStep [] combinadorY
    in [("Y", valorY)]

-- Integra el analisis, el desazucarado y la evaluacion desde prelude.
-- El resultado final debe pasar por strict antes de devolverse.
evalua :: String -> Maybe Value
evalua entrada
  | Just asa <- desugar (parse(lexer entrada)),
    Just val <- bigStep prelude asa = strict val
  |otherwise = Nothing

-- Infraestructura provista. No forma parte de los retos.
repl :: IO ()
repl = runInputT defaultSettings loop

loop :: InputT IO ()
loop =
  getInputLine "MiniLisp++> " >>= maybe (pure ()) procesaEntrada

procesaEntrada :: String -> InputT IO ()
procesaEntrada ":q" = pure ()
procesaEntrada entrada =
  liftIO (maybe muestraBloqueo print (evalua entrada)) >> loop

muestraBloqueo :: IO ()
muestraBloqueo =
  putStrLn "Error: evaluacion bloqueada"

main :: IO ()
main = repl

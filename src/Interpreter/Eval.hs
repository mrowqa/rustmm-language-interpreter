{-# Options -Wall #-}

module Interpreter.Eval (
    evalProgram,
    typeCheck,
) where
import Interpreter.Defs
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans

type EvalResult = (String, Maybe String) -- stdout, stderr
type TypeCheckResult = Either String ()

evalProgram :: Exp -> EvalResult
evalProgram _ = ("", Nothing)

typeCheck :: Exp -> TypeCheckResult
typeCheck prog = Right ()


initState :: EvalState ()
initState = undefined

nextId :: EvalState Int
nextId = undefined

--save


typeCheckImpl :: Exp -> Interpreter ()
typeCheckImpl (EBlock defs) = do
    initTopDefs defs
    return ()

initTopDefs :: [Exp] -> Interpreter [(Var, (Loc, Bool))]
initTopDefs [] = return []
initTopDefs (x:xs) = do
    let (ELet mut name val) = x
    l <- lift $ nextId
    lift $ modify (Map.insert l val)
    return $ (val, (l, mut)) : (initTopDefs xs)


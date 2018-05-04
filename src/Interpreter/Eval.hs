{-# Options -Wall #-}

module Interpreter.Eval (
    evalProgram,
    staticCheck,
) where
import Interpreter.Defs
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
--import Control.Monad.Trans

type EvalResult = (String, Maybe String) -- stdout, stderr
type StaticCheckResult = Either String ()

evalProgram :: Exp -> EvalResult
evalProgram _ = ("", Nothing)

-- checks if variables are defined, mutability and types
staticCheck :: Exp -> StaticCheckResult
staticCheck prog = fst $ runState (runExceptT (runReaderT (staticCheckImpl prog) Map.empty)) Map.empty


nextId :: EvalState Integer
nextId = do
    m <- get
    let k = case Map.lookup nextIdKey m of Just (VInt k') -> k'
                                           Just _ -> error "unreachable"
                                           Nothing -> 1
    modify (Map.insert nextIdKey $ VInt $ k+1)
    return k
  where nextIdKey = 0

staticCheckImpl :: Exp -> Interpreter ()
staticCheckImpl (EBlock defs) = do
    env <- initTopDefs defs
    withReaderT (Map.union env) $ helper defs
  where helper :: [Exp] -> Interpreter ()  -- almost fmap ; is there nicer way to do this?
        helper [] = return ()
        helper (x:xs) = do staticCheckSingleExp x
                           helper xs
staticCheckImpl _ = error "unreachable"

staticCheckSingleExp :: Exp -> Interpreter ()
staticCheckSingleExp (ELet mut name exp') = do
    error "not implemented yet :("
staticCheckSingleExp _ = error "not implemented yet :("

initTopDefs :: [Exp] -> Interpreter Env
initTopDefs [] = return Map.empty
initTopDefs (x:xs) = do
    let (ELet mut name (ELitVal val)) = x
    l <- lift $ nextId
    lift $ modify (Map.insert l val)
    env <- initTopDefs xs
    case Map.lookup name env of
        Just _ ->  throwError $ "Error: function named " ++ name ++ " already defined!"
        Nothing -> return $ Map.insert name (l, mut) env


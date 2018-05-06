{-# Options -Wall #-}

module Interpreter.Eval (
    evalProgram,
) where
import Interpreter.Defs
--import Interpreter.Primitives
--import qualified Data.Map as Map
--import Control.Monad.Except
--import Control.Monad.State
--import Control.Monad.Reader
--import Control.Monad.Trans

type EvalResult = (String, Maybe String) -- stdout, stderr

evalProgram :: Exp -> EvalResult
evalProgram _ = ("", Nothing) -- TODO

---- checks if variables are defined, mutability and types
--staticCheck :: Exp -> StaticCheckResult
--staticCheck prog = fst $ runState (runExceptT (runReaderT (staticCheckImpl prog) Map.empty)) Map.empty


--nextId :: EvalState Integer
--nextId = do
--    m <- get
--    let k = case Map.lookup nextIdKey m of Just (VInt k') -> k'
--                                           Just _ -> error "unreachable"
--                                           Nothing -> 1
--    modify (Map.insert nextIdKey $ VInt $ k+1)
--    return k
--  where nextIdKey = 0

--staticCheckImpl :: Exp -> Interpreter ()
--staticCheckImpl (EBlock defs) = do
--    env <- initTopDefs defs
--    maybe_main <- withReaderT (Map.union env) (getVar mainFn)
--    case maybe_main of
--        Nothing -> throwError $ "Error: main function not defined"
--        Just (_,_,v) -> do
--            if typeOf v /= mainFnType
--                then throwError $ "Error: main function must have signature " ++ show mainFnType ++ " (found: " ++ show (typeOf v) ++ ")"
--                else do ts <- withReaderT (Map.union env) $ staticCheckMultipleExp defs
--                        if not (all (==TUnit) ts)  -- enforce calculation
--                            then error "unreachable"
--                            else return ()
--staticCheckImpl _ = error "unreachable"

--staticCheckSingleExp :: Exp -> Interpreter Type
--staticCheckSingleExp (ELet mut name exp' cont) = do
--    t <- staticCheckSingleExp exp'
--    l <- lift $ nextId
--    lift $ modify (Map.insert l $ defaultValueOf t)
--    withReaderT (Map.insert name (l, mut)) $ staticCheckSingleExp cont

--staticCheckSingleExp (EFnCall fn args) = do
--    TFn args_t ret_t <- staticCheckSingleExp fn
--    args_t' <- staticCheckMultipleExp args
--    if args_t == args_t'
--        then return ret_t
--        else throwError $ "Error: expected types " ++ (show args_t) ++ ", got: " ++ (show args_t')

--staticCheckSingleExp (EVar var) = do
--    maybe_var <- getVar var
--    case maybe_var of
--        Nothing -> throwError $ "Error: undefined variable " ++ var
--        Just (_,_,v) -> return $ typeOf v

--staticCheckSingleExp (ELitVal val) = do
--    case val of
--        VFn (TFn args_t ret_t) vars body -> do
--            args_env <- argsEnv vars args_t  -- TODO rehandle error to add info about position
--            ret_t' <- withReaderT (Map.union args_env) $ staticCheckSingleExp body
--            if ret_t == ret_t' then return ret_t
--                               else throwError $ "Error: function declares to return " ++ show ret_t ++ ", but last expression is of type " ++ show ret_t'
--        _ -> return $ typeOf val
--  where argsEnv :: [(Bool, Var)] -> [Type] -> Interpreter Env
--        argsEnv [] [] = return Map.empty
--        argsEnv ((mut, name):vs) (t:ts) = do
--            l <- lift $ nextId
--            lift $ modify (Map.insert l $ defaultValueOf t)
--            env <- argsEnv vs ts
--            case Map.lookup name env of
--                Just _ ->  throwError $ "Error: multiple arguments with the same name " ++ name
--                Nothing -> return $ Map.insert name (l, mut) env
--        argsEnv _ _ = error "unreachable"

--staticCheckSingleExp (ETakeRef mut var) = do
--    maybe_var <- getVar var
--    case maybe_var of
--        Nothing -> throwError $ "Error: undefined variable " ++ var
--        Just (_,m,v) -> do
--            case typeOf v of
--                TRef _ _ -> throwError $ "Error: reference to reference is forbidden (variable " ++ var ++ ")"
--                TFn _ _ -> throwError $ "Error: reference to function is forbidden (variable " ++ var ++ ")"
--                TUnit -> throwError $ "Error: reference to unit is forbidden (variable " ++ var ++ ")"
--                t -> if m == False && mut == True
--                        then throwError $ "Error: cannot take mutable reference to immutable variable " ++ var
--                        else return t


--staticCheckSingleExp (EBlock exps) = do
--    ts <- staticCheckMultipleExp exps
--    --error $ show exps ++ " | " ++ show ts -- TODO remove me
--    if not $ all (==TUnit) (init ts)  -- enforcement of type checking
--        then throwError $ "Error: all but last expression in block must return unit" -- ++ show exps ++ " | " ++ show ts -- TODO remove me
--        else return $ last ts

--staticCheckSingleExp (EAssign var exp') = do
--    t <- staticCheckSingleExp exp'
--    maybe_var <- getVar var
--    case maybe_var of
--        Nothing -> throwError $ "Error: undefined variable " ++ var
--        Just (_,False,_) -> throwError $ "Error: variable " ++ var ++ " is immutable" -- TODO make exception for references!
--        Just (_,_,v) -> do
--            if t /= typeOf v  -- TODO coercing mutable reference to immutable
--                then throwError $ "Error: variable " ++ var ++ " is of type " ++ (show $ typeOf v) ++ " and tried to assign value of type " ++ (show t)
--                else return TUnit


--getVar :: Var -> Interpreter (Maybe (Loc, Bool, Value))
--getVar var = do
--    maybe_loc <- asks (Map.lookup var)
--    case maybe_loc of  -- i couldn't apply here Maybe monad :(
--        Nothing -> return Nothing
--        Just (l, mut) -> do Just val <- lift $ gets (Map.lookup l)
--                            return $ Just (l, mut, val)

--staticCheckMultipleExp :: [Exp] -> Interpreter [Type]
--staticCheckMultipleExp [] = return []
--staticCheckMultipleExp (x:xs) = do
--    t <- staticCheckSingleExp x
--    ts <- staticCheckMultipleExp xs
--    return (t:ts)


--initTopDefs :: [Exp] -> Interpreter Env
--initTopDefs [] = return Map.empty
--initTopDefs (x:xs) = do
--    let (ELet mut name (ELitVal val) _) = x
--    l <- lift $ nextId
--    lift $ modify (Map.insert l val)
--    env <- initTopDefs xs
--    case Map.lookup name env of
--        Just _ ->  throwError $ "Error: function named " ++ name ++ " already defined!"
--        Nothing -> return $ Map.insert name (l, mut) env

--typeOf :: Value -> Type
--typeOf (VInt _) = TInt
--typeOf (VBool _) = TBool
--typeOf VUnit = TUnit
--typeOf (VFn t _ _) = t
--typeOf (VRef _) = undefined -- TODO hmm...

--defaultValueOf :: Type -> Value
--defaultValueOf TInt = VInt 0
--defaultValueOf TBool = VBool True
--defaultValueOf TUnit = VUnit
--defaultValueOf _t@(TFn _ _) = undefined -- VFn _t [] emptyBlock -- TODO done wrong :( -- TODO check what is used by static checker
--defaultValueOf (TRef _ _) = undefined -- TODO hmm..

---- TODO impl show for Type
---- TODO impl show for Exp
---- TODO impl show for Value
---- TODO tests (* for each error msg)


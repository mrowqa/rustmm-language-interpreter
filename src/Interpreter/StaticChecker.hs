{-# Options -Wall #-}

module Interpreter.StaticChecker (
    staticCheck,
) where
import Interpreter.Defs
import Interpreter.Eval  -- for builtins
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

type StaticCheckResult = Either String ()

-- checks if variables are defined, mutability and types
staticCheck :: Exp -> StaticCheckResult
staticCheck prog = fst $ runState (runExceptT (runReaderT (staticCheckImpl prog) Map.empty)) Map.empty


nextId :: StaticCheckState Integer
nextId = do
    m <- get
    return $ fromIntegral $ Map.size m

staticCheckImpl :: Exp -> StaticChecker ()
staticCheckImpl (EBlock defs) = do
    bltnEnv <- initBuiltIns
    env <- withReaderT (Map.union bltnEnv) $ initTopDefs defs
    maybe_main <- withReaderT (Map.union env) (getVar mainFn)
    case maybe_main of
        Nothing -> throwError $ "Error: main function not defined"
        Just (_,_,t) -> do
            if t /= mainFnType
                then throwError $ "Error: main function must have signature " ++ show mainFnType ++ " (found: " ++ show t ++ ")"
                else do ts <- withReaderT (Map.union env) $ checkMultipleExp defs
                        if not (all (==TUnit) ts)  -- enforce calculation
                            then error "unreachable"
                            else return ()
staticCheckImpl _ = error "unreachable"

initBuiltIns :: StaticChecker StaticCheckEnv
initBuiltIns = helper builtInsRegister
  where helper :: [(Var, Value)] -> StaticChecker StaticCheckEnv
        helper [] = ask
        helper ((name, val):vs) = do
            l <- lift $ nextId
            lift $ modify (Map.insert l $ typeOf val)
            env <- helper vs
            return $ Map.insert name (l, False) env

initTopDefs :: [Exp] -> StaticChecker StaticCheckEnv
initTopDefs [] = ask
initTopDefs (x:xs) = do
    let (ELet mut name (ELitVal val) _) = x
    l <- lift $ nextId
    lift $ modify (Map.insert l $ typeOf val)
    env <- initTopDefs xs
    case Map.lookup name env of
        Just _ ->  throwError $ "Error: function named " ++ name ++ " already defined!"
        Nothing -> return $ Map.insert name (l, mut) env


checkSingleExp :: Exp -> StaticChecker Type
checkSingleExp (ELet mut name exp' cont) = do
    t <- checkSingleExp exp'
    l <- lift $ nextId
    lift $ modify (Map.insert l t)
    withReaderT (Map.insert name (l, mut)) $ checkSingleExp cont

checkSingleExp (EFnCall fn args) = do
    TFn args_t ret_t <- checkSingleExp fn
    args_t' <- checkMultipleExp args
    if args_t == args_t'
        then return ret_t
        else throwError $ "Error: expected types " ++ (show args_t) ++ ", got: " ++ (show args_t')

checkSingleExp (EVar deref var) = do
    maybe_var <- getVar var
    case maybe_var of
        Nothing -> throwError $ "Error: undefined variable " ++ var
        Just (_,_,t) -> case (deref, t) of
            (True, TRef _ t') -> return t'
            (True, _) -> throwError $ "Error: variable " ++ var ++ " of type " ++ show t ++ " cannot be dereferenced"
            (False, _) -> return t

checkSingleExp (ELitVal val) = do
    case val of
        VFn valType@(TFn args_t ret_t) vars body _ -> do
            args_env <- argsEnv vars args_t  -- TODO rehandle error to add info about position
            ret_t' <- withReaderT (Map.union args_env) $ checkSingleExp body
            if ret_t == ret_t' then return valType
                               else throwError $ "Error: function declares to return " ++ show ret_t ++ ", but last expression is of type " ++ show ret_t'
        _ -> return $ typeOf val
  where argsEnv :: [(Bool, Var)] -> [Type] -> StaticChecker StaticCheckEnv
        argsEnv [] [] = return Map.empty
        argsEnv ((mut, name):vs) (t:ts) = do
            l <- lift $ nextId
            lift $ modify (Map.insert l t)
            env <- argsEnv vs ts
            case Map.lookup name env of
                Just _ ->  throwError $ "Error: multiple arguments with the same name " ++ name
                Nothing -> return $ Map.insert name (l, mut) env
        argsEnv _ _ = error "unreachable"

checkSingleExp (ETakeRef mut var) = do
    maybe_var <- getVar var
    case maybe_var of
        Nothing -> throwError $ "Error: undefined variable " ++ var
        Just (_,m,t) -> do
            case t of
                TRef _ _ -> throwError $ "Error: reference to reference is forbidden; variable " ++ var ++ " is already a reference"
                TFn _ _ -> throwError $ "Error: reference to function is forbidden (variable " ++ var ++ ")"
                TUnit -> throwError $ "Error: reference to unit is forbidden (variable " ++ var ++ ")"
                _ -> if m == False && mut == True
                        then throwError $ "Error: cannot take mutable reference to immutable variable " ++ var
                        else return $ TRef mut t


checkSingleExp (EBlock exps) = do
    ts <- checkMultipleExp exps
    if not $ all (==TUnit) (init ts)  -- enforcement of type checking
        then throwError $ "Error: all but last expression in block must return unit; did you forget to check value of some expression?"
        else return $ last ts

checkSingleExp (EAssign deref var exp') = do
    t <- checkSingleExp exp'
    maybe_var <- getVar var
    case (deref, maybe_var) of
        (_, Nothing) -> throwError $ "Error: undefined variable " ++ var
        (False, Just (_, False, _)) -> throwError $ "Error: variable " ++ var ++ " is immutable"
        (False, Just (_, _, t')) | t == t' -> return TUnit
        (True, Just (_, _, TRef True t'')) | t == t'' -> return TUnit
        (_, Just (_, _, t')) -> throwError $ "Error: variable " ++ var ++ " is of type " ++ show t' ++ " and tried to assign value of type " ++ show t ++ (if deref then " (while derefencing the variable)" else "")

checkSingleExp (EBuiltIn _) = error "unreachable"

checkSingleExp (EIf cond brTrue brFalse) = do
    [tCond, tBrTrue, tBrFalse] <- checkMultipleExp [cond, brTrue, brFalse]
    if tCond /= TBool
        then throwError $ "Error: if condition must be bool, got: " ++ show tCond
        else if tBrTrue /= tBrFalse
            then throwError $ "Error: both branches of if statement must be of the same type, got: " ++ show tBrTrue ++ " and " ++ show tBrFalse
            else return tBrTrue


getVar :: Var -> StaticChecker (Maybe (Loc, Bool, Type))
getVar var = do
    maybe_loc <- asks (Map.lookup var)
    case maybe_loc of  -- i couldn't apply here Maybe monad :(
        Nothing -> return Nothing
        Just (l, mut) -> do Just typ <- lift $ gets (Map.lookup l)
                            return $ Just (l, mut, typ)

checkMultipleExp :: [Exp] -> StaticChecker [Type]
checkMultipleExp [] = return []
checkMultipleExp (x:xs) = do
    t <- checkSingleExp x
    ts <- checkMultipleExp xs
    return (t:ts)


typeOf :: Value -> Type
typeOf (VInt _) = TInt
typeOf (VBool _) = TBool
typeOf VUnit = TUnit
typeOf (VFn t _ _ _) = t
typeOf (VRef _) = error "unreachable"  -- literal can't be VRef with internal reference
typeOf (VString _) = error "unreachable"  -- used only for stdout during eval

-- TODO tests (* for each error msg)
-- TODO check variable naming convention!


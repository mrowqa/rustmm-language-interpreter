{-# Options -Wall #-}
{-# LANGUAGE BangPatterns #-}

module Interpreter.Eval (
    evalProgram,
    builtInsRegister,
    bltnWhileVar,
) where
import Interpreter.Defs
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader


type EvalResult = (String, Maybe String) -- stdout, stderr

evalProgram :: Exp -> EvalResult
evalProgram prog = let (result, endState) = runState (runExceptT (runReaderT (evalImpl prog) initEnv)) initStore in
    let Just (VString stdout) = Map.lookup stdoutLoc endState in
    case result of
        Left stderr -> (stdout, Just stderr)
        Right () -> (stdout, Nothing)

initEnv :: Env
initEnv = Map.fromList [(stdoutVar, stdoutLoc), (nextIdVar, nextIdLoc)]

initStore :: Store
initStore = Map.fromList [(stdoutLoc, VString ""), (nextIdLoc, VInt 0)]

stdoutVar :: Var
stdoutVar = "@stdout@"
stdoutLoc :: Loc
stdoutLoc = (-2)

nextIdVar :: Var
nextIdVar = "@nextId@"
nextIdLoc :: Loc
nextIdLoc = (-1)


nextId :: Interpreter Integer
nextId = do
    VInt k <- getValueAtLoc nextIdLoc
    updateValueAtLoc nextIdLoc $ VInt (k+1)
    return k


evalImpl :: Exp -> Interpreter ()
evalImpl (EBlock defs) = do
    bltnEnv <- initBuiltIns
    env <- withReaderT (Map.union bltnEnv) $ initTopDefs defs
    (_,main) <- withReaderT (Map.union env) (getVar mainFn)
    let VFn _ _ body = main
    VUnit <- withReaderT (Map.union env) $ evalSingleExp body
    return ()
evalImpl _ = error "unreachable"

initBuiltIns :: Interpreter Env
initBuiltIns = helper builtInsRegister
  where helper :: [(Var, Value)] -> Interpreter Env
        helper [] = ask
        helper ((name, val):vs) = do
            l <- nextId
            updateValueAtLoc l val
            env <- helper vs
            return $ Map.insert name l env


initTopDefs :: [Exp] -> Interpreter Env
initTopDefs [] = ask
initTopDefs (x:xs) = do
    let (ELet _ name (ELitVal val) _) = x
    l <- nextId
    updateValueAtLoc l val
    env <- initTopDefs xs
    return $ Map.insert name l env


evalSingleExp :: Exp -> Interpreter Value
evalSingleExp (ELet _ name exp' cont) = do
    val <- evalSingleExp exp'
    l <- nextId
    updateValueAtLoc l val
    withReaderT (Map.insert name l) $ evalSingleExp cont

evalSingleExp (EFnCall fn args) = do
    fnVal <- evalSingleExp fn
    argsVals <- evalMultipleExp args
    let VFn _ argsMutNames body = fnVal
    let (_, argsNames) = unzip argsMutNames
    env <- argsEnv argsNames argsVals
    case body of
        EBuiltIn impl -> impl argsVals
        _ -> withReaderT (Map.union env) $ evalSingleExp body
  where argsEnv :: [Var] -> [Value] -> Interpreter Env
        argsEnv [] [] = return Map.empty
        argsEnv (n:ns) (v:vs) = do
            l <- nextId
            updateValueAtLoc l v
            env <- argsEnv ns vs
            return $ Map.insert n l env
        argsEnv _ _ = error "unreachable"

evalSingleExp (EVar deref var) = do
    (_, val) <- getVar var
    case (deref, val) of
        (True, VRef l) -> getValueAtLoc l
        (True, _) -> error "unreachable"
        (False, _) -> return val

evalSingleExp (ELitVal val) = return val

evalSingleExp (ETakeRef _ var) = do
    (l, _) <- getVar var
    return $ VRef l

evalSingleExp (EBlock exps) = do
    !vs <- evalMultipleExp exps  -- enforcing evaluation of all expressions
    return $ last vs

evalSingleExp (EAssign deref var exp') = do
    newVal <- evalSingleExp exp'
    (l, oldVal) <- getVar var
    case (deref, oldVal) of
        (True, VRef l') -> updateValueAtLoc l' newVal
        (True, _) -> error "unreachable"
        (False, _) -> updateValueAtLoc l newVal
    return VUnit

evalSingleExp (EBuiltIn _) = error "unreachable"


evalMultipleExp :: [Exp] -> Interpreter [Value]
evalMultipleExp [] = return []
evalMultipleExp (x:xs) = do
    v <- evalSingleExp x
    vs <- evalMultipleExp xs
    return (v:vs)


getVar :: Var -> Interpreter (Loc, Value)
getVar var = do
    Just l <- asks (Map.lookup var)
    val <- getValueAtLoc l
    return (l, val)

getValueAtLoc :: Loc -> Interpreter Value
getValueAtLoc l = do
    Just val <- lift $ gets (Map.lookup l)
    return val

-- not used
--updateVar :: Var -> Value -> Interpreter ()
--updateVar var val = do
--    (l,_) <- getVar var
--    updateValueAtLoc l val

updateValueAtLoc :: Loc -> Value -> Interpreter ()
updateValueAtLoc l val = do
    lift $ modify (Map.insert l val)
    return ()


-- TODO impl show for Type
-- TODO impl show for Exp
-- TODO impl show for Value

--------------------------------------------------------
------------ built ins ---------------------------------
--------------------------------------------------------

builtInsRegister :: [(Var, Value)]
builtInsRegister = [
        ("printInt", printInt),
        ("printBool", printBool),
        (bltnWhileVar, bltnWhile)
    ]

bltnWhileVar :: Var
bltnWhileVar = "_bltn_@while"

--bltnIf :: Exp
--bltnIf = EVar "_bltn_@if"

--bltnNeg :: Exp
--bltnNeg = EVar "_bltn_@neg"

--bltnMul :: Exp
--bltnMul = EVar "_bltn_@mul"

--bltnDiv :: Exp
--bltnDiv = EVar "_bltn_@div"

--bltnAdd :: Exp
--bltnAdd = EVar "_bltn_@add"

--bltnSub :: Exp
--bltnSub = EVar "_bltn_@sub"

bltnWhile :: Value
bltnWhile = VFn (TFn [TFn [] TBool, TFn [] TUnit] TUnit) [(False, "cond"), (False, "body")]
    (EBuiltIn (\[VFn _ _ cond, VFn _ _ body] -> while cond body))
  where while :: Exp -> Exp -> Interpreter Value
        while cond body = do
            VBool condVal <- evalSingleExp cond
            if condVal
                then do VUnit <- evalSingleExp body
                        while cond body
                else return VUnit

printInt :: Value
printInt = VFn (TFn [TInt] TUnit) [(False, "arg")]
    (EBuiltIn (\[VInt arg] -> do VString stdout <- getValueAtLoc stdoutLoc
                                 updateValueAtLoc stdoutLoc $ VString (stdout ++ show arg ++ "\n")
                                 return VUnit))

printBool :: Value
printBool = VFn (TFn [TBool] TUnit) [(False, "arg")]
    (EBuiltIn (\[VBool arg] -> do VString stdout <- getValueAtLoc stdoutLoc
                                  updateValueAtLoc stdoutLoc $ VString (stdout ++ show arg ++ "\n")
                                  return VUnit))


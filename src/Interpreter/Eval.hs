{-# Options -Wall #-}
{-# LANGUAGE BangPatterns #-}

module Interpreter.Eval (
    evalProgram,
    builtInsRegister,
    bltnWhileVar,
    bltnUnary,
    bltnBinary,
    bltnNegVar,
    bltnMulVar,
    bltnDivVar,
    bltnModVar,
    bltnAddVar,
    bltnSubVar,
    bltnLtVar,
    bltnLeVar,
    bltnGtVar,
    bltnGeVar,
    bltnEqVar,
    bltnNeVar,
    bltnNotVar,
    bltnAndVar,
    bltnOrVar,
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
    withReaderT (Map.union env) $ updateTopDefsEnv defs
    (_,main) <- withReaderT (Map.union env) (getVar mainFn)
    let VFn _ _ body _ = main
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

updateTopDefsEnv :: [Exp] -> Interpreter ()
updateTopDefsEnv [] = return ()
updateTopDefsEnv (x:xs) = do
    env <- ask
    let (ELet _ name _ _) = x
    (l, v) <- getVar name
    let VFn t args body _ = v
    let v' = VFn t args body env
    updateValueAtLoc l v'
    updateTopDefsEnv xs


evalSingleExp :: Exp -> Interpreter Value
evalSingleExp (ELet _ name exp' cont) = do
    val <- evalSingleExp exp'
    l <- nextId
    updateValueAtLoc l val
    withReaderT (Map.insert name l) $ evalSingleExp cont

evalSingleExp (EFnCall fn args) = do
    fnVal <- evalSingleExp fn
    argsVals <- evalMultipleExp args
    let VFn _ argsMutNames body fnEnv = fnVal
    let (_, argsNames) = unzip argsMutNames
    env <- argsEnv argsNames argsVals
    case body of
        EBuiltIn impl -> impl argsVals
        _ -> withReaderT (\_ -> Map.union env fnEnv) $ evalSingleExp body
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

evalSingleExp (ELitVal val) = do
    env <- ask
    return $ case val of
            VFn t args body _ -> VFn t args body env
            _ -> val

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

evalSingleExp (EIf cond brTrue brFalse) = do
    VBool condVal <- evalSingleExp cond
    evalSingleExp $ if condVal then brTrue else brFalse


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
        (bltnWhileVar, bltnWhile),
        (bltnNegVar, bltnNeg),
        (bltnMulVar, bltnMul),
        (bltnDivVar, bltnDiv),
        (bltnModVar, bltnMod),
        (bltnAddVar, bltnAdd),
        (bltnSubVar, bltnSub),
        (bltnLtVar, bltnLt),
        (bltnLeVar, bltnLe),
        (bltnGtVar, bltnGt),
        (bltnGeVar, bltnGe),
        (bltnEqVar, bltnEq),
        (bltnNeVar, bltnNe),
        (bltnNotVar, bltnNot),
        (bltnAndVar, bltnAnd),
        (bltnOrVar, bltnOr)
    ]

bltnUnary :: Var -> Exp -> Exp
bltnUnary opVar exp' = EFnCall (EVar False opVar) [exp']

bltnBinary :: Var -> Exp -> Exp -> Exp
bltnBinary opVar e0 e1 = EFnCall (EVar False opVar) [e0, e1]


bltnWhileVar :: Var
bltnWhileVar = "_bltn_@while"

bltnNegVar :: Var
bltnNegVar = "_bltn_@neg"

bltnMulVar :: Var
bltnMulVar = "_bltn_@mul"

bltnDivVar :: Var
bltnDivVar = "_bltn_@div"

bltnModVar :: Var
bltnModVar = "_bltn_@mod"

bltnAddVar :: Var
bltnAddVar = "_bltn_@add"

bltnSubVar :: Var
bltnSubVar = "_bltn_@sub"

bltnLtVar :: Var
bltnLtVar = "_bltn_@lt"

bltnLeVar :: Var
bltnLeVar = "_bltn_@le"

bltnGtVar :: Var
bltnGtVar = "_bltn_@gt"

bltnGeVar :: Var
bltnGeVar = "_bltn_@ge"

bltnEqVar :: Var
bltnEqVar = "_bltn_@eq"

bltnNeVar :: Var
bltnNeVar = "_bltn_@ne"

bltnNotVar :: Var
bltnNotVar = "_bltn_@not"

bltnAndVar :: Var
bltnAndVar = "_bltn_@and"

bltnOrVar :: Var
bltnOrVar = "_bltn_@or"


bltnWhile :: Value
bltnWhile = VFn (TFn [TFn [] TBool, TFn [] TUnit] TUnit) [(False, "cond"), (False, "body")]
    (EBuiltIn (\[VFn _ _ cond _, VFn _ _ body _] -> while cond body)) Map.empty
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
                                 return VUnit)) Map.empty

printBool :: Value
printBool = VFn (TFn [TBool] TUnit) [(False, "arg")]
    (EBuiltIn (\[VBool arg] -> do VString stdout <- getValueAtLoc stdoutLoc
                                  updateValueAtLoc stdoutLoc $ VString (stdout ++ show arg ++ "\n")
                                  return VUnit)) Map.empty

bltnNeg :: Value
bltnNeg = VFn (TFn [TInt] TInt) [(False, "arg")]
    (EBuiltIn (\[VInt arg] -> return $ VInt (-arg))) Map.empty

bltnMul :: Value
bltnMul = VFn (TFn [TInt, TInt] TInt) [(False, "n0"), (False, "n1")]
    (EBuiltIn (\[VInt n0, VInt n1] -> return $ VInt (n0 * n1))) Map.empty


bltnDiv :: Value
bltnDiv = VFn (TFn [TInt, TInt] TInt) [(False, "n0"), (False, "n1")]
    (EBuiltIn (\[VInt n0, VInt n1] -> if n1 == 0
                                          then throwError "Error: division by zero"
                                          else return $ VInt (n0 `div` n1))) Map.empty

bltnMod :: Value
bltnMod = VFn (TFn [TInt, TInt] TInt) [(False, "n0"), (False, "n1")]
    (EBuiltIn (\[VInt n0, VInt n1] -> if n1 == 0
                                          then throwError "Error: division by zero"
                                          else return $ VInt (n0 `mod` n1))) Map.empty


bltnAdd :: Value
bltnAdd = VFn (TFn [TInt, TInt] TInt) [(False, "n0"), (False, "n1")]
    (EBuiltIn (\[VInt n0, VInt n1] -> return $ VInt (n0 + n1))) Map.empty


bltnSub :: Value
bltnSub = VFn (TFn [TInt, TInt] TInt) [(False, "n0"), (False, "n1")]
    (EBuiltIn (\[VInt n0, VInt n1] -> return $ VInt (n0 - n1))) Map.empty


bltnLt :: Value
bltnLt = VFn (TFn [TInt, TInt] TBool) [(False, "n0"), (False, "n1")]
    (EBuiltIn (\[VInt n0, VInt n1] -> return $ VBool (n0 < n1))) Map.empty


bltnLe :: Value
bltnLe = VFn (TFn [TInt, TInt] TBool) [(False, "n0"), (False, "n1")]
    (EBuiltIn (\[VInt n0, VInt n1] -> return $ VBool (n0 <= n1))) Map.empty


bltnGt :: Value
bltnGt = VFn (TFn [TInt, TInt] TBool) [(False, "n0"), (False, "n1")]
    (EBuiltIn (\[VInt n0, VInt n1] -> return $ VBool (n0 > n1))) Map.empty


bltnGe :: Value
bltnGe = VFn (TFn [TInt, TInt] TBool) [(False, "n0"), (False, "n1")]
    (EBuiltIn (\[VInt n0, VInt n1] -> return $ VBool (n0 >= n1))) Map.empty


bltnEq :: Value
bltnEq = VFn (TFn [TInt, TInt] TBool) [(False, "n0"), (False, "n1")]
    (EBuiltIn (\[VInt n0, VInt n1] -> return $ VBool (n0 == n1))) Map.empty


bltnNe :: Value
bltnNe = VFn (TFn [TInt, TInt] TBool) [(False, "n0"), (False, "n1")]
    (EBuiltIn (\[VInt n0, VInt n1] -> return $ VBool (n0 /= n1))) Map.empty


bltnNot :: Value
bltnNot = VFn (TFn [TBool] TBool) [(False, "p")]
    (EBuiltIn (\[VBool p] -> return $ VBool (not p))) Map.empty


bltnAnd :: Value
bltnAnd = VFn (TFn [TBool, TBool] TBool) [(False, "p0"), (False, "p1")]
    (EBuiltIn (\[VBool p0, VBool p1] -> return $ VBool (p0 && p1))) Map.empty


bltnOr :: Value
bltnOr = VFn (TFn [TBool, TBool] TBool) [(False, "p0"), (False, "p1")]
    (EBuiltIn (\[VBool p0, VBool p1] -> return $ VBool (p0 || p1))) Map.empty



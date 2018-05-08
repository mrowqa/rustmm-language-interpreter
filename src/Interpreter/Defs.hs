{-# Options -Wall #-}

module Interpreter.Defs (
    Span(..),
    Exp(..),
    Type(..),
    Value(..),
    Loc,
    Store,
    Env,
    StaticCheckStore,
    StaticCheckEnv,
    Var,
    RuntimeExc,
    EvalState,
    Interpreter,
    StaticCheckExc,
    StaticCheckState,
    StaticChecker,
    mainFn,
    mainFnType,
) where
import Data.Map (Map)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader


data Span = String (Int, Int) (Int, Int) -- filename, pos begin, pos end
                                         -- pos is (line, column)
  deriving (Show)

data Exp =
    ELet Bool Var Exp Exp
  | EFnCall Exp [Exp]
  | EVar Bool Var
  | ELitVal Value
  | ETakeRef Bool Var
  | EBlock [Exp]
  | EAssign Bool Var Exp
  | EBuiltIn ([Value] -> Interpreter Value)
  | EIf Exp Exp Exp  -- can't be builtin without generics if we want to be expression based
  --deriving (Show)

data Type =
    TInt
  | TBool
  | TUnit
  | TFn [Type] Type
  | TRef Bool Type
  deriving (Eq,Show)

data Value =
    VInt Integer
  | VBool Bool
  | VUnit
  | VFn Type [(Bool, Var)] Exp Env
  | VRef Loc
  | VString String  -- only for stdout
  --deriving (Show)

type Loc = Integer
type StaticCheckStore = Map Loc Type
type StaticCheckEnv = Map Var (Loc, Bool)
type Store = Map Loc Value
type Env = Map Var Loc
type Var = String


type RuntimeExc = String
type EvalState = ExceptT RuntimeExc (State Store)
type Interpreter a = ReaderT Env EvalState a

type StaticCheckExc = String
type StaticCheckState = ExceptT StaticCheckExc (State StaticCheckStore)
type StaticChecker a = ReaderT StaticCheckEnv StaticCheckState a


mainFn :: Var
mainFn = "main"

mainFnType :: Type
mainFnType = TFn [] TUnit


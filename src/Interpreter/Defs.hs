{-# Options -Wall #-}

module Interpreter.Defs (
    Span(..),
    Exp(..),
    Type(..),
    Value(..),
    Loc,
    Store,
    TypeStore,
    Env,
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
--import qualified Data.Map as Map
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
  deriving (Show)

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
  | VFn Type [(Bool, Var)] Exp
  | VRef Loc
  deriving (Show)

type Loc = Integer
type Store = Map Loc Value
type TypeStore = Map Loc Type
type Env = Map Var (Loc, Bool)
type Var = String


type RuntimeExc = String
type EvalState = ExceptT RuntimeExc (State Store)
type Interpreter a = ReaderT Env EvalState a

type StaticCheckExc = String
type StaticCheckState = ExceptT StaticCheckExc (State TypeStore)
type StaticChecker a = ReaderT Env StaticCheckState a


mainFn :: Var
mainFn = "main"

mainFnType :: Type
mainFnType = TFn [] TUnit


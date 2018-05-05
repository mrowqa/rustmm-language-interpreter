{-# Options -Wall #-}

module Interpreter.Defs (
    Span(..),
    Exp(..),
    Type(..),
    Value(..),
    Loc,
    Store,
    Env,
    Var,
    RuntimeExc,
    EvalState,
    Interpreter,
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
  | EVar Var
  | ELitVal Value
  | ETakeRef Bool Var
  | EBlock [Exp]
  | EAssign Var Exp
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
  | VFn Type [Var] Exp
  | VRef Loc
  deriving (Show)

type Loc = Integer
type Store = Map Loc Value --Loc -> Value
type Env = Map Var (Loc, Bool) --Var -> (Loc, Bool) -- is mutable
type Var = String


type RuntimeExc = String
type EvalState = ExceptT RuntimeExc (State Store)
type Interpreter a = ReaderT Env EvalState a


--updateFn :: Eq a => (a->b) -> a -> b -> (a->b)
--updateFn ro x y x' | x == x' = y
--                   | otherwise = ro x


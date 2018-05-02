{-# Options -Wall #-}

module Interpreter.Defs (
    Span,
    Exp,
    Type,
    Value,
    Loc,
    Store,
    Env,
    Var
) where


data Span = String (Int, Int) (Int, Int) -- filename, pos begin, pos end
                                         -- pos is (line, column)

data Exp =
    ELet Bool Var Exp
  | EFnCall Exp [Exp]
  | EVar Var
  | ELiteral Value
  | ETakeRef Var
  | EBlock [Exp]
  | EAssign Var Exp

data Type =
    TI32
  | TBool
  | TUnit
  | TFn [Type] Type
  | TRef Type

data Value =
    VI32 Int
  | VBool Bool
  | VUnit
  | VFn Type [Var] Exp
  | VRef Loc

type Loc = Int
type Store = Loc -> Value
type Env = Var -> Loc
type Var = String


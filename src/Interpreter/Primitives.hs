{-# Options -Wall #-}

module Interpreter.Primitives (
--    bltnIf,
--    bltnWhile,
    emptyBlock,
--    bltnNeg,
--    bltnMul,
--    bltnDiv,
--    bltnAdd,
--    bltnSub
) where
import Interpreter.Defs

--bltnIf :: Exp
--bltnIf = EVar "_bltn_@if"
--
--bltnWhile :: Exp
--bltnWhile = EVar "_bltn_@while"

emptyBlock :: Exp
emptyBlock = EBlock [ELitVal VUnit]

--bltnNeg :: Exp
--bltnNeg = EVar "_bltn_@neg"
--
--bltnMul :: Exp
--bltnMul = EVar "_bltn_@mul"
--
--bltnDiv :: Exp
--bltnDiv = EVar "_bltn_@div"
--
--bltnAdd :: Exp
--bltnAdd = EVar "_bltn_@add"
--
--bltnSub :: Exp
--bltnSub = EVar "_bltn_@sub"


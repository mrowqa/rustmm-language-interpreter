{-# Options -Wall #-}

module Interpreter.Parser (
    parseStr,
    ParseResult
) where
import Interpreter.Defs

parseStr :: String -> ParseResult
parseStr _ = error "not implemented"

type ParseResult = ()


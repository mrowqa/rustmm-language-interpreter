module Main where
import System.Environment
import Control.Monad

--import Interpreter.Parser


main :: IO ()
main = do
    args <- getArgs
    (fname, src) <- (uncurry $ liftM2 (,)) $ (case args of
            [] -> (return "<stdin>", getContents)
            [fname] -> (return fname, readFile fname)
            _ -> error "Usage: <me> [filename]")
    runProgram fname src


runProgram :: String -> String -> IO ()
runProgram fname src = do
    --let _ = Parser.parseStr src in ()
    putStr $ "fname: " ++ fname ++ ", src: " ++ src


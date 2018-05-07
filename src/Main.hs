module Main where
import Control.Monad
import System.Environment
import System.IO

import Interpreter.Eval
import Interpreter.StaticChecker
import Interpreter.Parser
import Text.Megaparsec


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
    case parseCode fname src of
        Left err -> hPutStrLn stderr $ parseErrorPretty err
        Right prog -> do
            --putStrLn $ show prog
            case staticCheck prog of
                Left err -> hPutStrLn stderr err
                Right () -> do
                    let (out, err) = evalProgram prog
                    putStr out
                    case err of Just errMsg -> hPutStrLn stderr errMsg
                                Nothing -> return ()


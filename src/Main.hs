module Main where

import Quantum
import Gate
import Utils
import FunctionParser

import System.IO

circuitDJ :: QGate -> QRegister
circuitDJ gateU = qregister (replicate m 0) |+> qbit 1
                  |> gateH n
                  |> gateU
                  |> gateH m |+> gateI 1
    where n = dimensionsNum gateU
          m = n - 1

performDJ :: Int -> ([Int] -> Int) -> IO String
performDJ n f = do
    res <- measure . circuitDJ $ makeBinaryGate n f
    let bits = init . dec2bin (2 ^ n) $ res
    return $ if sum bits == 0 then "constant" else "balansed"

main :: IO ()
main = do
    putStrLn "Enter functions in format '<name> <args> = <expression>'"
    putStrLn "& - and, | - or, ^ - xor, ! - not"
    putStrLn "Examples:"
    putStrLn "f x = x"
    putStrLn "g y z = (y ^ z) & 1"
    putStrLn "Type 'quit' for exit"
    loop

loop :: IO ()
loop = do
    putStrLn ""
    putStr "#> "
    hFlush stdout
    input <- getLine
    let str = lstrip input
    if str == "quit"
        then return ()
        else case tryMakeFunction str of
            Left err -> do
                putStrLn "Error:"
                putStrLn err
                loop
            Right (name, n, f) -> do
                res <- performDJ n f
                let out = "function " ++ name ++ " is " ++ res
                putStrLn out
                loop

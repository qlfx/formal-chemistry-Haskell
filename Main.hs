module Main where

import System.IO
    ( hFlush
    , isEOF
    , stdout
    )

import Chemistry.Compiler
    ( compileReaction
    )

processLine :: String -> IO ()
processLine source =
    case compileReaction source of
        Left errorMessage ->
            putStrLn
                ("Parse error: " ++ errorMessage)

        Right result ->
            putStrLn result

repl :: IO ()
repl = do
    putStr "chem> "
    hFlush stdout

    endOfFile <- isEOF

    if endOfFile
        then
            pure ()

        else do
            source <- getLine

            if source == "exit"
                then
                    pure ()

                else do
                    processLine source
                    repl

main :: IO ()
main = do
    putStrLn "Chemical Compiler: Haskell Prototype"
    putStrLn "Supported metals: Zn, Fe, Cu"
    putStrLn "Supported acids: HCl, H2SO4"
    putStrLn "Example: Zn + HCl"
    putStrLn "Type exit to quit."
    putStrLn ""
    
    repl
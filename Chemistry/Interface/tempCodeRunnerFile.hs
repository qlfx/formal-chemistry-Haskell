module Main where
import Chemistry.Interface.Compiler(compileReaction)
import Chemistry.Definitions.Error(renderCompileError)

main :: IO()
main = do
    putStrLn "Enter a chemical reaction:"
    source <- getLine
    case compileReaction source of
        Left compileError -> 
            putStrLn (renderCompileError compileError)
        Right equation ->
            putStrLn equation
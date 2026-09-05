module Main where
import Chemistry.Workers.Counter
import Chemistry.Workers.Parser   
import Chemistry.Workers.Lexer
import Chemistry.Definitions.Error
import Chemistry.Definitions.AST
import Chemistry.Definitions.Token
import Data.Bifunctor (first)
import Language.Haskell.TH (implBidir)

lexerForCompiler :: String -> Either CompileError [Token]
lexerForCompiler = first LexicalFailure . lexer
parseForCompiler :: [Token] -> Either CompileError ReactionAST
parseForCompiler = first SyntaxFailure . parseReaction

getReactionAST :: String ->Either CompileError ReactionAST
getReactionAST string = do
    tokens <- lexerForCompiler string
    parseForCompiler tokens

main :: IO()
main = do
    let ast = getReactionAST "3HCl + 2Zn(OH)2"
    case ast of
        Left err -> print err
        Right rast -> print (countReactionAST rast)
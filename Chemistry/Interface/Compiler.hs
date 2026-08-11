module Chemistry.Interface.Compiler
    ( compileReaction
    ,analyzeSource
    ,evaluateSource
    ) where

import Chemistry.Workers.Parser
    ( parseReaction
    )

import Chemistry.Workers.Analyzer
    (
        analyzeReaction
    )
import Chemistry.Workers.Evaluator
    ( evaluateReaction
    )
import Chemistry.Workers.Render
    ( renderReactionResult
    )
import Language.Haskell.TH (implBidir)
import Data.Bifunctor (first)
import Chemistry.Definitions.Error
import Chemistry.Definitions.Token (Token)
import Chemistry.Definitions.AST (ReactionAST)
import Chemistry.Definitions.Types
import Chemistry.Workers.Lexer (lexer)

analyzeSource :: String -> Either CompileError ReactionInput
analyzeSource source = do
    tokens <- lexerForCompiler source
    reactionAST <- parseForCompiler tokens
    analyzeForCompiler reactionAST

evaluateSource :: String -> Either CompileError ReactionResult
evaluateSource source = fmap evaluateReaction (analyzeSource source)

compileReaction :: String -> Either CompileError String
compileReaction source = 
    fmap renderReactionResult (evaluateSource source)

lexerForCompiler :: String -> Either CompileError [Token]
lexerForCompiler = first LexicalFailure . lexer

parseForCompiler :: [Token] -> Either CompileError ReactionAST
parseForCompiler = first SyntaxFailure . parseReaction

analyzeForCompiler :: ReactionAST -> Either CompileError ReactionInput
analyzeForCompiler = first SemanticFailure . analyzeReaction
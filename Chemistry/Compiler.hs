module Chemistry.Compiler
    ( compileReaction
    ) where

import Chemistry.Parser
    ( parseInputAST
    )

import Chemistry.Analyzer
    (
        analyzeReaction
    )

import Chemistry.Evaluator
    ( evaluate
    )

import Chemistry.Render
    ( renderResult
    )

compileReaction :: String -> Either String String
compileReaction source = do
    syntaxTree <- parseInputAST source

    semanticInput <- analyzeReaction syntaxTree

    let result = evaluate semanticInput

    Right (renderResult result)
    
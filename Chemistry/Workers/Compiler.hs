module Chemistry.Workers.Compiler
    ( compileReaction
    ) where

import Chemistry.Workers.Parser
    ( parseReaction
    )

import Chemistry.Workers.Analyzer
    (
        analyzeReaction
    )

import Chemistry.Workers.Evaluator
    ( evaluate
    )

import Chemistry.Workers.Render
    ( renderReactionResult
    )

compileReaction :: String -> Either String String
compileReaction source = do
    syntaxTree <- parseReaction source

    semanticInput <- analyzeReaction syntaxTree

    let result = evaluate semanticInput

    Right (renderResult result)
    
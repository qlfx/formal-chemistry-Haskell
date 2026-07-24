module Chemistry.Compiler
    ( compileReaction
    ) where

import Chemistry.Parser
    ( parseInput
    )

import Chemistry.Evaluator
    ( evaluate
    )

import Chemistry.Render
    ( renderResult
    )

compileReaction :: String -> Either String String
compileReaction source =
    case parseInput source of
        Left errorMessage ->
            Left errorMessage

        Right reactionInput ->
            Right
                (renderResult
                    (evaluate reactionInput))